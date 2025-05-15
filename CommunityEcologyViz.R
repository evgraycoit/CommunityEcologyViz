library(shiny)
library(tidyverse)
library(vegan)
library(PNWColors)
library(plotly)
library(dendextend)

# palettes for up to 9 clusters
pal9 <- pnw_palette("Shuksan2", n = 9)
pal1 <- pal9[1]
pal2 <- pal9[c(1,9)]
pal3 <- pal9[c(1,5,9)]
pal4 <- pal9[c(1,4,5,9)]
pal5 <- pal9[c(1,4,5,6,9)]
pal6 <- pal9[c(1,3,4,5,6,9)]
pal7 <- pal9[c(1,3,4,5,6,7,9)]
pal8 <- pal9[c(1,2,3,4,5,6,7,9)]

# saving palettes in list for indexing based on input
pal_list <- list(pal1, pal2, pal3, pal4, pal5, pal6, pal7, pal8, pal9)

# fungi metadata
sample_df <- read.csv("washington_fungi_metadata.csv", row.names = 1)
# fungi abundance counts
fungi <- read.csv("washington_fungi_counts.csv", row.names = 1)

# bray-curtis distance matrix
bray <- vegdist(fungi[,-1], method = "bray")

ui <- fluidPage(
        titlePanel("Community Ecology"),
        fluidRow(
          column(width = 10,
                 p("Welcome to the Community Ecology visualizer!"),
                 p("This app is designed to help explore the beta-diversity of community samples using hierarchical clustering and NMDS ordination. The demo data presented here is a set of 306 fungal community samples taken across Washington State, obtained through the compiled GlobalFungi database (https://globalfungi.com/)."),
                 p("Please give the app a few seconds to process the first ordination.")
                 )
        ),
        
        fluidRow(
          # interactive ordination plot using ggplotly
          column(width = 8,
                 plotlyOutput('nmds_plot')
                 ),
          column(width = 4,
                 # lets user select clustering method
                 selectInput("method",
                             label = "What clustering method?", 
                             choices = c("ward.D",
                                         "ward.D2",
                                         "single",
                                         "complete",
                                         "average",
                                         "mcquitty",
                                         "median",
                                         "centroid"), 
                             selected = "ward.D2"
                             ),
                 # lets user select number of clusters
                 sliderInput("clusters",
                             label = "How many clusters?",
                             min = 1,
                             max = 9, 
                             value = 5,
                             round = T,
                             ticks = F
                            ),
                 # creates an input list of clusters based on # of clusters selected
                 uiOutput("focus"),
                 # allows user to select metadata variable for summary plot
                 selectInput("metadata",
                             label = "What do you want to know about this cluster?", 
                             choices = c("Biome",
                                         "Elevation",
                                         "pH",
                                         "Month_sampled"), 
                             selected = "Biome"
                             )
                 )
        ),
        # plots dendrogram
        fluidRow(
          column(width = 6,
                 plotOutput('tree_plot', 
                            width = "100%",
                            height = "500px")
          ),
          # plots metadata summary plot
          column(width = 6,
                 plotOutput('meta_plot', 
                            width = "100%",
                            height = "500px")
          ),
        )
)
    
server <- function(input, output) {
  # perform ordination
  # eventually hoping to let this accept user data
  nmds <- metaMDS(fungi[,-1], distance = "bray", k = 2)
  
  # saves number of clusters desired
  clust_i <- reactive(as.numeric(input$clusters))
  # saves clustering method
  clust_meth <- reactive(input$method)
  
  # creates input$focus when # of clusters is selected
  output$focus <- renderUI({
    selectInput("focus", 
                "Which cluster would you like to focus on?",
                choices = c(1:clust_i()),
                selected = 1
    )
  })
  
  # saves a plotting data frame
  # would like this to eventually adapt to user desired metadata but this is where it is
  nmds_df <- reactive(data.frame(Sample = sample_df$Sample.ID,
                                 Biome = as.factor(sample_df$Biome),
                                 Elevation = as.numeric(sample_df$Elevation), 
                                 pH = as.numeric(sample_df$pH),
                                 Month_sampled = factor(sample_df$Month.of.Sampling,
                                                        levels = c("January",
                                                                   "February",
                                                                   "March",
                                                                   "April",
                                                                   "May",
                                                                   "June",
                                                                   "July",
                                                                   "August",
                                                                   "September",
                                                                   "October",
                                                                   "November",
                                                                   "December"
                                                                   )
                                                        ),
                                 MDS1 = nmds$points[,1],
                                 MDS2 = nmds$points[,2],
                                 cluster = as.factor(cuts())
                                )
                      )
  
  # saves user selected metadata summary variable from dataframe
  meta_focus <- reactive(nmds_df()[,as.character(input$metadata)])
  
  # saves a summary dataframe for plotting user selected variable                       
  sum_meta <- reactive(
    # if a factor, counts occurrences of each level
    if (class(meta_focus()) == 'factor') {
      nmds_df() |> 
        filter(cluster == input$focus) |> 
        group_by(pick(input$metadata)) |> 
        summarise(n = n())
    # if numeric or otherwise (hopefully numeric...)
    # just saves a filtered dataframe
    } else {
      nmds_df() |> 
        filter(cluster == input$focus) |> 
        select(input$metadata) |> 
        filter(!is.na(input$focus))
    }
    )
  
  # performs hclust based on input
  # will try and eventually make this also accept user input data
  clust <- reactive(hclust(bray, method = as.character(clust_meth())))
  # create dendrogram
  tree <- reactive(as.ggdend(as.dendrogram(clust())))
  # cut tree based on user input
  cuts <- reactive(cutree(clust(), k = input$clusters))
  
  # pick the appropriately sized palette
  palette <- reactive(pal_list[[input$clusters]])
  
  # interactive ordination plot
  output$nmds_plot <- renderPlotly({
    ggplotly(
      ggplot(nmds_df()) +
        geom_point(aes(x = MDS1, y = MDS2, color = cluster), size = 3) +
        stat_ellipse(aes(x = MDS1, y = MDS2, color = cluster)) +
        scale_color_manual(values = palette()) +
        theme(panel.background = element_rect(fill = "gray90"),
              panel.grid = element_line(color = "gray90")) 
    )
  })
  
  # plots dendrogram with line indicating where cluster cut is
  output$tree_plot <- renderPlot({
    ggplot(tree(), 
           horiz = F, 
           offset_labels = -last(clust()$height)/20
          ) +
      geom_hline(aes(yintercept = rev(clust()$height)[clust_i()] + last(clust()$height)/500), color = 'red') +
      theme(text = element_text(size = 8))
  })
  
  # creates summary plot for user selected metadata variable
  output$meta_plot <- renderPlot({
    # column plot if factor 
    if (class(meta_focus()) == 'factor') {
      ggplot(sum_meta()) +
        geom_col(aes_string(x = input$metadata, y = 'n')) +
        theme_classic() +
        labs(title = input$metadata,
             y = element_blank()) +
        theme(text = element_text(size = 20))
    # density plot if numeric or otherwise (once again, hopefully numeric...)
    } else {
      ggplot(sum_meta()) +
        geom_density(aes_string(x = input$metadata)) +
        theme_classic() +
        labs(title = input$metadata,
             y = element_blank()) +
        theme(text = element_text(size = 20))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
