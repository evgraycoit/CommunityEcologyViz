library(shiny)
library(tidyverse)
library(vegan)
library(PNWColors)
library(plotly)
library(dendextend)

# palettes for up to 9 clusters
pal15 <- c( "#004949", "#009292", "#b6dbff","#490092","#b66dff","#ffb6db", "#ff6db6", "#006ddb","#6db6ff","#ffff6d", "#920000", "#db6d00",  "#924900","#24ff24", "gray55")
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
sample_df <- read.csv("washington_fungi_metadata.csv", row.names = 1, stringsAsFactors = T)
# fungi abundance counts
fungi <- read.csv("washington_fungi_counts.csv", row.names = 1, stringsAsFactors = T)

# picture demonstrating formatting for input data
demo_pic <- file.path("demo_pics.png") 

ui <- fluidPage(
        titlePanel(title = "Visualize your communities!", windowTitle = "CommunityEcologyViz"),
        fluidRow(column(width = 10,
                        p("Welcome to the Community Ecology visualizer."),
                        p("This app is designed to help explore the beta-diversity of community samples using hierarchical clustering and NMDS ordination. The demo data presented here is a set of 306 fungal community samples taken across Washington State, obtained through the compiled GlobalFungi database (https://globalfungi.com/). If you would like to upload your own data, unclick the \"Use demo data\" button. This app will accept .csv and .rds files, but make sure they follow the format shown in the example!"),
                        p("Please give the app a few seconds to process the first ordination.")
                   )
        ),
        # added a sidebar for data input
        sidebarLayout(
          fluid = T,   
          sidebarPanel(
            width = 2,
            # was hoping this would make it collapsible
            # doesn't.
            open = "closed",
            fixedRow(
              column(width = 12,
                     checkboxInput("demo", 
                                   "Use demo data", 
                                   value = T),
                     # only displays when the checkbox is unclicked
                     conditionalPanel(
                       condition = "input.demo == false",
                       fileInput("counts", 
                                 label = "Upload Count Matrix",
                                 multiple = F,
                                 accept = c(".csv",".rds"),
                                 width = "100%"
                                 ),
                       fileInput("sample_data", 
                                 label = "Upload Sample Data",
                                 multiple = F,
                                 accept = c(".csv",".rds"),
                                 width = "100%"
                                ),
                       # action button opens window pop up 
                       p("What should my data look like?"),
                       actionButton("format", 
                                    "Example",
                                    style = "simple",
                                    size = "sm"   
                                    )
                       )
                     )
              ), 
            conditionalPanel(
              condition = "input.demo == false",
              p(" "),
              p(" "),
              p(" "),
              fixedRow(
                column(width = 12,
                       p("Give the plots a second to process after uploading data...")
                       )
              )
              )
            ),
          mainPanel(
            fluidRow(
              column(width = 6,
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
                                 )
                     ),
              column(width = 6,
                     # lets user select number of clusters
                     sliderInput("clusters",
                                 label = "How many clusters?",
                                 min = 1,
                                 max = 9, 
                                 value = 5,
                                 round = T,
                                 ticks = F
                                )
                     )
              ),
            fluidRow(
              # interactive ordination plot using ggplotly
              column(width = 6,
                     plotlyOutput('nmds_plot')
              ),
              # tree diagram of clusters
              column(width = 6,
                     plotOutput('tree_plot', 
                                width = "100%",
                                height = "500px")
                     )
              ),
            fluidRow(
              column(width = 6,
                     # creates an input list of clusters based on # of clusters selected
                     uiOutput("focus"),
                     # allows user to select metadata variable for summary plot
                     uiOutput("metadata")
              )
            ),
            fluidRow(
              # plots metadata summary plot
              column(width = 6,
                     plotOutput('meta_plot', 
                                width = "100%",
                                height = "500px")
                     ),
              # plots relative abundance of top species
              column(width = 6,
                     plotOutput('abundance_plot', 
                                width = "100%",
                                height = "500px")
                     ),
              )
            )
          )
        )
    
server <- function(input, output) {
  
  # reads in count matrix
  counts <- reactive(
    # uses demo data if checkbox selected
    if(input$demo == TRUE) {
      fungi
    # checks if input data is .csv and reads
    } else if (input$counts$type == "text/csv") {
      read.csv(input$counts$datapath, row.names = 1, stringsAsFactors = T)
    #if not .csv it's .rds so reads that
    } else {
      readRDS(input$counts$datapath)
    }, 
    label = "countreact"
  )
  
  # same as above but for sample metadata 
  sample_dat <- reactive(
    if(input$demo == TRUE) {
      sample_df
    } else if (input$counts$type == "text/csv") {
      read.csv(input$sample_data$datapath, row.names = 1, stringsAsFactors = T)
    } else {
      readRDS(input$sample_data$datapath)
    },
    label = "samplereact"
  )
  
  # observes example button, opens modal when selected
  # don't understand the modal 
  observeEvent(input$format, {
    showModal(
      modalDialog(
        title = "What should your data look like?",
        # plots my image showing data format
        plotOutput("demo_pic"), 
        size = "l",
        easyClose = T
      )
    )
  })
  
  # renders image in the popup modal window
  output$demo_pic <- renderImage({
    # calls path for demo pic
    # the sizing is frustrating, can't figure out modal layout
    list(src = demo_pic,
         contentType = "image/png",
         width = 600,
         height = 480)}, 
    deleteFile = F
  )
  
  # bray-curtis distance matrix
  bray <- reactive(vegdist(counts(), method = "bray", na.rm = T))
  
  # perform ordination
  # eventually hoping to let this accept user data
  nmds <- reactive(metaMDS(counts(), distance = "bray", k = 2))
  
  
  # performs hclust based on input
  # will try and eventually make this also accept user input data
  clust <- reactive(hclust(bray(), method = as.character(clust_meth())), )
  # create dendrogram
  tree <- reactive(as.ggdend(as.dendrogram(clust())))
  # cut tree based on user input
  cuts <- reactive(cutree(clust(), k = input$clusters))
  
  # reads order of samples in hierarchical cluster cut
  sample_order <- reactive(match(names(cuts()),table = rownames(sample_dat())))
  # rearranges sample data to match cut sample order
  sample_dat2 <- reactive(sample_dat()[sample_order(),] |> 
                            mutate(cluster = as.factor(cuts())))
  
  counts2 <- reactive(counts()[sample_order(),] |> 
                          mutate(cluster = as.factor(cuts())))

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
  
  # creates a list for the metadata dropdown menu of sample variables
  # uses column names from sample dataframe
  metadata_choices <- reactive(colnames(sample_dat()))

  # creates input$focus when # of clusters is selected
  output$metadata <- renderUI({
    selectInput("metadata",
                label = "What do you want to know about this cluster?", 
                choices = metadata_choices()
    )
  })
  
  # saves a plotting data frame
  # would like this to eventually adapt to user desired metadata but this is where it is
  nmds_df <- reactive(data.frame(
                                 MDS1 = nmds()$points[,1],
                                 MDS2 = nmds()$points[,2],
                                 cluster = as.factor(cuts())
                                )
                      )
  
  # saves user selected metadata summary variable from dataframe
  meta_focus <- reactive(sample_dat2()[,input$metadata])
  
  # saves a summary dataframe for plotting user selected variable                       
  sum_meta <- reactive(
    # if a factor, counts occurrences of each level
    if (class(meta_focus()) == 'factor') {
      sample_dat2() |> 
        filter(cluster == input$focus) |> 
        group_by(pick(input$metadata)) |> 
        summarise(n = n())
    # if numeric or otherwise (hopefully numeric...)
    # just saves a filtered dataframe
    } else {
      sample_dat2() |> 
        filter(cluster == input$focus) |> 
        select(input$metadata) |> 
        filter(!is.na(input$focus))
    }
    )
  
  # saves a summary of top 15 most abundant species and their rel. percentages
  sum_counts <- reactive({
    df <- counts2() |> 
      pivot_longer(cols = c(1:(ncol(counts2())-1)),
                   values_to = "counts",
                   names_to = "species") |> 
      filter(cluster == input$focus) |> 
      mutate(total = sum(counts)) |> 
      group_by(species) |> 
      summarise(spp = sum(counts),
                total = mean(total),
                rel_abun = spp/total*100) |> 
      ungroup() |> 
      slice_max(order_by = rel_abun, n = 15, with_ties = F) |> 
      arrange(-rel_abun)
    
    # change species order so most abundant plotted on top
    df$species <- factor(df$species, levels = unique(df$species))
    
    return(df)
  })
  
  
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
        geom_col(aes(x = .data[[input$metadata]], y = n)) +
        theme_classic() +
        labs(title = input$metadata,
             y = element_blank()) +
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 270))
    # density plot if numeric or otherwise (once again, hopefully numeric...)
    } else {
      ggplot(sum_meta()) +
        geom_density(aes(x = .data[[input$metadata]])) +
        theme_classic() +
        labs(title = input$metadata,
             y = element_blank()) +
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 270))
    }
  })
  
  output$abundance_plot <- renderPlot({
    # column plot if factor 
    ggplot(sum_counts()) +
      geom_col(aes(x = "", y = rel_abun, fill = species), position = "stack") +
      scale_fill_manual(values = pal15, name = "Species") +
      labs(y = "Relative Abundance of Cluster (%)",
           x = element_blank())  +
      theme_classic() +
      theme(text = element_text(size = 20),
            axis.ticks = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
