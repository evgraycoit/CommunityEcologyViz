library(shiny)
library(bslib)
library(tidyverse)
library(vegan)
library(PNWColors)
library(plotly)
library(dendextend)
library(ggdendro)

# palette for abundance plot
pal15 <- c( "#004949", "#009292", "#b6dbff","#490092","#b66dff","#ffb6db", "#ff6db6", "#006ddb","#6db6ff","#ffff6d", "#920000", "#db6d00",  "#924900","#24ff24", "gray55")

# palettes for clustering depending on # of clusters
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

# demo rds object for nmds and distance matrix to save startup time
demo_bray <- readRDS("demo_bray.rds")
demo_nmds <- readRDS("demo_nmds.rds")

# Use a navbar as the bottom page, allows header
# Use sidebar layout for main page for collapsible sidebar
ui <- page_navbar(
  # minty fresh
  theme = bs_theme(preset = "minty"),
  # false so cards are full size don't overlap
  fillable = FALSE,
  title = "Visualize your communities!",
  window_title = "CommunityEcologyViz",
  # intro card, use accordion so it can be collapsed
  header = card_body(
    fillable = TRUE,
    padding = 20, 
    accordion(
      accordion_panel(
        "Welcome!",
        p("Welcome to the Community Ecology visualizer."),
        p("This app is designed to help explore the beta-diversity of community samples using hierarchical clustering and NMDS ordination. The demo data presented here is a set of 286 fungal community samples taken across Washington State, obtained through the compiled GlobalFungi database (https://globalfungi.com/). If you would like to upload your own data, unclick the \"Use demo data\" button. This app will accept .csv and .rds files, but make sure they follow the format shown in the example! It will take a moment for the plots to process after uploading, so be patient..."),
        p("All plots below can be expanded using the button in the bottom right of each card.")
        )
      )
    ),
  # main page represented by singular nav_panel
  nav_panel(
    title = "",
    # layout sidebar for data entry sidebar
    layout_sidebar(
      sidebar = sidebar(
        id = "DataSidebar",
        title = "Data Input",
        position = "left",
        # when selected, uses demo data for the app
        checkboxInput("demo",
                      "Use demo data",
                      value = T),
        # only displays when the checkbox is unclicked
        conditionalPanel(
          # references the checkbox 
          condition = "input.demo == false",
          # two file inputs, one for count matrix, one for metadata
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
          # action button that opens window pop up
          p("What should my data look like?"),
          actionButton("format",
                       "Example",
                       style = "simple",
                       size = "sm"
                       )
          )
        ),
      # use column wrap so nmds and tree are side-by-side
      layout_column_wrap(
        # even widths
        width = 1/2,
        # use a nav card so description can be toggled
        navset_card_pill(
          full_screen = TRUE,
          height = 500,
          # main panel with tree plot
          nav_panel(
            card_title("Hierarchical Clustering"),
            # add layout sidebar for clustering options
            layout_sidebar(
              sidebar = sidebar(
                # allows user to input distance metric
                selectInput("distance",
                            label = "What distance metric?", 
                            choices = c("bray",
                                        "euclidean",
                                        "aitchison",
                                        "jaccard"),
                            selected = "bray"
                ),
                # allows user to input clustering method for hclust()
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
                # slider allows user to select cluster cutoff up to 9
                sliderInput("clusters",
                            label = "How many clusters?",
                            min = 1,
                            max = 9, 
                            value = 5,
                            round = T,
                            ticks = F
                            )
                ),
                # tree diagram of clusters
                # uses plotly so can be zoomed
                plotlyOutput('tree_plot', 
                           width = "100%",
                           height = "500px"
                           )
              )
            ),
          # description panel for information
          nav_panel(
            "What's this?",
            p("Hierarchical clustering groups your communities 
              based on species/taxa composition, in this case using Bray-Curtis 
              dissimilarity. The method controlling clustering can be
              set using the dropdown widget in the side bar, Ward.D2 is
              most common for community ecology. Setting the number of
              clusters will change where in the dendrogram the clusters are cut,
              and will affect the other plots in this app!"),
            p(" "),
            p("Note that changing the distance metric will require the distance measures to recalculate
              and will take some time, be patient. Aitchison distance should only be selected
              if data is compositional and should contain no zeroes.")
            )
          ),
        # card because only needs one panel
        card(
          full_screen = TRUE,
          min_height = 500,
          card_title("NMDS Ordination"),
          # ordination plot
          plotlyOutput('nmds_plot'),
          )
        ),
      # add navset for description of summary plots
      navset_card_pill(
        full_screen = TRUE,
        height = 500,
        nav_panel(
          card_title("Summary Plots"),
          # sidebar for selecting cluster and variable of interest
          layout_sidebar(
            sidebar = sidebar(
              # creates an input list of clusters based on # of clusters selected
              uiOutput("focus"),
              # allows user to select metadata variable for summary plot
              uiOutput("metadata")
              ),
            # use two columns so plots are side by side
            layout_column_wrap(
              # even width
              width = 1/2,
              # plot the variable distribution
              plotOutput('meta_plot', 
                         width = "100%",
                         height = "500px"
                         ),
              # plot relative abundance of top species
              plotOutput('abundance_plot', 
                         width = "100%",
                         height = "500px"
                         )
              )
            )
          ),
        # second panel gives description of the summary plots
        nav_panel(
          "What do these show?",
          p("These summary plots should help you understand the similarities between your communities."),
          p(" "),
          p("The plot on the left will summarize the metadata you provide about your community 
            samples. If the data is continuous, a density plot will be drawn. If the data is 
            categorical, a histogram will be drawn."),
          p(" "),
          p("The plot on the right will show you the relative abundance of the top 15 species within the
            cluster selected. These are selected by total abundance across the cluster, which may be misleading if
            some species are only dominant in a small subset of observations (i.e. take it with a grain of salt!).")
          )
        )
      )
    )
  )

# Define server logic required to draw a histogram
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
  # follow above comments
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
  # uses the image uploaded for demo 
  observeEvent(input$format, {
    showModal(
      modalDialog(
        title = "What should your data look like?",
        # plots my image showing data format
        plotOutput("demo_pic"), 
        size = "l",
        # closes if clicked outside modal
        easyClose = T
      )
    )
  })
  
  # renders image in the popup modal window
  output$demo_pic <- renderImage({
    # calls path for demo pic
    list(src = demo_pic,
         contentType = "image/png",
         width = 600,
         height = 480)}, 
    deleteFile = F
  )
  
  # bray-curtis distance matrix
  bray <- reactive(
    # uses demo rds file if demo box remains clicked and bray is still the selected distance metric
    if(input$demo == T & input$distance == "bray") {
      demo_bray
    # otherwise calculates distance for user uploaded count matrix
    } else {
      vegdist(counts(), method = input$distance, na.rm = T)
    }
  )
  
  # perform ordination
  nmds <- reactive(
    # uses demo rds file if demo box remains clicked and bray is still the selected distance metric
    if(input$demo == T & input$distance == "bray") {
      demo_nmds
    # otherwise processes ordination for user data
    } else {
      metaMDS(counts(), distance = input$distance, k = 2)
    }
  )
  
  
  # performs hclust based on input
  # will try and eventually make this also accept user input data
  clust <- reactive(hclust(bray(), method = as.character(clust_meth())), )
  # create dendrogram
  tree <- reactive(ggdendrogram(clust(), rotate = F))
  # cut tree based on user input
  cuts <- reactive(cutree(clust(), k = input$clusters))
  
  # reads order of samples in hierarchical cluster cut
  sample_order <- reactive(match(names(cuts()),table = rownames(sample_dat())))
  # rearranges sample data to match cut sample order
  sample_dat2 <- reactive(sample_dat()[sample_order(),] |> 
                            mutate(cluster = as.factor(cuts())))
  
  # first adjusts rows in sample dataframe to match cluster object order
  # then adds cluster to the sample dataframe
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
      # selects all columns except the last which is cluster 
      pivot_longer(cols = c(1:(ncol(counts2())-1)),
                   values_to = "counts",
                   names_to = "species") |>
      # filters for just cluster of interest
      filter(cluster == input$focus) |> 
      # calculates total abundance
      mutate(total = sum(counts)) |> 
      group_by(species) |> 
      # calculates relative species abundance in cluster
      summarise(spp = sum(counts),
                total = mean(total),
                rel_abun = spp/total*100) |> 
      ungroup() |> 
      # selects top 15 most abundant species
      slice_max(order_by = rel_abun, n = 15, with_ties = F) |> 
      arrange(-rel_abun)
    
    # change species order so most abundant plotted on top
    df$species <- factor(df$species, levels = unique(df$species))
    
    # returns arranged df
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
  output$tree_plot <- renderPlotly({
    ggplotly(
      tree() +
        # adds line that indicates the height clusters are cut at
      geom_hline(aes(yintercept = rev(clust()$height)[clust_i()] + last(clust()$height)/500), color = 'red') +
      theme(text = element_text(size = 8))
    )
  })
  
  # creates summary plot for user selected metadata variable
  output$meta_plot <- renderPlot({
    # column plot if factor 
    if (class(meta_focus()) == 'factor') {
      ggplot(sum_meta()) +
        # .data prefix allows indexing focus dataframe based on user variable input
        geom_col(aes(x = .data[[input$metadata]], y = n)) +
        theme_classic() +
        labs(title = input$metadata,
             y = element_blank()) +
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 270))
      # density plot if numeric or otherwise (once again, hopefully numeric...)
    } else {
      ggplot(sum_meta()) +
        # .data prefix allows indexing focus dataframe based on user variable input
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

shinyApp(ui = ui, server = server)
