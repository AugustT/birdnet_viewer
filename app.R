#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(tuneR)

source('functions.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bird data viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput(label = 'Select files',
                    inputId = 'files',
                    multiple = TRUE,
                    accept = '.csv'),
          sliderInput(label = 'Threshold',
                      inputId = 'thres',
                      min = 0, 
                      max = 1, 
                      step = 0.01, 
                      value = 0.2),
          selectInput('time_interval', 
                      label = 'Aggregate data by...',
                      choices = c('Minute','Hour','Day','Week','Month'),
                      selected = 'Day',
                      multiple = FALSE),
          numericInput('id', label = 'ID to play', step = 1, value = NULL),
          conditionalPanel(condition = 'input.id > 0',
                           actionButton(inputId = 'play',
                                        label = 'Play'),
                           downloadButton(outputId = 'download',
                                          label = "Download"))
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("plot2"),
          fluidRow(column(width = 6, plotlyOutput("call_activity")),
                   column(width = 6, plotlyOutput("sp_rich_plot"))),
          fluidRow(column(width = 6, plotlyOutput("dawn_plot")),
                   column(width = 6, plotlyOutput("dusk_plot"))),
          plotlyOutput("plot1")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # values <- reactiveValues()
  # values$wav <- NULL

  ##### DATA #####
  
  # Format all the data together
  dat_all <- reactive({
    
    if(!is.null(input$files)){
      
      files <- input$files
      dat <- lapply(files$datapath, FUN = read.csv.row)
      dat_all <- do.call(rbind, dat)
      colnam <- names(read.csv(files$datapath[1], header = TRUE)[1,])
      colnames(dat_all) <- colnam
      dat_all$time <- as.POSIXct(file_to_time(dat_all$filepath))
      dat_all$time_of_day <- as.POSIXct(format(dat_all$time, "%H:%M"), format = '%H:%M')
      dat_all$ID <- 1:nrow(dat_all) 
      dat_all <- dat_all[dat_all$confidence >= input$thres, ]
      dat_all
      
    }
    
  })
  
  # Get all the species richness
  species_richness_all <- reactive({
    
    dat_all() %>% 
      group_by(filepath) %>%
      summarise(species_richness = n_distinct(common_name))
    
  })
  
  # Group species richness by time period
  species_richness_by <- reactive({
    
    dat_all() %>% 
      summarise_by_time(.date_var = time,
                        .by = tolower(input$time_interval),
                        value = n_distinct(common_name))
    
  })
  
  # Get all call activity
  call_activity_all <- reactive({
    
    dat_all() %>%
    group_by(filepath) %>%
    summarise(call_activity = n())
    
  })
  
  # Group call activity by time period
  call_activity_by <- reactive({
    
    dat_all() %>% 
    summarise_by_time(.date_var = time,
                      .by = tolower(input$time_interval),
                      value = n())
  
  })
  
  # Combined summary data
  summary_data <- reactive({
    
    file_time <- unique(dat_all()[, c("filepath", "time_of_day")])
    
    full_join(file_time, call_activity_all(), by = 'filepath') %>%
      full_join(species_richness_all(), by = 'filepath')
    
  })
  
  # Dawn and dusk data
  dawn <- reactive({
    
    summary_data()[as.numeric(format(summary_data()$time_of_day,"%H")) < 12, ] 
    
  })
  dusk <- reactive({
    
    summary_data()[as.numeric(format(summary_data()$time_of_day,"%H")) >= 12, ] 
    
  })
  
  ##### PLOTS#####
  
  # Plot of confidence and species over time
  output$plot1 <- renderPlotly({
    if(!is.null(dat_all())){
      plot1 <- ggplot(dat_all(),
                     aes(y = confidence,
                         x = time, 
                         col = common_name,
                         text = ID)) +
        geom_point() +
        ylim(0, 1) +
        ylab('Confidence') +
        xlab('Time') +
        theme(legend.position = "none")
      ggplotly(plot1)
    }
  })
  
  # Plot of overall species call activity
  output$plot2 <- renderPlotly({
    if(!is.null(dat_all())){
      dat_all <- dat_all()
      sp_rich <- as.data.frame(table(dat_all$common_name), 
                               stringsAsFactors = FALSE)
      colnames(sp_rich) <- c('Species', 'Count')
      
      plot2 <- ggplot(sp_rich, aes(y = Count, 
                                   x = reorder(Species, -Count), 
                                   fill = Species)) +
                  geom_bar(stat = "identity") +
                  xlab("") +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.05),
                        legend.position = "none")
      ggplotly(plot2)
    }
  })
  
  # Plot species richness
  output$sp_rich_plot <- renderPlotly({
    
    if(!is.null(dat_all())){
    
      plt <- ggplot(species_richness_by(), aes(x = time, y = value)) +
        geom_bar(stat = "identity") +
        xlab(str_to_title(input$time_interval)) +
        ylab('Species richness')
      ggplotly(plt)
      
    }
    
  })
  
  # Plot call activity
  output$call_activity <- renderPlotly({
    
    if(!is.null(dat_all())){
      
      plt <- ggplot(call_activity_by(), aes(x = time, y = value)) +
        geom_bar(stat = "identity") +
        xlab(str_to_title(input$time_interval)) +
        ylab('Call activity')
      ggplotly(plt)
      
    }
    
  })
  
  # dawn plot
  output$dawn_plot <- renderPlotly({
    if(!is.null(dat_all())){
      dawn_plot <- ggplot(dawn(), aes(x = time_of_day, y = call_activity)) +
        geom_point() +
        geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
        ggtitle('Aggregate dawn activity trend') +
        xlab('Time') +
        ylab('Number of calls')
      ggplotly(dawn_plot)
    }
  })
  
  # dusk plot
  output$dusk_plot <- renderPlotly({
    if(!is.null(dat_all())){
      dusk_plot <- ggplot(dusk(), aes(x = time_of_day, y = call_activity)) +
        geom_point() +
        geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
        ggtitle('Aggregate dusk activity trend') +
        xlab('Time') +
        ylab('Number of calls')
      ggplotly(dusk_plot)    
    }
  })

  
  # Return wav file for a given ID
  wav <- reactive({
    if(!is.null(input$id)){
      
      dat_all <- dat_all()
      clip_audio(df = dat_all,
                 ID = input$id)  
      
    } else {
      
      NULL
      
    }
  })
    
  # Play a wav file
  observeEvent(input$play, {
      play(object = wav(), )
    })
  
  # Download a wav file
  output$download <- downloadHandler(
    filename = function() {
      paste(gsub(' ', '_', dat_all()[dat_all()$ID == input$id,'common_name']),
            dat_all()[dat_all()$ID == input$id, 'start'],
            basename(dat_all()[dat_all()$ID == input$id, 1]),
            sep = '_')
    },
    content = function(file) {
      writeWave(wav(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
