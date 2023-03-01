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
library(dplyr)
library(timetk)
library(stringr)
library(data.table)
library(shinyjs)
library(shinythemes)

source('functions.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # add JS
    useShinyjs(),

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
          selectInput(inputId = 'species',
                      label =  'Select Species',
                      choices = c('All species'),
                      selected = 'All species',
                      multiple = FALSE),
          selectInput('time_interval',
                      label = 'Aggregate data by...',
                      choices = c('Minute','Hour','Day','Week','Month'),
                      selected = 'Day',
                      multiple = FALSE),
          numericInput('id', label = 'ID to play', step = 1, value = NULL),
          conditionalPanel(condition = 'input.id > 0',
                           actionButton(inputId = 'play',
                                        label = 'Play'),
                           downloadButton(outputId = 'download_indiv',
                                          label = "Download clip")),
          downloadButton(outputId = 'download_best',
                         label = "Download best calls")
          
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
server <- function(input, output, session) {
  
  # values <- reactiveValues()
  # values$wav <- NULL

  ##### DATA #####
  
  # Format all the data together
  dat_all_raw <- reactive({
    
    if(!is.null(input$files)){
      
      withProgress(message = 'Loading data', value = 0, {
        files <- input$files
        percentage <- 0
        dat <- lapply(files$datapath, FUN = function(x){
          percentage <<- percentage + 1/length(files$datapath)*100
          incProgress(1/length(files$datapath), detail = paste0("Progress: ",round(percentage), '%'))
          read.csv.row(x) 
        })
        dat_all <- do.call(rbind, dat)
        incProgress(0.1)
        colnam <- names(read.csv(files$datapath[1], header = TRUE)[1,])
        colnames(dat_all) <- colnam
        dat_all$time <- as.POSIXct(file_to_time(dat_all$filepath))
        dat_all$time_of_day <- as.POSIXct(format(dat_all$time, "%H:%M"), format = '%H:%M')
        dat_all$ID <- 1:nrow(dat_all) 
        incProgress(0.1)
      })
      
      return(dat_all)
      
    }
  })

  dat_all <- reactive({
    withProgress(message = 'Thresholding data', value = 0, {
      x <- dat_all_raw()[dat_all_raw()$confidence >= input$thres, ]
      if(input$species != 'All species'){
        x <- x[x$common_name == input$species, ]
      }
      incProgress(1)
    })
    x
  })


  # Get all the species richness
  species_richness_all <- reactive({
    withProgress(message = 'Calculate species richness', value = 0, {
      x <- dat_all() %>% 
        group_by(filepath) %>%
        summarise(species_richness = n_distinct(common_name))
      incProgress(1)
    })
    x
  })
  
  # Group species richness by time period
  species_richness_by <- reactive({
    withProgress(message = 'Calculate species by time', value = 0, {
      x <- dat_all() %>% 
      summarise_by_time(.date_var = time,
                        .by = tolower(input$time_interval),
                        value = n_distinct(common_name))
      incProgress(1)
    })
    x
  })
  
  # Get all call activity
  call_activity_all <- reactive({
    withProgress(message = 'Calculate call activity', value = 0, {
      x <- dat_all() %>%
      group_by(filepath) %>%
      summarise(call_activity = n())
      incProgress(1)
    })
    x
  })
  
  # Group call activity by time period
  call_activity_by <- reactive({
    withProgress(message = 'Calculate call activity by time', value = 0, {
      x <- dat_all() %>% 
      summarise_by_time(.date_var = time,
                        .by = tolower(input$time_interval),
                        value = n())
      incProgress(1)
    })
    x
  })
  
  # Combined summary data
  summary_data <- reactive({
    withProgress(message = 'Build summary data', value = 0, {
      file_time <- unique(dat_all()[, c("filepath", "time_of_day")])
      x <- full_join(file_time, call_activity_all(), by = 'filepath') %>%
        full_join(species_richness_all(), by = 'filepath')
      incProgress(1)
    })
    x
  })
  
  # Dawn and dusk data
  dawn <- reactive({
    
    summary_data()[as.numeric(format(summary_data()$time_of_day,"%H")) < 12, ] 
    
  })
  dusk <- reactive({
    
    summary_data()[as.numeric(format(summary_data()$time_of_day,"%H")) >= 12, ] 
    
  })
  
  ##### PLOTS#####
  
  # Show/hide plots
  observe({
    if (input$species != 'All species') {
      hide("plot2")
      hide("sp_rich_plot")
    } else {
      show("plot2")
      show("sp_rich_plot")
    }
  })
  
  # Plot of confidence and species over time
  output$plot1 <- renderPlotly({
      if(!is.null(dat_all())){
        withProgress(message = 'Building plot 1', value = 0, {
          
        incProgress(0.1)
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
        incProgress(0.5)
        x <- ggplotly(plot1)
        incProgress(0.4)
      })
      x
    }
  })
  
  # Plot of overall species call activity
  output$plot2 <- renderPlotly({
    if(!is.null(dat_all())){
      if(input$species == 'All species'){
        withProgress(message = 'Building plot 2', value = 0, {
          incProgress(0.1)
          sp_rich <- as.data.frame(table(dat_all()$common_name), 
                                   stringsAsFactors = FALSE)
          colnames(sp_rich) <- c('Species', 'Count')
          incProgress(0.1)
          plot2 <- ggplot(sp_rich, aes(y = Count, 
                                       x = reorder(Species, -Count), 
                                       fill = Species)) +
                      geom_bar(stat = "identity") +
                      xlab("") +
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.05),
                            legend.position = "none")
          incProgress(0.4)
          x <- ggplotly(plot2, tooltip = c("Species", "Count"))
          incProgress(0.4)
        })
        x
      } else {
        NULL
      }
    }
  })
  
  # Plot species richness
  output$sp_rich_plot <- renderPlotly({
    
    if(!is.null(dat_all())){
      withProgress(message = 'Building plot 3', value = 0, {
        incProgress(0.1)
        plt <- ggplot(species_richness_by(), aes(x = time, y = value)) +
          geom_bar(stat = "identity") +
          xlab(str_to_title(input$time_interval)) +
          ylab('Species richness')
        incProgress(0.4)
        x <- ggplotly(plt)
        incProgress(0.5)
      })
      x
    }
  })
  
  # Plot call activity
  output$call_activity <- renderPlotly({
    
    if(!is.null(dat_all())){
      withProgress(message = 'Building plot 4', value = 0, {
        incProgress(0.1)
        plt <- ggplot(call_activity_by(), aes(x = time, y = value)) +
          geom_bar(stat = "identity") +
          xlab(str_to_title(input$time_interval)) +
          ylab('Call activity')
        incProgress(0.4)
        x <- ggplotly(plt)
        incProgress(0.5)
      })
      x
    }
  })
  
  # dawn plot
  output$dawn_plot <- renderPlotly({
    if(!is.null(dat_all())){
      withProgress(message = 'Building plot 5', value = 0, {
        incProgress(0.1)
        dawn_plot <- ggplot(dawn(), aes(x = time_of_day, y = call_activity)) +
          geom_point() +
          geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
          ggtitle('Aggregate dawn activity trend') +
          xlab('Time') +
          ylab('Number of calls')
        incProgress(0.4)
        x <- ggplotly(dawn_plot)
        incProgress(0.5)
      })
      x
    }
  })
  
  # dusk plot
  output$dusk_plot <- renderPlotly({
    if(!is.null(dat_all())){
      withProgress(message = 'Building plot 6', value = 0, {
        incProgress(0.1)
        dawn_plot <- ggplot(dusk(), aes(x = time_of_day, y = call_activity)) +
          geom_point() +
          geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
          ggtitle('Aggregate dusk activity trend') +
          xlab('Time') +
          ylab('Number of calls')
        incProgress(0.4)
        x <- ggplotly(dawn_plot)
        incProgress(0.5)
      })
      x
    }
  })

  ##### Call download and playback #####
  
  # Return wav file for a given ID
  wav <- reactive({
    if(!is.null(input$id)){
      
      clip_audio(df = dat_all(),
                 index = input$id)  
      
    } else {
      
      NULL
      
    }
  })
    
  # Play a wav file
  observeEvent(input$play, {
      play(object = wav(), )
    })
  
  # Download a wav file
  output$download_indiv <- downloadHandler(
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
  
  # best audio snippets
  bests <- reactive({
    
    bests <- dat_all() %>% group_by(common_name) %>% top_n(1, confidence)
     
  })
  
  
  # Download the best call of each species currently over the threshold
  output$download_best <- downloadHandler(
    filename = function() {
      paste0('best_calls_threshold_', input$thres, '.zip')
    },
    content = function(file) {
      
      tDir <- tempdir()
      names <- NULL
      
      withProgress(message = 'Bundling best calls', value = 0, {
        for(i in 1:nrow(bests())){
          incProgress(1/nrow(bests()))
          wav <- clip_audio(index = bests()$ID[i], df = as.data.frame(bests()))
          wav_name <- paste0(gsub(' ', '_', bests()$common_name[i]),
                             '_',
                             format(bests()$time[i], format = '%Y%M%d'),
                             '_',
                             bests()$confidence[i],
                             '.wav')
          writeWave(wav, filename = file.path(tDir, wav_name))
          names <- c(names, file.path(tDir, wav_name))
        }
      })

      withProgress(message = 'Zipping files', value = 0, {
        incProgress(0.1)
        x <- zip(zipfile = file, files = names, flags = '-j')
        incProgress(0.9)
      })
      x
   }
  )
  
  ##### Render UI elements #####
  observe({
    updateSelectInput(session, "species",
                      choices = c('All species',
                                  sort(unique(dat_all_raw()$common_name[dat_all_raw()$confidence >= input$thres]))))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
