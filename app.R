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
          numericInput('id', label = 'ID to play', step = 1, value = 1),
          actionButton(inputId = 'play', label = 'Play')
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("plot1"),
          plotlyOutput("plot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  dat_all <- reactive({
    
    if(!is.null(input$files)){
      
      files <- input$files
      dat <- lapply(files$datapath, FUN = read.csv.row)
      dat_all <- do.call(rbind, dat)
      colnam <- names(read.csv(files$datapath[1], header = TRUE)[1,])
      colnames(dat_all) <- colnam
      dat_all$time <- as.POSIXct(file_to_time(dat_all$filepath))
      dat_all$ID <- 1:nrow(dat_all) 
      dat_all <- dat_all[dat_all$confidence >= input$thres, ]
      dat_all
      
    }
    
  })
  
  output$plot1 <- renderPlotly({
    if(!is.null(dat_all())){
      plot1 <- ggplot(dat_all(),
                     aes(y = confidence,
                         x = time, # Change this to be the time through the night
                         col = common_name,
                         text = ID)) +
        geom_point() +
        ylim(0, 1) +
        theme(legend.position = "none")
      ggplotly(plot1)
    }
  })
  
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
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.05),
                        legend.position = "none")
      ggplotly(plot2)
    }
  })
  
  observeEvent(input$play, {
    dat_all <- dat_all()
    play_clip(df = dat_all,
              ID = input$id)})
}

# Run the application 
shinyApp(ui = ui, server = server)
