#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(visNetwork)
library(tidygraph)
library(dplyr)

# Define UI for application that draws a histogram
ui <- navbarPage("Research Implementation Policy Institute",
                 id = "tabs",

    
    tabPanel("Home"),
    navbarMenu("CRS",
                 tabPanel("data viz", textInput("example text", "label1")),
                 tabPanel("something else", textInput("example text", "label2")),
                 tabPanel("another thing", textInput("example text", "label3"))
      ),
      navbarMenu("menu 2")
)
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
