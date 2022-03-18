# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(datasets)

# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                
                navbarPage(
                  
                  "Monthly Reporting",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             #textInput("txt1", "Given Name:", ""),
                             fileInput("file", label = h3("File input")),
                             dateRangeInput("dates", label = h3("Date range")),
                             
                           ), # sidebarPanel
                           mainPanel(
                             img(src='DLI_logo.png', align = "center"),
                             
                             
                           ) # mainPanel
                           
                  ), 
                  tabPanel("Navbar 2",
                           sidebarPanel(
                             tags$h3("Input:"),
                             selectInput('xcol','X Variable', names(iris)),
                             selectInput('ycol','Y Variable', names(iris)),
                             
                           ), 
                           mainPanel(
                             h1("Header 1"),
                             plotOutput("plot", width = 250, height = 250)
                             
                           ) 
                           
                  ),
                  tabPanel("Navbar 3", "This panel is intentionally left blank"),
                  tabPanel("Navbar 4", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {

    output$plot <- renderPlot(
      xcol = function() input$xcol,
      ycol = function() input$ycol,
      ggplot(iris, aes(wt, mpg)) + geom_point()
    )
   
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)