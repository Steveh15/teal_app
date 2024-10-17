library(shiny)
library(tidyverse)
# library(bslib)

# Define the UI (User Interface)
ui <- fluidPage(
  titlePanel("Drug Concentration App"),
  tabsetPanel(
    tabPanel("App",
             fluidRow(
               column(4, h3("Controls"),
                      fileInput("data_file", 
                                "ADPC input",
                                accept = c(".sas7bdat", ".xpt")
                      ),
                      uiOutput("armCheckboxes"),
                      uiOutput("plot_n"),
                      shiny::textOutput("text_output")
                      
                      
                      
                      # uiOutput("num_subjects_input"),

               ),
               column(8, 
                      h3("Plot"),
                      plotOutput("concentrationPlot")
                      )
             )
    ),
    tabPanel("Information", 
             h3("Information will go here"))
  )
)


