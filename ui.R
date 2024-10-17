library(shiny)
library(tidyverse)


# Define the UI (User Interface)
ui <- fluidPage(
  titlePanel("Drug Concentration App"),
  tabsetPanel(
    tabPanel("App",
             fluidRow(
               column(4, h3("Controls"),
                      
                      
                      uiOutput("armCheckboxes"),
                      uiOutput("num_subjects_input"),
                      
                      # numericInput("num_subjects_input", 
                      #              "Number of Subjects to Show:", 
                      #              value = 5,  # Default value
                      #              min = 0,  # Minimum value
                      #              max = length(unique(df$USUBJID))  # Maximum value
                      # )
                      ),
               column(8, h3("Plot"),
                      plotOutput("concentrationPlot"))
             )
    ),
    tabPanel("Information", 
             h3("Information will go here"))
  )
)