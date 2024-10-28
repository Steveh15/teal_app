# Load necessary libraries
library(shiny)
library(teal)
library(teal.modules.general)
library(teal.data)
library(haven)
library(dplyr)

# ADPC <- pharmaverseadam::adpc
#
# haven::read_xpt("data/adpc.xpt")
# ADPC = pharmaverseadam::adpc

preloaded_data <- teal_data(ADPC = haven::read_xpt("data/adpc.xpt"))


# Create a teal data module
data_module <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    div(# File input for user to upload a data file
      fileInput(ns("data_file"), "ADPC input", accept = c(".sas7bdat", ".xpt")),
      # Optional action button to confirm the file upload
      actionButton(ns("submit"), "Submit"))
  },
  
  server = function(id) {
    moduleServer(id, function(input, output, session) {
      # Create a reactive variable to store the uploaded data
      input_data <- reactiveVal(NULL)
      
      # Load dataset when user clicks submit
      observeEvent(input$submit, {
        req(input$data_file)
        
        # Process the uploaded file
        file <- input$data_file$datapath
        ext <- tools::file_ext(file)
        
        
        if (ext == "xpt") {
          input_data(haven::read_xpt(file))
        } else if (ext == "sas7bdat") {
          input_data(haven::read_sas(file))
        } else {
          showNotification("Unsupported file type. Please upload an XPT or SAS7BDAT file.",
                           type = "error")
          return(NULL)
        }
        
      })
      
      # Return a teal_data object containing the uploaded data
      return(reactive({
        req(input_data())
        teal_data(d1 = input_data())
      }))
    })
  }
)


example_module_2 <- function(label = "example teal module") {
  checkmate::assert_string(label)
  
  module(
    label = label,
    server = function(id, data) {
      checkmate::assert_class(data, "reactive")
      checkmate::assert_class(isolate(data()), "teal_data")
      
      moduleServer(id, function(input, output, session) {
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
      })
    },
    ui = function(id) {
      ns <- NS(id)
      sidebarLayout(sidebarPanel(selectInput(
        ns("dataname"), "Choose a dataset", choices = NULL
      )),
      mainPanel(verbatimTextOutput(ns("dataset"))))
    }
  )
}

line_chart_module <- function(label = "Line Chart") {
  checkmate::assert_string(label)
  
  module(
    label = label,
    server = function(id, data) {
      checkmate::assert_class(data, "reactive")
      checkmate::assert_class(isolate(data()), "teal_data")
      
      moduleServer(id, function(input, output, session) {
        
        updateSelectInput(session, "dataname", choices = isolate(datanames(data())))
        
        output$dataset <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
        
        
        n_subjects_to_plot <- reactiveVal(5)
        
        n_plot_is_valid <- reactiveVal(TRUE)
        
        n_start <- reactiveVal(1)
        
        
        
        n_subjects <- reactiveVal(length(unique(data()[[input$dataname]]$USUBJID)))
        
        output$plot_n <- renderUI({
          
          numericInput(
            "n_subjects_input",
            "Number of Subjects to Show:",
            min = 0,
            max = length(unique(data()[[input$dataname]]$USUBJID)),
            # max = 1,
            step = 1,
            value = isolate(n_subjects_to_plot())
          )
        })
        
        
        
        output$decrease_subs <- renderUI({
          actionButton("decrease_subs", paste0("Prev ", n_subjects_to_plot()))
        })
        
        output$increase_subs <- renderUI({
          actionButton("increase_subs", paste0("Next ", n_subjects_to_plot()))
        })
        
        observeEvent(input$decrease_subs, {
          
          if(n_start() - n_subjects_to_plot() >= 1){
            n_start(n_start()-n_subjects_to_plot())
          } else{
            n_start(1)
          }
          
        })
        
        observeEvent(input$increase_subs, {
          if(n_start() + n_subjects_to_plot() <= n_subjects()){
            n_start(n_start()+n_subjects_to_plot())
          }
        })
        
        observeEvent(input$n_subjects_input, {
          
          req(input$n_subjects_input)
          new_value <- as.numeric(input$n_subjects_input)
          
          if(!is_integer(new_value) | new_value < 0 ){
            n_plot_is_valid(FALSE)
          } else{
            n_plot_is_valid(TRUE)
            if(new_value != n_subjects_to_plot()){
              n_subjects_to_plot(new_value)
            }
          }
          
        })
        
        
        
        
        # Render the line chart
        
        output$line_chart <- renderPlot({
          
          req(data()[[input$dataname]])
          
          plot_data <- data()[[input$dataname]]
          
          
          # subjects_to_show <- sort(unique(plot_data$USUBJID))[1:5]
          subjects_to_show <- sort(unique(plot_data$USUBJID))[n_start():(n_start() + n_subjects_to_plot() - 1)]
          data_to_plot <- plot_data %>% filter(USUBJID %in% subjects_to_show)
          
          
          ggplot(data_to_plot,
                 aes(
                   x = as.factor(x_ord),
                   y = AVAL,
                   group = USUBJID,
                   color = USUBJID
                 )) +
            geom_line() +
            geom_point() +
            labs(x = "Ordinal Variable",
                 y = "Concentration (AVAL)",
                 title = "Concentration Over Time by Subject") +
            theme_minimal() +
            theme(legend.position = "right") +
            scale_x_discrete(labels = data_to_plot$x_label)
        })
        
        
        
      })
    },
    ui = function(id) {
      ns <- NS(id)
      
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("dataname"), "Choose a dataset", choices = NULL),
          fluidRow(
            column(3, uiOutput(ns("decrease_subs"))),
            # Column for second action button
            column(6, uiOutput(ns("plot_n"))),
            # Column for numeric input
            column(3, uiOutput(ns("increase_subs")))   # Column for first action button
          ),
          # selectInput(ns("x_var"), "Select X variable", choices = NULL),
          # selectInput(ns("y_var"), "Select Y variable", choices = NULL),
          width = 4
        ),
        mainPanel(plotOutput(ns("line_chart")), verbatimTextOutput(ns("dataset")))
        
      )
    }
  )
}
? sidebarLayout

# Create the app with the data module and an example module
app <- init(# data = data_module,
  data = preloaded_data,
  modules = modules(
    tm_data_table(),
    line_chart_module()
    
    )
  )

# Run the app if in an interactive environment
if (interactive()) {
  shinyApp(app$ui, app$server)
}
