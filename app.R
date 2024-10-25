# Load necessary libraries
library(shiny)
library(teal)
library(teal.modules.general)
library(teal.data)
library(haven)

# Create a teal data module
data_module <- teal_data_module(
  ui = function(id) {
    ns <- NS(id)
    div(
      # File input for user to upload a data file
      fileInput(ns("data_file"), 
                "ADPC input",
                accept = c(".sas7bdat", ".xpt")
      ),
      # Optional action button to confirm the file upload
      actionButton(ns("submit"), "Submit")
    )
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
          showNotification("Unsupported file type. Please upload an XPT or SAS7BDAT file.", type = "error")
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


?teal_data

# Create the app with the data module and an example module
app <- init(
  data = data_module,
  modules = example_module()
)

# Run the app if in an interactive environment
if (interactive()) {
  shinyApp(app$ui, app$server)
}
