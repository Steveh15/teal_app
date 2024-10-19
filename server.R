# Load necessary libraries
library(shiny)
library(tidyverse)
library(reactlog)
reactlog::reactlog_enable()

# Define server logic
server <- function(input, output, session) {
  
  # --- Input Data
  #############################
  input_data <- reactive({
    
    req(input$data_file)
    
    file <- input$data_file
    ext <- tools::file_ext(file$datapath)
    
    if(ext == "xpt"){
      data <- haven::read_xpt(file$datapath)
    }
    else if(ext == "sas7bdat"){
      data <- haven::read_sas(file$datapath)
    } else {
      showNotification("Unsupported file type. Please upload an XPT or SAS7BDAT file.", type = "error")
      data <- NULL
    }
    
    data
  })
  
  # --- Arm values and widget
  #############################
  
  arm_values <- reactive({
    req(input_data()) 
    sort(unique(input_data()$ARM))
  })

  output$armCheckboxes <- renderUI({
    req(arm_values())  
    
    subject_counts <- sapply(arm_values(), function(arm) {
      length(unique(input_data()$USUBJID[input_data()$ARM == arm]))  # Count unique subjects for each arm
    })
    
    
    arm_labels <- paste0(arm_values(), " (N = ", subject_counts, ")")  # Create labels with counts
    
    checkboxGroupInput(
      inputId = "selected_arms",
      label = "Select Arms",
      # choices = arm_values(),
      choiceNames = arm_labels,
      choiceValues =    arm_values(),
      selected = arm_values()  # Default to select all arms
    )
  })
  
  #
  # --- Create plot data
  #############################
  
  
  n_subjects <- reactiveVal(NULL)

  n_subjects_to_plot <- reactiveVal(5)

  n_plot_is_valid <- reactiveVal(TRUE)
  

  plot_data <- reactive({
    
    req(input_data())
  
    if(is.null(input$selected_arms)){
      df <- input_data() %>% filter(FALSE)  
    } else{
      df <- input_data() %>% filter(ARM %in% input$selected_arms)  
    }
    
    # --- Update n_subjects_1
    n_subjects(length(unique(df$USUBJID)))
    
    df
    
  })
  
  output$plot_n <- renderUI({
    req(input_data())

    numericInput(
      "n_subjects_input",
      "Number of Subjects to Show:",
      min = 0,
      # max = n_subjects(),
      step = 1,
      value = isolate(n_subjects_to_plot())
    )
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

  
  #
  # --- Plot results
  #############################
  
  
  output$concentrationPlot <- renderPlot({

    req(plot_data())


    if(is.null(input$selected_arms)){

      ggplot() +
        labs(x = "Ordinal Variable", y = "Concentration (AVAL)", title = "Concentration Over Time by Subject") +
        geom_blank() +
        theme_minimal()

    }
    else{

      subjects_to_show <- sort(unique(plot_data()$USUBJID))[1:n_subjects_to_plot()]
      data_to_plot <- plot_data() %>% filter(USUBJID %in% subjects_to_show)

      ggplot(data_to_plot, aes(x = as.factor(x_ord), y = AVAL, group = USUBJID, color = USUBJID)) +
        geom_line() +
        geom_point() +
        labs(x = "Ordinal Variable", y = "Concentration (AVAL)", title = "Concentration Over Time by Subject") +
        theme_minimal() +
        theme(legend.position = "right") +
        scale_x_discrete(
          labels = data_to_plot$x_label  # Concatenate visit and timepoint labels
        )
    }




  })
  
  
  
  output$error_message <- renderText({
    if (!n_plot_is_valid()) {
      return("Invalid input, please enter a positive integer")
    } else {
      return("")  # Return an empty string when valid
    }
  })
  
  output$text_output <- renderText({
    # print("HELLO")
    req(n_subjects_to_plot)
    paste0( n_subjects(), " - ", n_subjects_to_plot())
    # n_subjects_to_plot()
    # req(plot_data())
    # plot_data() %>% nrow()
    })
  
  
}

