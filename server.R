# Load necessary libraries
library(shiny)
library(tidyverse)



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
    req(arm_values())  # Ensure arm_values are available
    
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
  
  plot_data <- reactive({
    
    req(input_data())
    req(input$selected_arms)
    
    input_data() %>% filter(ARM %in% input$selected_arms)
  })
  
  output$plot_n <- renderUI({
    req(plot_data())
    req(nrow(plot_data()) != 0)
    
    n_subjects <- length(unique(plot_data()$USUBJID))
    default_value = min(5, n_subjects)
    
    numericInput(
      "n_subjects_input",
      "Number of Subjects to Show:",
      min = 0,
      max = n_subjects,
      value = default_value
    )
  })
  
  n_subjects_to_plot <- reactiveVal(NULL)
  
  observeEvent(input$n_subjects_input, {
    
    new_value <- as.numeric(input$n_subjects_input)
    
    n_subjects <- length(unique(plot_data()$USUBJID))
    
    # Validate the new value
    if (is.na(new_value)){
      # Do nothing
    } else if (new_value < 0 ){
      n_subjects_to_plot(0)
      updateNumericInput(session, "n_subjects_input", value = 0)
    } else if(new_value > n_subjects){
      n_subjects_to_plot(n_subjects)
      updateNumericInput(session, "n_subjects_input", value = n_subjects)
      
    } else{
      n_subjects_to_plot(new_value)
    }

  })
  
  
  #
  # --- Plot results
  #############################
  
  
  output$concentrationPlot <- renderPlot({
    
    req(plot_data())
    req(n_subjects_to_plot())
    print("We got there!")
    print(paste0("nplot: ", n_subjects_to_plot()))
    print(paste0("other n: ", nrow(plot_data())))
    print(input$selected_arms)


    if(n_subjects_to_plot() == 0 | nrow(plot_data()) == 0){


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
  
  
  output$text_output <- renderText({
    req(n_subjects_to_plot)
    print(n_subjects_to_plot())
    # n_subjects_to_plot()
    # req(plot_data())
    # plot_data() %>% nrow()
    })
  
  
}

