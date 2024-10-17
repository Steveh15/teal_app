# Load necessary libraries
library(shiny)
library(tidyverse)



# Define server logic
server <- function(input, output, session) {
  
  # Input dataset
  input_data <- reactive({
    NULL
  })
  
  
  # Dataset that will be plotted once filtering has been applied
  plot_data <- reactive({
    # df %>% filter(ARM %in% input$selected_arms)
    NULL
  })
  
  output$armCheckboxes <- renderUI({
    
    if (is.null(input_data())) {
      return(NULL)  # Return nothing if input_data is NULL
    }
    # Create checkbox group input dynamically
    arm_choices <- unique(df$ARM) %>% sort()
    
    subject_counts <- sapply(arm_choices, function(arm) {
      length(unique(df$USUBJID[df$ARM == arm]))  # Count unique subjects for each arm
    })
    
    choices_with_counts <- paste0(arm_choices, " (N = ", subject_counts, ")")  # Create labels with counts
    checkboxGroupInput("selected_arms", 
                       "Select Arms", 
                       # choices = arm_choices,  # Use the dynamic choices
                       # label = TRUE,
                       choiceNames = choices_with_counts,
                       choiceValues = arm_choices,
                       selected = arm_choices  # Select all by default
    )
  })
  
  

  
  n_subjects <- reactive({
    length(unique(plot_data()$USUBJID))
  })
  
  # Number of subjects to be plotted
  num_subjects <- reactiveVal(5)  # Default value
  print("React!")
  # print(n_subjects())
  # print(num_subjects())
  observeEvent(input$num_subjects_input, {
    # Check if the input is valid and update the reactive value
    new_value <- as.numeric(input$num_subjects_input)
    print(new_value)
    if(is.na(new_value) | new_value < 0){
      # if not valid then display warning, do not update value.
    } 
    else if(new_value > n_subjects()){
      # If above valid range, set to max of valid range
      updateNumericInput(session, "num_subjects_input", value = n_subjects())
      num_subjects(n_subjects())
    } else{
      num_subjects(new_value)  # Update reactive value
    }
    
  })
  


  # Observe changes in selected arms and update the numeric input accordingly
  observeEvent(input$selected_arms, {

    if(is.null(input$selected_arms)){
      
      updateNumericInput(session, "num_subjects_input",
                         min = 0,
                         max = 0,
                         value = 0)
    }
    else{
      print("Hello")
      print(n_subjects())
      print(num_subjects())
      # Update valid range and value if it is no longer in bounds.
      updateNumericInput(session, "num_subjects_input",
                         min = 0,
                         max = n_subjects(),
                         value = min(max(num_subjects(),1), n_subjects()))
    }

  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  output$concentrationPlot <- renderPlot({
      
    req(plot_data())
    
      if(is.null(plot_data()) | num_subjects() == 0 | plot_data() %>% nrow() == 0){

        
        ggplot() +
          labs(x = "Ordinal Variable", y = "Concentration (AVAL)", title = "Concentration Over Time by Subject") +
          geom_blank() +  # This ensures a blank plot, but still respects the axes and other scales.
          theme_minimal()

      } else{
        subjects_to_show <- sort(unique(plot_data()$USUBJID))[1:num_subjects()]
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
}

