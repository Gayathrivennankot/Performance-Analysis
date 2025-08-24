library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr) # For pivot_longer
library(DT) # For interactive tables

server <- function(input, output, session) {
  
  # Reactive values to hold the data from the CSV files
  marks_raw_data <- reactiveVal(NULL)
  mapping_raw_data <- reactiveVal(NULL)
  
  # Use an observer to read the data when the app starts
  observe({
    marks_path <- "marks.csv"
    mapping_path <- "question_mapping.csv"
    
    if (file.exists(marks_path) && file.exists(mapping_path)) {
      
      # Read the CSV files
      marks_raw_data(read.csv(marks_path, stringsAsFactors = FALSE))
      mapping_raw_data(read.csv(mapping_path, stringsAsFactors = FALSE))
      
      cat("Data loaded successfully from marks.csv and question_mapping.csv.\n")
      
    } else {
      cat("Error: One or both CSV files not found. Please ensure they are in the same directory as the app.\n")
    }
  })
  
  # Reactive expression for cleaned marks data
  cleaned_marks_data <- reactive({
    req(marks_raw_data())
    df <- marks_raw_data()
    
    # Identify question columns (Q1 to Q25)
    q_cols <- grep("^Q[0-9]+$", names(df), value = TRUE)
    
    # Convert 'N' to 0 (not attempted/incorrect), then to numeric
    df[q_cols] <- lapply(df[q_cols], function(x) as.numeric(ifelse(x == "N", 0, x)))
    
    # Replace any other NA values (if present) with 0 for question scores
    df[q_cols][is.na(df[q_cols])] <- 0
    
    # Recalculate 'Total' column based on cleaned question scores
    df$Total <- rowSums(df[q_cols], na.rm = TRUE)
    
    df
  })
  
  # Reactive expression for merged data (marks + question mapping)
  merged_data <- reactive({
    req(cleaned_marks_data(), mapping_raw_data())
    marks_df <- cleaned_marks_data()
    mapping_df <- mapping_raw_data()
    
    # Pivot marks data to long format for easier merging with mapping
    marks_long <- marks_df %>%
      pivot_longer(
        cols = starts_with("Q"),
        names_to = "question",
        values_to = "score"
      ) %>%
      select(Reg.No., Name, question, score) # Select relevant columns
    
    # Merge with question mapping data
    merged <- left_join(marks_long, mapping_df, by = "question")
    
    merged
  })
  
  # Populate the student dropdown in the UI
  observe({
    req(cleaned_marks_data())
    student_names <- unique(cleaned_marks_data()$Name)
    updateSelectInput(session, "selected_student_name", choices = sort(student_names))
  })
  
  # Reactive expression for the selected student's data
  selected_student_data <- reactive({
    req(input$selected_student_name, cleaned_marks_data())
    cleaned_marks_data() %>%
      filter(Name == input$selected_student_name)
  })
  
  # Reactive expression for the selected student's merged data (for learning type analysis)
  selected_student_merged_data <- reactive({
    req(input$selected_student_name, merged_data())
    merged_data() %>%
      filter(Name == input$selected_student_name)
  })
  
  # Output: Student Name and University Number (Reg No.)
  output$student_info <- renderUI({
    req(selected_student_data())
    student <- selected_student_data()
    HTML(paste0("<h4>", student$Name, "</h4>",
                "<p><strong>Registration No.:</strong> ", student$Reg.No., "</p>"))
  })
  
  # --- Class Analysis Outputs (Existing) ---
  
  # Graph 1: Bar chart – Class average by learning type
  output$class_average_plot <- renderPlot({
    req(merged_data())
    
    plot_data <- merged_data() %>%
      group_by(group) %>%
      summarise(average_score = mean(score, na.rm = TRUE)) %>%
      ungroup() %>%
      # Order groups by average score for better visualization
      arrange(desc(average_score)) %>%
      mutate(group = factor(group, levels = group))
    
    ggplot(plot_data, aes(x = group, y = average_score, fill = group)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = sprintf("%.2f", average_score)), vjust = -0.5, size = 4) +
      labs(title = "Class Average Score by Learning Type",
           x = "Learning Type",
           y = "Average Score (0-1)",
           fill = "Learning Type") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.title = element_text(size = 12),
            legend.position = "none") +
      scale_y_continuous(limits = c(0, 1)) # Scores are between 0 and 1
  })
  
  # Graph 2: Ranking table with bar indicators
  output$student_ranking_table <- DT::renderDataTable({
    req(cleaned_marks_data())
    
    ranking_df <- cleaned_marks_data() %>%
      arrange(desc(Total)) %>% # Sort by total score
      select(Name, Total, starts_with("Q")) # Include all question scores
    
    # Function to create HTML indicators for scores (Green for 1, Red for 0)
    # This is a basic visual indicator, not a true sparkline chart
    create_score_indicators <- function(scores) {
      sapply(scores, function(score) {
        if (score == 1) {
          return("<span style='color: green; font-weight: bold;'>&#10003;</span>") # Checkmark
        } else {
          return("<span style='color: red; font-weight: bold;'>&#10008;</span>")  # Cross mark
        }
      }) %>% paste(collapse = "")
    }
    
    # Apply the function to each row's question scores
    # Create a new column that combines all Q scores into a single indicator string
    ranking_df <- ranking_df %>%
      rowwise() %>% # Operate row by row
      mutate(Q_Indicators = create_score_indicators(c_across(starts_with("Q")))) %>%
      ungroup() %>%
      select(Name, Total, Q_Indicators) # Keep only relevant columns for display
    
    DT::datatable(
      ranking_df,
      escape = FALSE, # Allow HTML in Q_Indicators column
      options = list(
        paging = TRUE,
        searching = TRUE,
        info = TRUE,
        ordering = TRUE,
        scrollX = TRUE, # Allow horizontal scrolling if many questions
        columnDefs = list(
          list(width = '200px', targets = 0), # Name
          list(width = '80px', targets = 1),  # Total
          list(width = '400px', targets = 2)  # Q_Indicators
        )
      ),
      caption = "Student Performance Ranking with Question Indicators"
    )
  })
  
  # Graph 3: Stacked bar / heatmap – Correctness per question across students
  output$question_heatmap <- renderPlot({
    req(merged_data())
    
    heatmap_data <- merged_data() %>%
      # Ensure students are ordered by total score for consistent heatmap ordering
      left_join(cleaned_marks_data() %>% select(Reg.No., Total), by = "Reg.No.") %>%
      mutate(
        Name = factor(Name, levels = unique(cleaned_marks_data() %>% arrange(desc(Total)) %>% pull(Name))),
        question = factor(question, levels = paste0("Q", 1:25)) # Ensure questions are ordered Q1-Q25
      )
    
    ggplot(heatmap_data, aes(x = question, y = Name, fill = factor(score))) +
      geom_tile(color = "white", linewidth = 0.5) + # Add white borders to tiles
      scale_fill_manual(values = c("0" = "salmon", "1" = "seagreen"),
                        labels = c("Incorrect/Not Attempted", "Correct"),
                        name = "Score") +
      labs(title = "Correctness Per Question Across Students",
           x = "Question Number",
           y = "Student Name") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      ) +
      coord_fixed(ratio = 0.5) # Adjust aspect ratio for better visual
  })
  
  # --- Student Analysis Outputs (New) ---
  
  # Graph 1 (Student Analysis): Bar chart – Student scores by learning type
  output$student_learning_type_scores <- renderPlot({
    req(selected_student_merged_data())
    
    plot_data <- selected_student_merged_data() %>%
      group_by(group) %>%
      summarise(average_score = mean(score, na.rm = TRUE)) %>%
      ungroup() %>%
      # Order groups by average score for better visualization
      arrange(desc(average_score)) %>%
      mutate(group = factor(group, levels = group))
    
    student_name <- unique(selected_student_merged_data()$Name)
    
    ggplot(plot_data, aes(x = group, y = average_score, fill = group)) +
      geom_bar(stat = "identity", color = "white") +
      geom_text(aes(label = sprintf("%.2f", average_score)), vjust = -0.5, size = 4) +
      labs(title = paste0(student_name, "'s Average Score by Learning Type"),
           x = "Learning Type",
           y = "Average Score (0-1)",
           fill = "Learning Type") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.title = element_text(size = 12),
            legend.position = "none") +
      scale_y_continuous(limits = c(0, 1)) # Scores are between 0 and 1
  })
  
}
