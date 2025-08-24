library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Class Analysis", tabName = "class_analysis", icon = icon("chalkboard-teacher")),
    menuItem("Student Analysis", tabName = "student_analysis", icon = icon("user-graduate"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "class_analysis",
            h2("Class Analysis tab content"),
            # Add a tabBox to hold the different graphs
            tabBox(
              width = 12, # Make the tabBox span the full width
              # Graph 1: Class Average by Learning Type
              tabPanel("Learning Type Averages", 
                       fluidRow(
                         box(
                           title = "Class Average by Learning Type", 
                           status = "primary", 
                           solidHeader = TRUE, 
                           width = 12, 
                           plotOutput("class_average_plot")
                         )
                       )
              ),
              # Graph 2: Student Ranking Table
              tabPanel("Student Ranking", 
                       fluidRow(
                         box(
                           title = "Top / Bottom Student Ranking", 
                           status = "primary", 
                           solidHeader = TRUE, 
                           width = 12, 
                           # Using DTOutput for interactive data table
                           DT::dataTableOutput("student_ranking_table")
                         )
                       )
              ),
              # Graph 3: Correctness Heatmap
              tabPanel("Question Correctness Heatmap", 
                       fluidRow(
                         box(
                           title = "Correctness Per Question Across Students", 
                           status = "primary", 
                           solidHeader = TRUE, 
                           width = 12, 
                           plotOutput("question_heatmap", height = "600px") # Adjust height as needed
                         )
                       )
              )
            )
    ),
    tabItem(tabName = "student_analysis",
            h2("Student Analysis"), # Updated title for the main tab content
            
            fluidRow( # Row for student selection and details
              box(
                title = "Select Student",
                status = "info",
                solidHeader = TRUE,
                width = 4,
                # Dropdown for student selection
                selectInput("selected_student_name", "Choose a Student:", choices = NULL)
              ),
              box(
                title = "Student Details",
                status = "primary",
                solidHeader = TRUE,
                width = 8,
                # Display student's name and registration number
                htmlOutput("student_info") 
              )
            ),
            
            # Inner tabBox for student-specific graphs
            tabBox(
              width = 12,
              id = "student_analysis_tabs", # Give an ID to this tabBox
              tabPanel("Scores by Learning Type", 
                       fluidRow(
                         box(
                           title = "Student Scores by Learning Type", 
                           status = "primary", 
                           solidHeader = TRUE, 
                           width = 12, 
                           plotOutput("student_learning_type_scores")
                         )
                       )
              )
              # Other student-specific graph tabs will go here
            )
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = tags$span("PERFORMANCE ANALYSIS", 
                                    style = "text-align: left; 
                                             font-size: 20px; 
                                             display: block;")),
  sidebar,
  body
)
