library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Student Dashboard"),
  
  dashboardSidebar(disable = TRUE),  # no sidebar used in your layout
  
  dashboardBody(
    fluidRow(
      # To Do List
      box(title = "To do", width = 4, solidHeader = TRUE, status = "primary",
          checkboxGroupInput("todo_list", label = NULL,
                             choices = list("Quiz 1" = "quiz1",
                                            "Group Assignment 1" = "group1",
                                            "Workshop 1" = "workshop1",
                                            "Group Assignment 2" = "group2",
                                            "Workshop 2" = "workshop2",
                                            "Final Exam" = "exam"),
                             selected = "quiz1"),
          p("Deadlines:"),
          tags$ul(
            tags$li("Quiz 1 - Fr 30 May 2025 at 23:59"),
            tags$li("Group Assignment 1 - Tu 10 June 2025 at 12:00"),
            tags$li("Workshop 1 - Th 12 June 2025 at 23:59"),
            tags$li("Group Assignment 2 - Fri 20 June 2025 at 23:59"),
            tags$li("Workshop 2 - Tu 1 July 2025 at 23:59"),
            tags$li("Final Exam - Tu 1 July 2025 at 10:00")
          )
      ),
      
      # Personal avatar and badges
      box(title = "Personal avatar", width = 4, solidHeader = TRUE, status = "info",
          tags$img(src = "avatar.png", height = "100px"),  # Add image to www folder
          p("Earn badges to unlock accessories for your avatar!"),
          tags$div(
            tags$img(src = "badge1.png", height = "30px"),  # badges should be added to www/
            tags$img(src = "badge2.png", height = "30px"),
            tags$img(src = "badge3.png", height = "30px"),
            tags$img(src = "badge4.png", height = "30px"),
            tags$img(src = "badge5.png", height = "30px")
          )
      ),
      
      # Badges and explanations
      box(title = "Badges and explanation", width = 4, solidHeader = TRUE, status = "success",
          tags$ul(
            tags$li(tags$b("Quiz Master Badge:"), " Top 25% of quizzes."),
            tags$li(tags$b("Steady Engagement Badge:"), " Weekly activity."),
            tags$li(tags$b("Early Bird Badge:"), " Submit 24+ hrs early."),
            tags$li(tags$b("Comeback Kid Badge:"), " Significant quiz improvement."),
            tags$li(tags$b("Conversation Starter Badge:"), " Frequent discussion initiator.")
          )
      )
    ),
    
    fluidRow(
      # Grade progress chart
      box(title = "Grade average progress", width = 6, solidHeader = TRUE,
          plotlyOutput("grade_plot", height = "250px")
      ),
      
      # Practice quiz progress pie chart
      box(title = "Practise quiz progress", width = 6, solidHeader = TRUE,
          plotlyOutput("quiz_pie", height = "250px")
      )
    )
  )
)

server <- function(input, output) {
  output$grade_plot <- renderPlotly({
    plot_ly(
      x = 1:8, y = c(6, 5, 8, 6, 6, 6, 8, 9),
      type = 'scatter', mode = 'lines+markers',
      line = list(color = 'orange'),
      name = 'Quiz progress'
    ) %>%
      layout(xaxis = list(title = "Week number"),
             yaxis = list(title = "Grade"))
  })
  
  output$quiz_pie <- renderPlotly({
    plot_ly(
      labels = c("To Do", "Completed"),
      values = c(80, 20),  # Adjust these to reflect real values
      type = 'pie',
      marker = list(colors = c("cornflowerblue", "goldenrod"))
    )
  })
}

shinyApp(ui, server)