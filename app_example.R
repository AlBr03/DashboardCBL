library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)

# Cached reactive values
cachedUserData <- reactiveVal(NULL)

getUserData <- function(course_id) {
  # For development purposes we wish to see a user that has submitted a quiz (27974 has no quizzes)
  data <- quiz_progression %>%
    filter(course_id == !!course_id) %>%
    collect()
  
  # Check if data is empty before proceeding
  if (nrow(data) == 0) {
    # If no data found for quiz progression, look up enrollment data
    data <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_enrollments")) %>%
      filter(course_id == !!course_id, enrollment_type == "StudentEnrollment") %>%
      collect()
  }
  
  # Check if user_id is available in either dataset
  if (nrow(data) > 0) {
    user_id <- sample(data$user_id, 1)
    return(user_id)
  } else {
    return(NULL)  # If no users found, return NULL
  }
}

# Static data (shared across users)
quizzes <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
  rename(quiz_id = id)

submissions <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions"))
weblogs <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_web_logs"))

total_quizzes <- quizzes %>%
  filter(workflow_state == "available") %>%
  group_by(course_id) %>%
  summarise(nr_quizzes = n())

quiz_progression <- quizzes %>%
  inner_join(total_quizzes, by = "course_id") %>%
  inner_join(submissions, by = c("quiz_id" = "quiz_id", "course_id" = "course_id")) %>%
  group_by(course_id, user_id) %>%
  summarise(
    nr_submissions = n_distinct(quiz_id),
    nr_quizzes = max(nr_quizzes),
    .groups = "drop"
  )

weblogs_chart <- weblogs %>%
  mutate(
    date = substr(timestamp, 1, 10),
    item = case_when(
      grepl("file", web_application_controller) ~ "files",
      grepl("quizzes", web_application_controller) ~ "quizzes",
      grepl("wiki_pages", web_application_controller) ~ "wiki_pages",
      grepl("module", web_application_controller) ~ "modules",
      grepl("discussion", web_application_controller) ~ "discussions",
      grepl("grade", web_application_controller) ~ "grades",
      TRUE ~ "other"
    )
  ) %>%
  group_by(course_id, user_id, date, item) %>%
  summarise(count = n(), .groups = "drop")

# UI
ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: white;
      position: relative;
    }
    .background-text {
      position: absolute;
      top: 40%;
      left: 40%;
      transform: translate(-50%, -50%);
      font-size: 50px;
      color: red;
      z-index: -1;
      pointer-events: none;
    }
  ")),
  
  titlePanel("Shiny Dashboard"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 radioButtons(inputId = "cId", label = "Course ID:",
                              choices = list("0HV30-2024" = 27886, "0HV60-2024" = 28301, "0HV90-2024" = 27893, "0HV100-2024" = 27974),
                              selected = 28301)
    ),
    mainPanel(
      fluidRow(
        column(width = 12, textOutput("selectedUserId")),
        column(width = 6, plotlyOutput("pieChart")),
        column(width = 6, uiOutput("onlineTime"))
      ),
      fluidRow(
        column(width = 6, plotlyOutput("stackedBarChart")),
        column(width = 6, uiOutput("quizGrades"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: course_id
  course_id <- reactive({ input$cId })
  
  # Reactive: get and cache user_id when course changes
  observeEvent(course_id(), {
    uid <- getUserData(course_id())
    cachedUserData(uid)
    print(paste("New course selected:", course_id(), "Random user selected:", uid))
  })
  
  # Reactive: user_id
  user_id <- reactive({ cachedUserData() })
  
  # Output: selected user welcome
  output$selectedUserId <- renderText({
    paste("Welcome", user_id(), "!")
  })
  
  # Output: online time summary
  output$onlineTime <- renderUI({
    course_id_value <- course_id()
    user_id_value <- user_id()
    
    result4 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_enrollments")) %>%
      filter(course_id == course_id_value, user_id == user_id_value) %>%
      select(total_active_seconds) %>%
      head(1) %>%
      collect()
    hours_online <- round(as.numeric(result4$total_active_seconds) / 3600, 0)
    
    result6 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_enrollments")) %>%
      filter(course_id == course_id_value) %>%
      summarise(avg_time = mean(total_active_seconds)) %>%
      collect()
    avg_hours <- round(as.numeric(result6$avg_time) / 3600, 0)
    
    message <- if (hours_online > avg_hours) {
      "<strong>Great job!</strong> You've spent more time than the average student."
    } else if (hours_online == avg_hours) {
      "<strong>You're on track!</strong>"
    } else {
      "<strong>Keep going!</strong> You're behind the average."
    }
    
    HTML(paste0("<div style='background-color: #88ff88; padding: 10px;'>",
                "You have been active for <strong>", hours_online, " hours</strong>.<br>",
                "Average is <strong>", avg_hours, " hours</strong>.<br><br>",
                message, "</div>"))
  })
  
  # Output: pie chart of quiz progression
  output$pieChart <- renderPlotly({
    user_id_value <- user_id()
    course_id_value <- course_id()
    
    result1 <- quiz_progression %>%
      filter(user_id == user_id_value, course_id == course_id_value) %>%
      select(nr_submissions, nr_quizzes) %>%
      head(1) %>%
      collect()
    
    if (nrow(result1) == 0) {
      # Return a plotly message (make sure you handle non-plot output separately)
      return(NULL)  # Return NULL to show no plot
    }
    
    done <- result1$nr_submissions
    todo <- result1$nr_quizzes - done
    
    plot_ly(
      labels = c("Completed", "To Do"),
      values = c(done, todo),
      type = "pie",
      marker = list(colors = c("green", "red"))
    ) %>% layout(title = "Quiz Progress")
  })
  
  # Output: message when no submissions are present
  output$noDataMessage <- renderText({
    user_id_value <- user_id()
    course_id_value <- course_id()
    
    result1 <- quiz_progression %>%
      filter(user_id == user_id_value, course_id == course_id_value) %>%
      select(nr_submissions, nr_quizzes) %>%
      head(1) %>%
      collect()
    
    if (nrow(result1) == 0) {
      return("<strong>No quiz submissions yet.</strong>")  # Display message if no data
    }
    
    return(NULL)  # Return NULL if there is data, so the pie chart can display
  })
  
  
  # Output: quiz grades
  output$quizGrades <- renderUI({
    course_id_value <- course_id()
    user_id_value <- user_id()
    
    result_quiz <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
      left_join(tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")), by = c("id" = "quiz_id")) %>%
      filter(workflow_state_x == 'available', user_id == user_id_value, course_id_x == course_id_value) %>%
      select(title = quiz_title, score = score_anonymous, max_score = points_possible) %>%
      collect()
    
    if (nrow(result_quiz) == 0) {
      return(HTML("<strong>No quiz submissions yet.</strong>"))
    }
    
    quiz_html <- "<strong>Quiz Grades:</strong><br><table style='width:100%;'><tr><th>Quiz</th><th>Score</th><th>%</th></tr>"
    
    for (i in 1:nrow(result_quiz)) {
      if (is.na(result_quiz$score[i])) {
        quiz_html <- paste0(quiz_html, "<tr><td>", result_quiz$title[i], "</td><td>Not submitted</td><td>-</td></tr>")
      } else {
        percent <- round(result_quiz$score[i] / result_quiz$max_score[i] * 100, 1)
        quiz_html <- paste0(quiz_html, "<tr><td>", result_quiz$title[i], "</td><td>", result_quiz$score[i], "</td><td>", percent, "%</td></tr>")
      }
    }
    
    quiz_html <- paste0(quiz_html, "</table>")
    HTML(quiz_html)
  })
  # Render the stacked bar chart
  output$stackedBarChart <- renderPlotly({
  user_id_value <- user_id()  # Get the current user_id reactively
  course_id_value <- course_id()  # Get the current course_id reactively
  result5 <- weblogs_chart %>%
    filter(course_id == course_id_value & user_id == user_id_value) %>%
    arrange(date) %>%
    collect()
  
  result5$date <- as.Date(result5$date, format = "%Y-%m-%d")
  
  data_wide <- tidyr::pivot_wider(result5, id_cols = date, names_from = item, values_from = count, values_fill = list(count = 0))
  plot_data <- tidyr::pivot_longer(data_wide, cols = -date, names_to = "item", values_to = "count")
  
  plot_data$count <- as.numeric(plot_data$count)
  
  q <- plot_ly(data = plot_data, x = ~date, y = ~count, color = ~item, colors = c("#003f5c","#374c80","#7a5195","#bc5090","#ef5675","#ff764a", "#ffa600"), type = 'bar', source = "stackedBarChart") %>%
    layout(barmode = 'stack', title = "User Activity by Item Type", xaxis = list(title = "", tickformat = "%b %d"), yaxis = list(title = "Count"))
  q
})
}

# Run the app
shinyApp(ui, server)
