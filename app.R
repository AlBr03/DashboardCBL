library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(bslib)

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

ui <- page_fillable(
  "Thinking and Deciding 0HV60 Dashboard", 
  layout_column_wrap(
    width = 1/2,
    card(card_header("Card 1: User Greeting"),
         textOutput("selectedUserId")),
    card(card_header("Card 2: Selected Course ID"),
         textOutput("display")),
    card(card_header("Card 3: piechart quiz progression"),
         plotlyOutput("pieChart", height = "400px")),
    card(card_header("Card 4: avatar"),
         div(tags$img(src = "https://i.kym-cdn.com/photos/images/original/002/897/911/6ff",
                  height = "200px", width = "auto"),
         tags$p("Your personal avatar is 'just a chill guy'")))
  )
)

server <- function(input, output, session) {
  course_id <- 28301 #locked on thinking and deciding 0HV60
  
  observe({
    isolate({
      user_id <- getUserData("28301")  # Pass the fixed Course ID
      cachedUserData(user_id)          # Cache the user ID
    })
  })
  
  output$display <- renderText({
    paste("Current Course ID:", course_id)
  })
  
  # Output: selected user welcome
  output$selectedUserId <- renderText({
    # Reactive: user_id
    user_id <- reactive({ cachedUserData() })
    if (!is.null(user_id)){
      paste("Welcome", user_id(), "!")
    } else {
      "Welcome, Guest!"
    }
  })
  
  # Output: pie chart of quiz progression
  output$pieChart <- renderPlotly({
    user_id_value <- cachedUserData()
    course_id_value <- course_id
    
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
  
}

shinyApp(ui = ui, server = server)
