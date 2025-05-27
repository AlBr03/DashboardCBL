# Load libraries
library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(bslib)

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

# Connect to database (assumes sc is already set in connect.R)
quizzes <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
  rename(quiz_id = id)

submissions <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions"))

# Filter valid quizzes: available, due date, and submitted
valid_quizzes <- quizzes %>%
  filter(workflow_state == "available", !is.na(due_at)) %>%
  left_join(submissions, by = c("assignment_id" = "assignment_id", "course_id" = "course_id")) %>%
  distinct(quiz_id, course_id, user_id, title, due_at, assignment_id, submitted_at_anonymous)

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
  theme = bs_theme(bootswatch = "minty", base_font = font_google("Nunito")),
  titlePanel("🎓 Thinking and Deciding Dashboard"),
  fluidRow(
    column(6,
           card(
             card_header("📝 To-Do List (Quizzes with Due Dates)"),
             uiOutput("todo")
           )
    ),
    column(6,
           card(
             card_header("📊 Quiz Completion Progress"),
             plotlyOutput("pieChart", height = "300px")
           )
    )
  ),
  fluidRow(
    column(6,
           card(
             card_header("🏆 Achievement Badge"),
             uiOutput("badge")
           )
    ),
    column(6,
           card(
             card_header("📅 Time Estimate (Mock)"),
             textOutput("selectedUserId"),
             HTML("<p>🔜 Predictive features coming in Sprint 3!</p>")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  course_id <- 28301 #locked on thinking and deciding 0HV60
  
  observe({
    isolate({
      user_id <- getUserData("28301")  # Pass the fixed Course ID
      cachedUserData(user_id)          # Cache the user ID
    })
  })
  
  observe({
    sampled <- valid_quizzes %>% filter(course_id == !!course_id) %>% collect()
    if (nrow(sampled) > 0) user_id(sample(sampled$user_id, 1))
  })
  
  # Output: selected user welcome
  output$selectedUserId <- renderText({
      paste("Welcome", cachedUserData(), "!")
  })
  
  output$todo <- renderUI({
    uid <- cachedUserData()
    if (is.null(uid)) return("Loading...")
    
    data <- valid_quizzes %>%
      filter(course_id == !!course_id, user_id == uid) %>%
      collect()
    
    data$due_at <- as.POSIXct(data$due_at)
    data <- data[order(data$due_at), ]
    
    if (!exists("checkbox_states", envir = .GlobalEnv)) {
      checkbox_states <<- reactiveValues()
    }
    
    data <- data %>% mutate(
      aid = as.character(assignment_id),
      is_submitted = !is.na(submitted_at_anonymous)
    )
    
    completed <- c()
    pending <- c()
    
    for (i in 1:nrow(data)) {
      aid <- data$aid[i]
      title <- data$title[i]
      due <- data$due_at[i]
      
      if (is.null(checkbox_states[[aid]])) {
        checkbox_states[[aid]] <- data$is_submitted[i]
      }
      
      checked <- checkbox_states[[aid]]
      label <- if (checked) paste0("<span style='text-decoration: line-through;'>", title, "</span>") else title
      block <- fluidRow(
        column(1, checkboxInput(paste0("check_", aid), NULL, value = checked)),
        column(11, HTML(paste0("<div>", label, " (Due: ", due, ")</div>")))
      )
      
      if (checked) completed <- append(completed, list(block))
      else pending <- append(pending, list(block))
    }
    
    tagList(c(pending, completed))
  })
  
  observe({
    isolate({
      if (!exists("checkbox_states", envir = .GlobalEnv)) return()
      for (name in names(checkbox_states)) {
        input_id <- paste0("check_", name)
        if (!is.null(input[[input_id]])) {
          checkbox_states[[name]] <- input[[input_id]]
        }
      }
    })
  })
  
  output$pieChart <- renderPlotly({
    uid <- cachedUserData()
    if (is.null(uid)) return(NULL)
    
    df <- valid_quizzes %>%
      filter(course_id == !!course_id, user_id == uid) %>%
      collect()
    
    completed <- df %>% filter(!is.na(submitted_at_anonymous)) %>% nrow()
    total <- df %>% nrow()
    remaining <- total - completed
    
    plot_ly(
      labels = c("Completed", "Remaining"),
      values = c(completed, remaining),
      type = "pie",
      marker = list(colors = c("green", "red"))
    ) %>% layout(title = "Your Quiz Progress")
  })
  
  output$badge <- renderUI({
    uid <- cachedUserData()
    if (is.null(uid)) return("Loading...")
    
    df <- valid_quizzes %>%
      filter(course_id == !!course_id, user_id == uid) %>%
      collect()
    
    all_done <- all(!is.na(df$submitted_at_anonymous))
    
    if (all_done) {
      badge_url <- "https://img.icons8.com/emoji/96/000000/star-emoji.png"
      HTML(paste0("<div style='text-align:center;'><img src='", badge_url, "' height='80'><br><strong>Quiz Star!</strong><br>All quizzes done! 🎉</div>"))
    } else {
      NULL
    }
  })
}

# Run
shinyApp(ui, server)