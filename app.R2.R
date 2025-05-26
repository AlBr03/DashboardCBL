# Load libraries
library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(bslib)

# Connect to database (assumes sc is already set in connect.R)
quizzes <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
  rename(quiz_id = id)

submissions <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions"))

# Filter valid quizzes: available, due date, and submitted
valid_quizzes <- quizzes %>%
  filter(workflow_state == "available", !is.na(due_at)) %>%
  left_join(submissions, by = c("assignment_id" = "assignment_id", "course_id" = "course_id")) %>%
  distinct(quiz_id, course_id, user_id, title, due_at, assignment_id, submitted_at_anonymous)

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
             HTML("<p>🔜 Predictive features coming in Sprint 3!</p>")
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  course_id <- 28301
  user_id <- reactiveVal(NULL)
  
  observe({
    sampled <- valid_quizzes %>% filter(course_id == !!course_id) %>% collect()
    if (nrow(sampled) > 0) user_id(sample(sampled$user_id, 1))
  })
  
  output$todo <- renderUI({
    uid <- user_id()
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
    uid <- user_id()
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
    uid <- user_id()
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