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

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty", base_font = font_google("Nunito")),
  titlePanel("🎓 Thinking and Deciding Dashboard"),
  fluidRow(
    column(7,
           card(
             card_header("📝 To-Do List (Quizzes with Due Dates)"),
             uiOutput("todoV2")
           )
    ),
    column(5,
           card(
             card_header("👤 Personal Avatar"),
             tags$img(src = "avatar.png", height = "120px"),
             p("Earn badges to unlock accessories!"),
             tags$div(
               tags$img(src = "badge1.png", height = "30px"),
               tags$img(src = "badge2.png", height = "30px"),
               tags$img(src = "badge3.png", height = "30px"),
               tags$img(src = "badge4.png", height = "30px"),
               tags$img(src = "badge5.png", height = "30px")
             )
           )
    )
  ),
  fluidRow(
    column(4,
           card(
             card_header("📊 Quiz Completion Progress"),
             plotlyOutput("pieChart", height = "300px")
           )
    ),
    column(4,
           card(
             card_header("🏆 Achievement Badge"),
             uiOutput("badge")
           )
    ),
    column(4,
           card(
             card_header("📊 Grade overview"),
             plotlyOutput("lineChart", height = "300px")
    )
  )
))


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
  
  # line graph of grades ##TODO: sort by due date and normalize to a grade from 0-10
  output$lineChart <- renderPlotly({
    user_id_value <- cachedUserData()
    course_id_value <- "28301"
    
    if (is.null(user_id_value)) return(NULL)
    
    # Fetch scores, excluding specific quiz IDs
    user_scores <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(user_id == user_id_value, course_id == course_id_value) %>%
      filter(!quiz_id %in% c(26608, 26581)) %>%
      select(quiz_id, quiz_title, score_anonymous) %>%
      collect() %>%
      group_by(quiz_title) %>%
      summarise(score_anonymous = max(score_anonymous, na.rm = TRUE)) %>%
      arrange(quiz_title)
    
    if (nrow(user_scores) == 0 || all(is.na(user_scores$score_anonymous))) {
      return(NULL)
    }
    
    user_scores$score_anonymous <- as.numeric(user_scores$score_anonymous)
    
    # Line chart
    plot_ly(
      data = user_scores,
      x = ~quiz_title,
      y = ~score_anonymous,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = "linear"),
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Quiz Scores Over Time",
        xaxis = list(title = "Quiz"),
        yaxis = list(title = "Score", rangemode = "tozero")
      )
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
  
  #badges
  output$badge <- renderUI({
    user_id_value <- cachedUserData()
  
    
    #badge for completing all quizzes
    result3 <- quiz_progression %>%
      filter(user_id == user_id_value, course_id == !!course_id) %>%
      select(nr_submissions, nr_quizzes) %>%
      head(1) %>%
      collect()
    
    if (nrow(result3) == 0) {
      return(NULL)  # Return NULL to show no plot
    }
    
    done <- result3$nr_submissions
    todo <- result3$nr_quizzes - done
    
    if (todo == 0) {
      quiz_master = TRUE
      badge_url <- "https://img.icons8.com/emoji/96/000000/star-emoji.png"
      HTML(paste0("<div style='text-align:center;'><img src='", badge_url, "' height='80'><br><strong>Quiz Star!</strong><br>All quizzes done! 🎉</div>"))
    } else {
      NULL
    }
  })
  
  
  # Draft for early bird badge (currently only for assignment 1 but easy to extend)
  output$submitTime <- renderUI({
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
    
    result4 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value, assignment_id == 127709) %>%
      select(submitted_at_anonymous) %>%
      head(1) %>%
      collect()
    submitted_time <- if(nrow(result4) > 0) result4$submitted_at_anonymous else NA
    
    result5 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
      filter(course_id == course_id_value, assignment_id == 127709) %>%
      select(due_at) %>%
      head(1) %>%
      collect()
    due_time <- if(nrow(result5) > 0) result5$due_at else NA
    
    if (!is.na(submitted_time) && !is.na(due_time)){
      finished_day_before <- substr(due_time, 1, 10) == substr(submitted_time, 1, 10)
      badge <- if(finished_day_before){
        "https://files.123freevectors.com/wp-content/original/33661-sad-face-emoticon.jpg"
      } else {
        "http://pluspng.com/img-png/png-smiling-face-smiley-png-3896.png"
        early_bird = TRUE
      }} else {
        finished_day_before <- TRUE
        submitted_time <- "not handed in"
        badge <- "https://files.123freevectors.com/wp-content/original/33661-sad-face-emoticon.jpg" 
      }
    
    HTML(paste0("<div style='background-color: #88ff88; padding: 10px;'>",
                due_time, "<br>", submitted_time, "<br>", finished_day_before,
                "<img src=", badge,"
                alt='Descriptive Text' 
                style='max-width:100px; display:block; margin-bottom:10px;'>",
                "</div>"))
  })
  
  #draft for quiz master badge
  output$quizScore <- renderUI({
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
    
    result6 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value, assignment_id == 127733) %>%
      select(score_anonymous) %>%
      head(1) %>%
      collect()
    score_1 <- if (nrow(result6) > 0) result6$score_anonymous else 0
    
    result7 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value, assignment_id == 127729) %>%
      select(score_anonymous) %>%
      head(1) %>%
      collect()
    score_2 <- if (nrow(result7) > 0) result7$score_anonymous else 0
    
    result8 <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value, assignment_id == 127735) %>%
      select(score_anonymous) %>%
      head(1) %>%
      collect()
    score_3 <- if (nrow(result8) > 0) result8$score_anonymous else 0
    
    scored_high <- if(score_1 + score_2 + score_3 >= 12){
      TRUE
    } else {
      FALSE
    }
    
    badge <- if(!scored_high){
      "https://files.123freevectors.com/wp-content/original/33661-sad-face-emoticon.jpg"
    } else {
      "http://pluspng.com/img-png/png-smiling-face-smiley-png-3896.png"
    }
    
    HTML(paste0("<div style='background-color: #88ff88; padding: 10px;'>",
                score_1, "<br>", score_2, "<br>", score_3, "<br>", scored_high,
                "<img src=", badge,"
                alt='Descriptive Text' 
                style='max-width:100px; display:block; margin-bottom:10px;'>",
                "</div>"))
  })
  
  #draft for todo list
  output$todoV2 <- renderUI({
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
    
    # Get user's completed submissions
    user_submissions <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value) %>%
      select(quiz_title) %>%
      distinct() %>%
      collect()
    
    completed_titles <- user_submissions$quiz_title
    
    # Get all quizzes with due dates
    quizzes_with_due <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(course_id == course_id_value) %>%
      select(quiz_id, quiz_title) %>%
      distinct() %>%
      left_join(
        tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")),
        by = c("quiz_title" = "title")
      ) %>%
      select(quiz_id, quiz_title, due_at) %>%
      distinct(quiz_id, .keep_all = TRUE) %>%
      arrange(is.na(due_at), due_at) %>%
      collect()
    
    if (nrow(quizzes_with_due) == 0) {
      return(HTML("<p>No quizzes found.</p>"))
    }
    
    # Build the quiz list with strikethrough for completed quizzes
    quiz_list <- lapply(seq_len(nrow(quizzes_with_due)), function(i) {
      title <- quizzes_with_due$quiz_title[i]
      due <- quizzes_with_due$due_at[i]
      
      # Check if user completed this quiz (by matching title)
      completed <- title %in% completed_titles
      
      # Format due date
      due_text <- if (!is.na(due)) {
        paste(" (Due:", format(as.POSIXct(due, tz = "UTC"), "%b %d, %Y %H:%M"), ")")
      } else {
        " (No due date)"
      }
      
      # Strike through if completed
      display_title <- if (completed) tags$s(title) else title
      
      tags$li(HTML(paste0(as.character(display_title), due_text)))
    })
    
    tags$ul(quiz_list)
  })
  
  
  output$todo <- renderUI({
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
  
    quizzes_with_status <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
      filter(course_id == course_id_value, !is.na(due_at)) %>%
      select(title, due_at, assignment_id) %>%
      left_join(
        tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
          filter(user_id == user_id_value) %>%
          select(quiz_id, quiz_title),
        by = c("assignment_id" = "quiz_id")
      ) %>%
      arrange(due_at) %>%
      collect()
    
    if(!exists("checkbox_states", envir = .GlobalEnv)) {
      checkbox_states <<- reactiveValues()
    }
    
    
    # Create UI elements for each assignment
    quiz_ui_list <- lapply(seq_len(nrow(quizzes_with_status)), function(i) {
      row <- quizzes_with_status[i, ]
      aid <- as.character(row$assignment_id)
      
      # Initialize checkbox state if not yet set
      if (is.null(checkbox_states[[aid]])) {
        checkbox_states[[aid]] <- !is.na(row$assignment_title)
      }
      
      # Apply strikethrough conditionally
      title_display <- if (checkbox_states[[aid]]) {
        paste0("<span style='text-decoration: line-through;'>", row$title, "</span>")
      } else {
        row$title
      }
      
      # Create checkbox + label
      fluidRow(
        column(1,
               checkboxInput(inputId = paste0("checkbox_", aid),
                             label = NULL,
                             value = checkbox_states[[aid]])),
        column(11,
               HTML(paste0("<div>", title_display, " (Due: ", row$due_at, ")</div>"))
        )
      )
    })
    
    # Return all quiz UI components
    div(
      style = "max-height: 300px; overflow-y: auto; padding-right: 10px;",
      do.call(tagList, quiz_ui_list)
    )
  })
  
  # Update checkbox_states when any checkbox is clicked
  observe({
    isolate({
      for (aid in names(checkbox_states)) {
        input_id <- paste0("checkbox_", aid)
        if (!is.null(input[[input_id]])) {
          checkbox_states[[aid]] <- input[[input_id]]
        }
      }
    })
  })
}

shinyApp(ui = ui, server = server)
