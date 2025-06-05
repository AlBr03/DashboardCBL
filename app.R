library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(bslib)
library(magick)

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
             uiOutput("avatar")
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
             card_header("📊 Grade overview"),
             plotlyOutput("lineChart", height = "300px")
           )
    ),
    column(4,
           card(
             card_header("🏆 Explanation of the badges"),
             uiOutput("badge_list")
           )
    )
  )
)


server <- function(input, output, session) {
  output$badge_list <- renderUI({
    badges <- list(
      list(img = "Images/1.png", label = "Verkennen van het probleem"),
      list(img = "Images/2.png", label = "Vooruit plannen"),
      list(img = "Images/3.png", label = "Experimenteren"),
      list(img = "Images/4.png", label = "Reflecteren"),
      list(img = "Images/5.png", label = "Verdiepen")
    )
    
    tagList(
      lapply(badges, function(badge) {
        tags$div(
          style = "margin-bottom: 20px; text-align: center;",
          tags$img(src = badge$img, height = "80px", style = "margin-bottom: 10px;"),
          tags$p(badge$label, style = "margin: 0; font-weight: bold;")
        )
      })
    )
  })
  
  
  render_badge <- function(path, show_active, badge_name) {
    img <- image_read(path)
    if (!show_active) {
      img <- image_fx(img, expression = "a*0.2", channel = "alpha")
    }
    
    out_dir <- "www/badges"
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    
    out_path <- file.path(out_dir, paste0(badge_name, "_badge.png"))
    image_write(img, path = out_path)
    
    tags$img(
      src = file.path("badges", paste0(badge_name, "_badge.png")),
      style = "height: 60px; width: 60px; display: block; margin-bottom: 10px;"
    )
  }
  
  
  
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
    
    # Fetch scores and timestamps
    user_scores <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(user_id == user_id_value, course_id == course_id_value) %>%
      filter(!quiz_id %in% c(26608, 26581)) %>%
      select(quiz_id, quiz_title, score_anonymous, finished_at_anonymous) %>%
      left_join(
        tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
          select(assignment_id, due_at),
        by = c("quiz_id" = "assignment_id")
      ) %>%
      collect() %>%
      group_by(quiz_id, quiz_title, due_at, finished_at_anonymous) %>%
      summarise(score_anonymous = max(score_anonymous, na.rm = TRUE), .groups = "drop") %>%
      arrange(finished_at_anonymous)
    
    if (nrow(user_scores) == 0 || all(is.na(user_scores$score_anonymous))) {
      return(NULL)
    }
    
    # Normalize scores to 0–10
    user_scores$score_anonymous <- as.numeric(user_scores$score_anonymous)
    valid_scores <- user_scores %>% filter(!is.na(score_anonymous))
    
    min_score <- min(valid_scores$score_anonymous, na.rm = TRUE)
    max_score <- max(valid_scores$score_anonymous, na.rm = TRUE)
    
    if (min_score == max_score) {
      user_scores$normalized_score <- 10
    } else {
      user_scores$normalized_score <- round(
        (user_scores$score_anonymous - min_score) /
          (max_score - min_score) * 10, 1
      )
    }
    
    # Sort x-axis by finished_at_anonymous
    user_scores <- user_scores %>%
      arrange(finished_at_anonymous) %>%
      mutate(quiz_title = factor(quiz_title, levels = unique(quiz_title)))
    
    # Plot
    plot_ly(
      data = user_scores,
      x = ~quiz_title,
      y = ~normalized_score,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = "linear"),
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Normalized Quiz Scores Over Time",
        yaxis = list(title = "Normalized Score (0–10)", rangemode = "tozero")
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
  
  #practice badge
  quiz_star <- reactive({
    required_ids <- c(26585, 26594, 26596, 26584, 26618)
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
    
    user_submissions <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value) %>%
      select(quiz_id) %>%
      distinct() %>%
      collect()
    
    completed_ids <- user_submissions$quiz_id
    all(required_ids %in% completed_ids)
  })
  
  
  
  
  # Draft for early bird badge
  early_bird <- reactive({
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
    assignment_ids <- c(127733, 127729, 127735, 131000, 131001, 131002)
    
    early_count <- 0
    
    for (assignment_id in assignment_ids) {
      # Get submitted time
      submitted <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions")) %>%
        filter(course_id == course_id_value, user_id == user_id_value, assignment_id == assignment_id) %>%
        select(submitted_at_anonymous) %>%
        head(1) %>%
        collect()
      
      # Get due time
      due <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
        filter(course_id == course_id_value, assignment_id == assignment_id) %>%
        select(due_at) %>%
        head(1) %>%
        collect()
      
      if (nrow(submitted) > 0 && nrow(due) > 0) {
        submitted_date <- as.Date(submitted$submitted_at_anonymous)
        due_date <- as.Date(due$due_at)
        
        if (!is.na(submitted_date) && !is.na(due_date)) {
          if (submitted_date == (due_date - 1)) {
            early_count <- early_count + 1
          }
        }
      }
    }
    
    # Return TRUE if at least 3 early submissions
    early_count >= 1
  })
  
  
  
  #draft for quiz master badge
  scored_high <- reactive({
    course_id_value <- "28301"
    user_id_value <- cachedUserData()
    
    score_ids <- c(26615, 26610, 26617, 27282, 27283, 27284)
    
    scores <- lapply(score_ids, function(quiz_id) {
      result <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
        filter(course_id == course_id_value, user_id == user_id_value, quiz_id == quiz_id) %>%
        select(score_anonymous) %>%
        head(1) %>%
        collect()
      if (nrow(result) > 0) result$score_anonymous else 0
    })
    
    sum(unlist(scores)) >= 20
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
  
  #Avatar section functionalities
  output$avatar <- renderUI({
    tags$div(
      style = "display: flex; gap: 20px; align-items: center;",
      render_badge("www/Images/1.png", show_active = scored_high(), badge_name = "quiz"),
      render_badge("www/Images/2.png", show_active = FALSE, badge_name = "engagement"),
      render_badge("www/Images/3.png", show_active = early_bird(), badge_name = "earlybird"),
      render_badge("www/Images/4.png", show_active = quiz_star(), badge_name = "practice"),
      render_badge("www/Images/5.png", show_active = FALSE, badge_name = "conversation")
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
