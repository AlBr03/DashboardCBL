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
      list(label = "Collect badges to earn cool accessories for your avatar"),
      list(img = "Images/1.png", label = "Achieve this badge by scoring more than 20 points over the graded quizzes!"),
      list(img = "Images/2.png", label = "Achieve this badge by being more active on canvas than 65% of students"),
      list(img = "Images/3.png", label = "Achieve this badge by completing at least one of the workshop preparations (more than) a day before the deadline!"),
      list(img = "Images/4.png", label = "Achieve this badge by completing all the practice quizzes!"),
      list(img = "Images/5.png", label = "Achieve this badge by starting a conversation!")
    )
    
    tagList(
      lapply(badges, function(badge) {
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 1px;",
          tags$img(src = badge$img, height = "50px", style = "margin-right: 20px;"),
          tags$span(badge$label, style = "font-weight: bold; font-size: 14px;")
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
  
  
  # helper for avatar:
  compose_avatar <- function(b1, b2, b3, b4, b5, out_path) {
    # read base at 250×250
    img <- image_read_svg("www/avatars/avatar.svg", width = 250, height = 250)
    
    if (b1) img <- image_composite(img,
                                   image_read_svg("www/avatars/avatar_1.svg", width=250, height=250),
                                   offset = "+0+0")
    if (b2) img <- image_composite(img,
                                   image_read_svg("www/avatars/avatar_2.svg", width=250, height=250),
                                   offset = "+0+0")
    if (b3) img <- image_composite(img,
                                   image_read_svg("www/avatars/avatar_3.svg", width=250, height=250),
                                   offset = "+0+0")
    if (b4) img <- image_composite(img,
                                   image_read_svg("www/avatars/avatar_4.svg", width=250, height=250),
                                   offset = "+0+0")
    if (b5) img <- image_composite(img,
                                   image_read_svg("www/avatars/avatar_5.svg", width=250, height=250),
                                   offset = "+0+0")
    
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    image_write(img, out_path)
    out_path
  }
  
  
  
  
  
  
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

  
  # line graph of grades
  output$lineChart <- renderPlotly({
    user_id_value <- cachedUserData()
    course_id_value <- "28301"
    
    if (is.null(user_id_value)) return(NULL)
    
    # Step 1: Get all user scores for the course to compute min/max per quiz
    all_scores <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(course_id == course_id_value) %>%
      filter(!quiz_id %in% c(26608, 26581)) %>%
      select(quiz_id, quiz_title, score_anonymous) %>%
      collect()
    
    quiz_ranges <- all_scores %>%
      group_by(quiz_id, quiz_title) %>%
      summarise(
        quiz_min = min(score_anonymous, na.rm = TRUE),
        quiz_max = max(score_anonymous, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Step 2: Get scores for the selected user
    user_scores <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(user_id == user_id_value, course_id == course_id_value) %>%
      filter(!quiz_id %in% c(26608, 26581)) %>%
      select(quiz_id, quiz_title, score_anonymous, finished_at_anonymous) %>%
      left_join(
        tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
          select(title, due_at),
        by = c("quiz_title" = "title")
      ) %>%
      collect() %>%
      group_by(quiz_id, quiz_title, due_at) %>%
      summarise(
        score_anonymous = max(score_anonymous, na.rm = TRUE),
        finished_at = max(finished_at_anonymous, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(user_scores) == 0 || all(is.na(user_scores$score_anonymous))) {
      return(NULL)
    }
    
    # Step 3: Join quiz min/max and normalize per quiz
    user_scores <- user_scores %>%
      left_join(quiz_ranges, by = c("quiz_id", "quiz_title")) %>%
      mutate(
        normalized_score = case_when(
          is.na(score_anonymous) ~ NA_real_,
          quiz_max == quiz_min ~ 10,  # Avoid division by zero
          TRUE ~ round((score_anonymous - quiz_min) / (quiz_max - quiz_min) * 10, 1)
        )
      )
    
    # Step 4: Arrange and prepare for plotting
    user_scores <- user_scores %>%
      arrange(finished_at) %>%
      mutate(quiz_title = factor(quiz_title, levels = unique(quiz_title)))
    
    # Step 5: Plot
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
        yaxis = list(title = "Normalized Score (0–10)", rangemode = "tozero"),
        xaxis = list(title = "Quiz Title", tickangle = -45)
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
    course_id_value <- 28301
    user_id_value <- cachedUserData()
    assignment_ids <- c(127709, 127727)
    
    if (is.null(user_id_value)) return(FALSE)
    
    # Step 1: Collect all submissions for these assignments
    all_submissions <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_submissions")) %>%
      filter(course_id == course_id_value, user_id == user_id_value, assignment_id %in% assignment_ids) %>%
      select(assignment_id, submitted_at_anonymous) %>%
      collect()
    
    # Step 2: Collect all assignment due dates
    all_due_dates <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_assignments")) %>%
      filter(course_id == course_id_value, id %in% assignment_ids) %>%
      select(id, due_at) %>%
      collect()
    
    # Defensive: if either is empty, no early submission possible
    if (nrow(all_submissions) == 0 || nrow(all_due_dates) == 0) return(FALSE)
    
    # Step 3: Merge submissions and due dates by assignment_id
    merged <- base::merge(
      all_submissions,
      all_due_dates,
      by.x = "assignment_id",
      by.y = "id",
      all.x = TRUE
    )
    
    
    # Step 4: Check if any submission is exactly 1 day before due date
    merged$submitted_date <- as.Date(merged$submitted_at_anonymous)
    merged$due_date <- as.Date(merged$due_at)
    
    # Count how many submissions were 1 day early
    early_count <- sum((merged$due_date - merged$submitted_date) == 1, na.rm = TRUE)
    
    return(early_count >= 1)
  })
  
  
  
  
  
  #draft for quiz master badge
  scored_high <- reactive({
    course_id_value <- 28301
    user_id_value <- cachedUserData()
    
    if (is.null(user_id_value)) return(FALSE)
    
    # Quizzes of interest
    score_ids <- c(26615, 26610, 26617, 27282, 27283, 27284)
    
    # Step 1: Pull all submissions for the user in this course
    user_scores <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quiz_submissions")) %>%
      filter(user_id == user_id_value, course_id == course_id_value) %>%
      filter(quiz_id %in% score_ids) %>%
      select(quiz_id, quiz_title, score_anonymous, finished_at_anonymous) %>%
      left_join(
        tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_quizzes")) %>%
          select(title, due_at),
        by = c("quiz_title" = "title")
      ) %>%
      collect() %>%
      group_by(quiz_id, quiz_title, due_at) %>%
      summarise(
        score_anonymous = max(score_anonymous, na.rm = TRUE),
        finished_at = max(finished_at_anonymous, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Step 2: Defensive check
    if (nrow(user_scores) == 0 || all(is.na(user_scores$score_anonymous))) {
      return(FALSE)
    }
    
    # Step 3: Sum scores across selected quizzes
    total_score <- sum(user_scores$score_anonymous, na.rm = TRUE)
    #print(paste("Total score across selected quizzes:", total_score))
    
    return(total_score >= 20)
  })
  
  
  
  
  
  
  # Draft for  engagement_badge function
  engagement_badge <- reactive({
    user_id_value <- cachedUserData()
    
    if (is.null(user_id_value)) {
      print("DEBUG: user_id_value is NULL, returning FALSE")
      return(FALSE)
    }
    
    weblogs_tbl <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_enrollments"))
    
    user_totals <- weblogs_tbl %>%
      filter(course_id == 28301) %>%
      group_by(user_id) %>%
      summarise(total_active = sum(total_active_seconds, na.rm = TRUE)) %>%
      collect()
    
    if (nrow(user_totals) == 0) {
      #print("DEBUG: user_totals has zero rows, returning FALSE")
      return(FALSE)
    }
    
    threshold <- quantile(user_totals$total_active, 0.65, na.rm = TRUE)
    #print(paste0("DEBUG: threshold (65th percentile) = ", threshold))
    
    this_user_total <- user_totals %>%
      filter(user_id == user_id_value) %>%
      pull(total_active)
    #print(paste0("DEBUG: this_user_total = ", ifelse(length(this_user_total)==0, "0", this_user_total)))
    
    if (length(this_user_total) == 0) {
      this_user_total <- 0
    }
    
    result <- this_user_total >= threshold
    #print(paste0("DEBUG: result = ", result))
    
    return(result)
  })
  
  
  # Draft for conversation_badge function
  conversation_badge <- reactive({
    course_id_value <- 28301
    user_id_value <- cachedUserData()
    
    if (is.null(user_id_value)) {
      print("User ID is NULL — returning FALSE.")
      return(FALSE)
    }
    
    # Step 1: Query the database
    user_posts <- tbl(sc, in_schema("sandbox_la_conijn_CBL", "silver_canvas_discussion_entries")) %>%
      filter(course_id == course_id_value, user_id == user_id_value) %>%
      summarise(n_posts = n()) %>%
      collect()
    
    #print(user_posts)
    
    # Step 2: Defensive check
    if (nrow(user_posts) == 0 || is.na(user_posts$n_posts)) {
      print("No posts found or n_posts is NA — returning FALSE.")
      return(FALSE)
    }
    
    # Step 3: Evaluate result
    result <- user_posts$n_posts > 0
    #print(paste("Number of posts:", user_posts$n_posts, "=> Badge earned?", result))
    
    return(result)
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
    
    tags$ul(style = "font-size: 14px", quiz_list)
  })
  
  #Avatar section functionalities
  output$avatar <- renderUI({
    req(cachedUserData())
    uid <- cachedUserData()
    
    # compute badge flags
    b1 <- scored_high()
    b2 <- engagement_badge()
    b3 <- early_bird()
    b4 <- quiz_star()
    b5 <- conversation_badge()
    
    # compose the avatar PNG
    out_png <- compose_avatar(
      b1, b2, b3, b4, b5,
      file.path("www/avatars", paste0("avatar_user_", uid, ".png"))
    )
    
    # return a DIV containing both the <img> and the badges row
    tags$div(
      style = "display: flex; flex-direction: column; align-items: center;",
      
      # the avatar image
      tags$img(
        src    = sub("^www/", "", out_png),
        height = "250px",
        width  = "250px",
        style  = "margin-bottom: 15px;"
      ),
      
      # the badges underneath
      tags$div(
        style = "display: flex; gap: 20px; align-items: center;",
        render_badge("www/Images/1.png", show_active = b1, badge_name = "quiz"),
        render_badge("www/Images/2.png", show_active = b2, badge_name = "engagement"),
        render_badge("www/Images/3.png", show_active = b3, badge_name = "earlybird"),
        render_badge("www/Images/4.png", show_active = b4, badge_name = "practice"),
        render_badge("www/Images/5.png", show_active = b5, badge_name = "conversation")
      )
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
