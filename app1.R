library(shiny)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)
library(pagedown)

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

# UI
ui <- page_fillable(
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
  "layout_column_wrap", 
  layout_column_wrap( 
    card("Card 1"), 
    card("Card 2"), 
    card("Card 3"), 
    card("Card 4"), 
    width = "300px" 
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: user_id
  user_id <- reactive({ cachedUserData() })
  # Output: selected user welcome
  output$selectedUserId <- renderText({
    paste("Welcome", user_id(), "!")
  })
}

# Run the app
shinyApp(ui, server)