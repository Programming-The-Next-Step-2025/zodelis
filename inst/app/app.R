library(shiny)

# Load Lithuanian 5-letter words
words_lt <- readRDS("data/words_lt.rds")

# Pick a random target word
target_word <- sample(words_lt, 1)

# Function to generate colored feedback tiles
create_feedback_tiles <- function(guess, target) {
  guess_letters <- strsplit(guess, "")[[1]]
  target_letters <- strsplit(target, "")[[1]]
  feedback <- rep("gray", 5)

  # Track matched letters
  used <- rep(FALSE, 5)

  # First pass: correct positions
  for (i in 1:5) {
    if (guess_letters[i] == target_letters[i]) {
      feedback[i] <- "green"
      used[i] <- TRUE
      target_letters[i] <- NA
    }
  }

  # Second pass: correct letters in wrong positions
  for (i in 1:5) {
    if (feedback[i] == "gray" && guess_letters[i] %in% target_letters) {
      idx <- which(target_letters == guess_letters[i])[1]
      if (!is.na(idx)) {
        feedback[i] <- "yellow"
        target_letters[idx] <- NA
      }
    }
  }

  # Create HTML tiles
  html <- ""
  for (i in 1:5) {
    html <- paste0(html, sprintf(
      '<div class="tile %s">%s</div>',
      feedback[i], toupper(guess_letters[i])
    ))
  }

  HTML(paste0('<div class="tile-row">', html, '</div>'))
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tile-row { display: flex; margin-bottom: 5px; }
      .tile {
        width: 40px; height: 40px;
        border: 2px solid #999;
        margin-right: 4px;
        text-align: center;
        line-height: 40px;
        font-weight: bold;
        font-size: 24px;
        text-transform: uppercase;
        color: white;
      }
      .green { background-color: #6aaa64; }
      .yellow { background-color: #c9b458; }
      .gray { background-color: #787c7e; }
    "))
  ),
  titlePanel("Lietuviškas Wordle"),
  textInput("guess", "Įveskite 5 raidžių žodį:", value = ""),
  actionButton("submit", "Spėti"),
  br(), br(),
  uiOutput("feedback_ui"),
  textOutput("status")
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(
    guesses = list(),
    solved = FALSE
  )

  observeEvent(input$submit, {
    req(input$guess)
    guess <- tolower(input$guess)

    # Check word length and validity
    if (nchar(guess) != 5) {
      output$status <- renderText("Žodis turi būti iš 5 raidžių.")
      return()
    }
    if (!(guess %in% words_lt)) {
      output$status <- renderText(paste("Nerastas toks žodis. Bandykite kitą.", target_word, sep=" "))
      return()
    }

    if (guess == target_word) {
      values$solved <- TRUE
      shiny::showNotification("Teisingas atsakymas!", type = "message")
    }

    # Store feedback
    values$guesses <- append(values$guesses, list(create_feedback_tiles(guess, target_word)))

    output$feedback_ui <- renderUI({
      tagList(values$guesses)
    })

    output$status <- renderText(if (values$solved) "Teisingai!" else paste("Bandykite dar kartą.", target_word, sep=" "))
  })
}

shinyApp(ui, server)
