#' Zodelis (Lithuanian version of Wordle)
#'
#' A lithuanian version of the famous New York Times game 'Wordle'.
#' A shiny app.
#' @docType package
#' @name zodelis

library(shiny)

#' Create feedback tiles.
#'
#' Generates colored feedback tiles to indicate whether the letters are
#' correct, present, or wrong.
#'
#' @param guess A 5-character string representing the user's guessed word.
#' @param target A 5-character string representing the target word.
#' @return A Shiny object containing the feedback tiles.
#' @examples
#' create_feedback_tiles("labas", "batas")
#'
#' @export
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

#' UI definition for Zodelis.
#'
#' Shiny UI layout including input, action button and feedback.
#'
#' @examples
#' shinyApp(ui, server)
#'
#' @export
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
  titlePanel("Žodelis"),
  textInput("guess", "Įveskite 5 raidžių žodį:", value = ""),
  actionButton("submit", "Spėti"),
  br(), br(),
  uiOutput("feedback_ui"),
  textOutput("status")
)


#' Server logic for Žodelis.
#'
#' Processes user guesses, updates tile feedback and shows notifications.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return None
#' @examples
#' shinyApp(ui, server)
#'
#' @export
server <- function(input, output, session) {
  values <- reactiveValues(
    guesses = list(),
    solved = FALSE
  )
  # Pick a random target word
  data("words_lt", package = "zodelis")
  target_word <- sample(words_lt, 1)
  observeEvent(input$submit, {
    req(input$guess)
    guess <- tolower(input$guess)

    # Check if the word is already solved or if there are no more guesses
    if (values$solved || length(values$guesses) >= 6) {
      output$status <- renderText(paste0("Turite tik šešis spėjimus. Teisingas žodis buvo: ", target_word))
      return()
    }

    # Check word length and validity
    if (nchar(guess) != 5) {
      output$status <- renderText("Žodis turi būti iš 5 raidžių.")
      return()
    }

    if (length(values$guesses) > 5) {
      output$status <- renderText("Turite tik šešis spėjimus. Ate!")
      return()
    }

    if (!(guess %in% words_lt)) {
      output$status <- renderText("Tokio žodžio nėra. Bandykite kitą.")
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

    output$status <- renderText(
      if (values$solved) {
        "Teisingai!"
      } else if (length(values$guesses) >= 6) {
        paste0("Turite tik šešis spėjimus. Teisingas žodis buvo: ", target_word)
      } else {
        "Bandykite dar kartą."
      }
    )
  })
}

shinyApp(ui, server)
