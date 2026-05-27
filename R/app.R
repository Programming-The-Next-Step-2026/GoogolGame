#' Run the Googol Game app
#'
#' @details Launches an interactive Shiny app for the Googol Game. The app has
#'   three stages: setup (choose a random or manual sequence), game (reveal
#'   cards one at a time and pick the highest), and result (win or loss message
#'   with an option to play again).
#' @examples
#' if (interactive()) run_app()
#' @import shiny
#' @export
run_app <- function() {

  # The entire UI is driven by a single page switcher in the server.
  # output$page renders setup, game, or result depending on game state.
  ui <- fluidPage(
    titlePanel("The Googol Game"),
    uiOutput("page")
  )

  server <- function(input, output, session) {

    # Tracks how many number rows the player has added in manual mode.
    # Starts at 1 so there is always at least one row visible.
    n_rows <- reactiveVal(1)

    # Holds the finalized game sequence once the player clicks Start.
    # NULL until Start is clicked, which signals the game has not yet begun.
    sequence <- reactiveVal(NULL)

    # Tracks which card is currently being revealed. Resets to 1 on Start.
    current_index <- reactiveVal(1)

    # Stores the value the player picked, NULL until they pick
    picked <- reactiveVal(NULL)

    # Switch between setup, game, and result pages based on game state.
    # sequence() == NULL: setup; picked() == NULL: game; otherwise: result.
    output$page <- renderUI({
      if (is.null(sequence())) {
        uiOutput("setup")
      } else if (is.null(picked())) {
        tagList(uiOutput("cards"), uiOutput("game_buttons"))
      } else {
        uiOutput("result")
      }
    })

    # --- Setup ---

    # Append a new row each time the player clicks "Add number"
    observeEvent(input$add_row, {
      n_rows(n_rows() + 1)
    })

    # Render the setup page with instructions, sequence type selector, and inputs
    output$setup <- renderUI({
      tagList(
        p("Choose how to generate the sequence of numbers (i.e. Random or Manual)."),
        tags$ul(
          tags$li("Random: the app generates a sequence of random numbers for you.
                   Choose how many cards you want."),
          tags$li("Manual: enter your own numbers one at a time. Use the multiplier
                   dropdown to enter large numbers (e.g. 2 x Googol). You need at
                   least 2 numbers.")
        ),
        # Let the player choose between a randomly generated or manually entered sequence
        radioButtons("mode", "Sequence type",
          choices = c("Random" = "random", "Manual" = "manual")
        ),
        # Show number-of-cards input only when random mode is selected
        conditionalPanel("input.mode == 'random'",
          numericInput("n_cards", "Number of cards", value = 3, min = 2)
        ),
        # Show manual entry controls only when manual mode is selected.
        # Each number gets its own row with a base value and multiplier dropdown.
        # The player clicks "Add number" to append a new row.
        conditionalPanel("input.mode == 'manual'",
          uiOutput("manual_inputs"),
          actionButton("add_row", "Add number")
        ),
        actionButton("start", "Start game")
      )
    })

    # Render one numericInput + selectInput pair per row.
    # Input IDs are indexed (e.g. number_1, multiplier_1) so each row
    # can be read independently when building the sequence.
    # Existing input values are read with isolate() to preserve what the
    # player entered when a new row is added.
    output$manual_inputs <- renderUI({
      lapply(seq_len(n_rows()), function(i) {
        num_val <- isolate(input[[paste0("number_", i)]])
        mul_val <- isolate(input[[paste0("multiplier_", i)]])
        fluidRow(
          column(6, numericInput(paste0("number_", i), paste("Number", i),
            value = if (is.null(num_val)) 0 else num_val, min = 0)),
          column(6, selectInput(paste0("multiplier_", i), "Multiplier",
            choices = c("None" = 1, "Million" = 1e6, "Billion" = 1e9,
                        "Trillion" = 1e12, "Googol" = 1e100),
            selected = if (is.null(mul_val)) 1 else mul_val))
        )
      })
    })

    # Build the sequence using game_logic functions when the player clicks Start.
    # In manual mode, read each row's number and multiplier, multiply them,
    # then pass the resulting vector to validate_manual_sequence() for validation.
    # Resets current_index and picked so a new game always starts fresh.
    observeEvent(input$start, {
      current_index(1)
      picked(NULL)
      if (input$mode == "random") {
        sequence(generate_sequence(input$n_cards))
      } else {
        vals <- sapply(seq_len(n_rows()), function(i) {
          input[[paste0("number_", i)]] *
            as.numeric(input[[paste0("multiplier_", i)]])
        })
        # Catch validation errors from validate_manual_sequence() and show a
        # notification instead of crashing the app
        tryCatch(
          sequence(validate_manual_sequence(vals)),
          error = function(e) showNotification(conditionMessage(e), type = "error")
        )
      }
    })

    # --- Game ---

    # Render one card per number in the sequence once the game has started.
    # get_card_states() assigns each card a state ("passed", "current", "hidden")
    # which determines what the player sees: passed and current cards show their
    # number, hidden cards show a placeholder.
    output$cards <- renderUI({
      req(sequence())
      states <- get_card_states(sequence(), current_index())
      lapply(seq_along(sequence()), function(i) {
        if (states[i] == "hidden") {
          div("?")
        } else {
          div(sequence()[i])
        }
      })
    })

    # Render Pick and Next buttons once the game has started.
    # Next is disabled on the last card, forcing the player to pick.
    output$game_buttons <- renderUI({
      req(sequence())
      is_last <- current_index() == length(sequence())
      tagList(
        actionButton("pick", "Pick this number"),
        if (!is_last) actionButton("next_card", "Next")
      )
    })

    # Advance to the next card when the player clicks Next
    observeEvent(input$next_card, {
      current_index(current_index() + 1)
    })

    # --- Result ---

    # Record the current number as the player's pick when they click Pick
    observeEvent(input$pick, {
      picked(sequence()[current_index()])
    })

    # Show win or loss message after the player picks, with a restart button.
    # is_winner() compares the picked value against the full sequence.
    output$result <- renderUI({
      req(picked())
      tagList(
        if (is_winner(picked(), sequence())) {
          p("You won! That was the highest number.")
        } else {
          p(paste("You lost. The highest number was", max(sequence())))
        },
        actionButton("restart", "Play again")
      )
    })

    # Reset all game state when the player clicks Play again, returning to setup
    observeEvent(input$restart, {
      sequence(NULL)
      picked(NULL)
      current_index(1)
      n_rows(1)
    })
  }

  # Launch the Shiny app
  shinyApp(ui, server)
}
