#' Play the Googol Game
#'
#' @details Launches an interactive Shiny app for the Googol Game. The app has
#'   three stages: setup (choose a random or manual sequence), game (reveal
#'   cards one at a time and pick the highest), and result (win or loss message
#'   with an option to play again).
#' @examples
#' if (interactive()) play_googol()
#' @import shiny
#' @export
play_googol <- function() {
  # Launch the Shiny app
  shinyApp(ui, server)
}

# --- UI ---

# The entire UI is driven by a single page switcher in the server.
# output$page renders setup, game, or result depending on game state.
ui <- fluidPage(
  # Add a title: "Googol Game"
  titlePanel(
    div(
      # Bold and center the title, with the Gs of Googol and
      # Game highlighted in blue
      HTML('<span style="color: #2c7be5; font-weight: bold;">G</span>oogol
            <span style="color: #2c7be5; font-weight: bold;">G</span>ame'),
      style = "text-align: center; font-weight: bold;"
    )
  ),
  uiOutput("page")
)

# --- Card Helper ---

# Renders a styled card div based on state.
# Hidden cards show a solid blue face-down card.
# Passed cards are greyed out to signal they are no longer selectable.
# The current card is highlighted in green to draw the player's attention.
card_div <- function(state, number = "", multiplier = "") {
  style <- switch(state,
    hidden  = "background-color: #2c7be5; color: #2c7be5;",
    passed  = "background-color: #d3d3d3; color: #888;",
    current = "background-color: #28a745; color: white;"
  )
  # Use flexbox to stack number and multiplier vertically in the center
  div(
    style = paste(
      "width: 100px; height: 140px; border-radius: 8px;
       border: 2px solid #333; display: inline-flex;
       flex-direction: column; align-items: center;
       justify-content: center; vertical-align: top;
       font-size: 16px; font-weight: bold; margin: 6px;",
      style
    ),
    div(number),
    div(multiplier)
  )
}

# --- Server ---

server <- function(input, output, session) {
  # Tracks how many number rows the player has added in manual mode.
  # Starts at 1 so there is always at least one row visible.
  n_rows <- reactiveVal(1)

  # Holds the finalized game sequence once the player clicks Start.
  # NULL until Start is clicked, which signals the game has not yet begun.
  sequence <- reactiveVal(NULL)

  # Tracks which card is currently being revealed. Starts at 0 (all hidden)
  # and increments each time the player reveals a card.
  current_index <- reactiveVal(0)

  # Stores the value the player picked, NULL until they pick
  picked <- reactiveVal(NULL)

  # Stores display labels for each card (e.g. "3 Million", "42").
  # Set in both random and manual mode. NULL until Start is clicked.
  labels <- reactiveVal(NULL)

  # Switch between setup, game, and result pages based on game state.
  # If sequence() == NULL: setup; if picked() == NULL: game; otherwise: result.
  output$page <- renderUI({
    if (is.null(sequence())) {
      # Setup page
      uiOutput("setup")
    } else if (is.null(picked())) {
      # Game page: display cards and buttons in the middle of the page
      div(
        style = "display: flex; flex-direction: column; align-items: center;
                 justify-content: center; height: 80vh;",
        uiOutput("cards"),
        uiOutput("game_buttons")
      )
    } else {
      # Result page: display win or loss message and "Play again" button in the
      # middle of the page
      div(
        style = "display: flex; flex-direction: column; align-items: center;
                 justify-content: center; height: 80vh;",
        uiOutput("cards"),
        uiOutput("result")
      )
    }
  })

  # --- Setup ---

  # Append a new row each time the player clicks "Add number"
  observeEvent(input$add_row, {
    n_rows(n_rows() + 1)
  })

  # Render the setup page with instructions, "Random" or "Manual" choice,
  # and inputs
  output$setup <- renderUI({
    tagList(
      p(
        "Choose how to generate the sequence of numbers 
        (i.e. Random or Manual)."
      ),
      tags$ul(
        tags$li(
          "Random: the app generates a sequence of random numbers for you.
                   Choose how many cards you want."
        ),
        tags$li(
          "Manual: enter your own numbers one at a time. Use the multiplier
                   dropdown to enter large numbers (e.g. 2 x Googol). 
                   You need at least 2 numbers."
        )
      ),

      # Let the player choose between a randomly generated or
      # manually entered sequence
      radioButtons(
        "mode",
        "Sequence type",
        choices = c("Random" = "random", "Manual" = "manual")
      ),

      # Center all inputs and buttons below the sequence type selector
      div(
        style = "display: flex; flex-direction: column; align-items: center;",

        # Show number-of-cards input only when random mode is selected
        conditionalPanel(
          "input.mode == 'random'",
          numericInput(
            "n_cards",
            "Number of cards",
            value = 3,
            min = 2
          )
        ),

        # Show manual entry controls only when manual mode is selected.
        # Each number gets its own row with a base value and
        # multiplier dropdown.
        # The player clicks "Add number" to add a new row.
        conditionalPanel(
          "input.mode == 'manual'",
          uiOutput("manual_inputs"),
          actionButton("add_row", "Add number")
        ),
        actionButton("start", "Start game")
      )
    )
  })

  # Render table split into two columns that shows the entered numbers
  # and multipliers.
  # Input IDs are indexed (e.g. number_1, multiplier_1) so each row
  # can be read independently when building the sequence.
  # Existing input values are read with isolate() to preserve what the
  # player entered when a new row is added.
  output$manual_inputs <- renderUI({
    lapply(seq_len(n_rows()), function(i) {
      num_val <- isolate(input[[paste0("number_", i)]])
      mul_val <- isolate(input[[paste0("multiplier_", i)]])
      fluidRow(column(
        6,
        numericInput(
          paste0("number_", i),
          paste("Number", i),
          value = if (is.null(num_val))
            0
          else
            num_val,
          min = 0
        )
      ), column(
        6,
        # Choices are names (e.g. "Million" or "Trillion"), so
        # input returns the label directly (e.g. "Million" or "Trillion")
        # making it easy to build display labels that players can understand
        # (e.g. "1 Trillion" instead of "1,000,000,000,000").
        selectInput(
          paste0("multiplier_", i),
          "Multiplier",
          choices = c("None", "Million", "Billion", "Trillion", "Googol"),
          selected = if (is.null(mul_val)) {
            "None"
          } else {
            mul_val
          }
        )
      ))
    })
  })

  # Next, the player clicks "Start".
  # If the player chose "Random", build the sequence with generate_sequence.
  # If the player chose "Manual", read each row's number and multiplier,
  # multiply them, then pass the resulting vector to validate_manual_sequence()
  # for validation.
  observeEvent(input$start, {

    # --- Random mode ---
    # Number generation with generate_sequence
    if (input$mode == "random") {
      # generate_sequence() returns a list with values and labels
      result <- generate_sequence(input$n_cards)
      # Store labels so cards display e.g. "3 Million" instead of 3000000
      labels(result$labels)
      # Store numeric values for game logic (comparisons, is_winner)
      sequence(result$values)
    } else {

      # --- Manual Mode ---
      # Save base numbers from manual input
      base_numbers <- sapply(seq_len(n_rows()),
                             function(i) input[[paste0("number_", i)]])

      # Save multiplier names from manual input
      multipliers <- sapply(seq_len(n_rows()),
                            function(i) input[[paste0("multiplier_", i)]])

      # Use convert_manual_input() to convert the manually entered numbers
      # into numeric values for the game logic and display labels for the
      # players to read
      vals <- convert_manual_input(base_numbers, multipliers, "number")
      label_vals <- convert_manual_input(base_numbers, multipliers, "label")

      # Shuffle manual values and labels
      shuffled <- shuffle_manual_sequence(vals, label_vals)
      vals <- shuffled$values
      labels(shuffled$labels)

      # Catch validation errors with validate_manual_sequence() and show a
      # notification instead of crashing the app.
      # And assign numeric values to sequence.
      tryCatch(
        sequence(validate_manual_sequence(vals)),
        error = function(e)
          showNotification(conditionMessage(e), type = "error")
      )
    }
  })

  # --- Game ---

  # Render one card per number in the sequence once the game has started.
  # get_card_states() assigns each card a state ("passed", "current", "hidden")
  # which determines what the player sees: hidden cards are blue. Passed and 
  # current cards are grey and green, respectively.
  output$cards <- renderUI({
    req(sequence())
    states <- get_card_states(sequence(), current_index())
    lapply(seq_along(sequence()), function(i) {
      if (states[i] == "hidden") {
        # Show hidden cards as blue
        card_div("hidden")
      } else {
        # Split the label by space, e.g. "3 Million" -> c("3", "Million")
        parts <- strsplit(labels()[i], " ")[[1]]
        # Pass number and multiplier separately so card_div displays them
        # on separate lines. Plain numbers (e.g. "42") have no multiplier.
        card_div(states[i], number = parts[1],
                 multiplier = if (length(parts) > 1) parts[2] else "")
      }
    })
  })

  # Render game buttons once the game has started.
  # Before any card is revealed (index 0), show only "Reveal first card".
  # During the game, show Pick and Next. Next is hidden on the last card,
  # forcing the player to pick.
  output$game_buttons <- renderUI({
    req(sequence())
    if (current_index() == 0) {
      actionButton("next_card", "Reveal first card")
    } else {
      is_last <- current_index() == length(sequence())
      tagList(
        actionButton("pick", "Pick this number"),
        if (!is_last) actionButton("next_card", "Next")
      )
    }
  })

  # Advance to the next card when the player clicks Next
  observeEvent(input$next_card, {
    current_index(current_index() + 1)
  })

  # --- Result ---

  # Record the current number as the player's pick when they click Pick,
  # and increase current_index to 1 larger than sequence to reveal all cards
  # on the results page
  observeEvent(input$pick, {
    picked(sequence()[current_index()])
    current_index(length(sequence()) + 1)
  })

  # Show win or loss message after the player picks.
  # is_winner() compares the picked value against the full sequence and
  # determines whether the player lost or won.
  output$result <- renderUI({
    req(picked())
    # Find the label of the highest number to show in the loss message
    max_index <- which.max(sequence())
    max_label <- labels()[max_index]

    # Print win or loss message in large bold text
    tagList(if (is_winner(picked(), sequence())) {
      p("You won! That was the highest number.",
        style = "font-size: 20px; font-weight: bold;")
    } else {
      p(paste("You lost! The highest number was", max_label),
        style = "font-size: 20px; font-weight: bold;")
    },
    # Add action button to restart the game
    actionButton("restart", "Play again"))
  })

  # Reset all game state when the player clicks Play again, returning to setup.
  observeEvent(input$restart, {
    sequence(NULL)
    picked(NULL)
    current_index(0)
    n_rows(1)
    labels(NULL)
    updateNumericInput(session, "number_1", value = 0)
    updateSelectInput(session, "multiplier_1", selected = "None")
  })
}
