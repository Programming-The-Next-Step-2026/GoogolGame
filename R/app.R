#' Launch the Googol Game Shiny app
#'
#' @export
run_app <- function() {
  library(shiny)

  ui <- fluidPage(
    titlePanel("Googol Game"),
    tags$head(tags$style(HTML("
      .card-row {
        display: flex;
        gap: 16px;
        flex-wrap: wrap;
        margin: 24px 0;
      }
      .card {
        width: 100px;
        height: 150px;
        border-radius: 10px;
        border: 3px solid #ccc;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 28px;
        font-weight: bold;
      }
      /* Face down: not yet revealed */
      .card-hidden {
        background-color: #2c3e50;
        color: #2c3e50;
      }
      /* Currently revealed card */
      .card-current {
        background-color: #ffffff;
        border-color: #3498db;
      }
      /* Cards the player chose to pass */
      .card-passed {
        background-color: #ecf0f1;
        color: #95a5a6;
      }
      /* Result: card the player picked */
      .card-picked {
        background-color: #ffffff;
        border-color: #3498db;
      }
      /* Result: highest number in the sequence */
      .card-winner {
        background-color: #d5f5e3;
        border-color: #27ae60;
      }
      /* Result: player picked the highest — win */
      .card-picked-winner {
        background-color: #d5f5e3;
        border-color: #27ae60;
        box-shadow: 0 0 10px #27ae60;
      }
    "))),
    # uiOutput renders different screens dynamically based on game state
    # State 1: setup — choose random or manual mode, set n, enter numbers if manual
    # State 2: game — shows cards face down, reveals one at a time
    # State 3: result — all cards face up, picked and highest card highlighted
    uiOutput("screen")
  )

  server <- function(input, output, session) {
    # reactiveValues persists game state across user interactions
    state <- reactiveValues(
      screen = "setup",      # which screen to display: "setup", "game", or "result"
      sequence = NULL,       # the vector of numbers for the current game
      current_index = 1,     # which card has been flipped (1 = first card revealed)
      picked = NULL          # the value the player chose to stop at
    )

    # Switch between screens by replacing the entire UI based on state$screen
    output$screen <- renderUI({
      switch(state$screen,
        setup = tagList(
          radioButtons("mode", "Mode",
            choices = c("Random" = "random", "Manual entry" = "manual")
          ),
          numericInput("n", "Number of values", value = 3, min = 2, step = 1),
          uiOutput("manual_inputs"),
          actionButton("start", "Start Game")
        ),
        game = {
          # Delegate card state logic to game_logic.R
          states <- get_card_states(state$sequence, state$current_index)
          tagList(
            tags$div(class = "card-row",
              lapply(seq_along(state$sequence), function(i) {
                css_class <- paste("card", paste0("card-", states[i]))
                label <- if (states[i] == "hidden") "?" else state$sequence[i]
                tags$div(class = css_class, label)
              })
            ),
            actionButton("pick", "Pick this card"),
            # Next button is hidden on the last card, forcing the player to pick
            if (state$current_index < length(state$sequence))
              actionButton("next_num", "Flip next card")
          )
        },
        result = {
          picked <- state$picked
          max_val <- max(state$sequence)
          tagList(
            h2(if (is_winner(picked, state$sequence)) "You win!" else "You lose!"),
            # Show all cards face up; highlight the picked and highest cards
            tags$div(class = "card-row",
              lapply(seq_along(state$sequence), function(i) {
                num <- state$sequence[i]
                css_class <- paste("card", if (num == picked && num == max_val) "card-picked-winner"
                                           else if (num == picked) "card-picked"
                                           else if (num == max_val) "card-winner"
                                           else "card-passed")
                tags$div(class = css_class, num)
              })
            ),
            actionButton("play_again", "Play Again")
          )
        }
      )
    })

    # Dynamically render one numericInput per value when in manual mode
    output$manual_inputs <- renderUI({
      req(input$mode == "manual", input$n)
      lapply(seq_len(input$n), function(i) {
        numericInput(paste0("num_", i), paste("Number", i), value = NA, min = 0, max = 1e6)
      })
    })

    observeEvent(input$start, {
      n <- input$n
      if (input$mode == "random") {
        state$sequence <- generate_sequence(n)
        state$current_index <- 1
        state$picked <- NULL
        state$screen <- "game"
      } else {
        # Collect values from dynamically generated inputs
        numbers <- sapply(seq_len(n), function(i) input[[paste0("num_", i)]])
        # create_manual_sequence validates input and throws an error if invalid
        tryCatch({
          state$sequence <- create_manual_sequence(numbers)
          state$current_index <- 1
          state$picked <- NULL
          state$screen <- "game"
        }, error = function(e) {
          # Show the validation error to the player without crashing the app
          showNotification(conditionMessage(e), type = "error")
        })
      }
    })

    observeEvent(input$pick, {
      state$picked <- get_current_number(state$sequence, state$current_index)
      state$screen <- "result"
    })

    observeEvent(input$next_num, {
      state$current_index <- state$current_index + 1
    })

    # Reset all state to allow a fresh game without restarting the app
    observeEvent(input$play_again, {
      state$screen <- "setup"
      state$sequence <- NULL
      state$current_index <- 1
      state$picked <- NULL
    })
  }

  shinyApp(ui, server)
}
