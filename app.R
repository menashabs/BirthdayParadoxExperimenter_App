# ----------------------------------------------------------

# ---------- The Birthday Paradox Experimenter -------------

# ----------------------------------------------------------

# --- LIBRARIES ---

library(shiny)
library(ggplot2)
library(dplyr)

# --- FUNCTIONS ---

# Theoretical probability of at least one shared birthday
birthday_probability <- function(n) {
  if (n < 2) return(0)
  if (n > 365) return(1)
  p_no_match <- prod((365 - (0:(n - 1))) / 365)
  1 - p_no_match
}

# Simulate one group of people
simulate_group <- function(n) {
  birthdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(birthdays))
}

# Run Monte Carlo simulation
run_simulation <- function(n, repetitions) {
  results <- replicate(repetitions, simulate_group(n))
  sum(results)
}

# --- UI ---

ui <- fluidPage(
  # --- Custom Color Palette (Burgundy) ---
  tags$head(
    tags$style(HTML("
      body {
        background-color: #fff8f5;
        color: #3b1725;
        font-family: 'Georgia', serif;
      }
      h1, h2, h3, h4 {
        color: #58152b;
        font-weight: bold;
      }
      .title-box {
        background-color: #58152b;
        padding: 22px;
        border-radius: 10px;
        text-align: center;
        margin-bottom: 25px;
        border: 2px solid #8f3b55;
      }
      .title-box h1 {
        color: #fff4e9;
        margin: 0;
      }
      .well {
        background-color: #f4e5e5;
        border: 1px solid #d8b7bd;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(88, 21, 43, 0.06);
      }
      .btn {
        background-color: #781d3b !important;
        color: #ffffff !important;
        border: none !important;
        font-weight: bold;
        border-radius: 6px;
        padding: 8px 15px;
      }
      .btn:hover {
        background-color: #9a3452 !important;
      }
      label {
        color: #58152b;
        font-weight: bold;
      }
      .form-control {
        background-color: #fffaf7;
        color: #3b1725;
        border: 1px solid #c99da8;
        border-radius: 6px;
      }
      .form-control:focus {
        border-color: #781d3b;
        box-shadow: 0 0 4px rgba(120, 29, 59, 0.3);
      }
      .message-box {
        background-color: #f4e5e5;
        border: 2px solid #781d3b;
        border-radius: 10px;
        padding: 15px;
        margin-top: 15px;
        text-align: center;
        font-size: 17px;
        font-weight: bold;
        color: #58152b;
      }
      .result-box {
        background-color: #f4e5e5;
        border: 1px solid #d8b7bd;
        border-radius: 10px;
        padding: 12px;
        text-align: center;
        min-height: 125px;
        margin-bottom: 12px;
      }
      .result-value {
        color: #781d3b;
        font-size: 28px;
        font-weight: bold;
      }
      .result-heading {
        color: #58152b;
        font-size: 16px;
        font-weight: bold;
      }
      .intro-text {
        font-size: 17px;
        text-align: justify;
        margin-bottom: 30px;
        margin-top: 15px;
        line-height: 1.6;
      }
      .section-heading {
        color: #58152b;
      }
      a {
        color: #781d3b;
        text-decoration: underline;
      }
      hr {
        border-top: 1px solid #c99da8;
      }
      .shiny-output-error {
        color: #a00030;
      }
    "))
  ),
  
  # --- Title Box ---
  tags$div(
    class = "title-box",
    h1("BirthdayParadoxExperimenter: Birthday Paradox Experimenting App")
  ),
  
  # --- Image and Introduction Side by Side ---
  fluidRow(
    column(
      5,
      tags$img(
        src = "Birthday_Paradox_Image.jpg",
        width = "80%",
        style = paste(
          "max-height:450px;",
          "object-fit:cover;",
          "border-radius:5px;",
          "margin-left:100px;",
          "margin-bottom:10px;"
        )
      )
    ),
    column(
      7,
      tags$div(
        class = "intro-text",
        "The Birthday Paradox demonstrates a surprising result in probability",
        "In a group of just 23 people, there is approximately a 50.73% chance that at least two people share the same birthday.",
        tags$br(), tags$br(),
        "At first, this seems unlikely. There are 365 possible birthdays. However, the probability increases quickly because every possible pair of people can form a birthday match.",
        "This experimenting app allows you to explore how group size affects the probability of a shared birthday and compare theoretical probabilities with results from repeated simulations.",
        tags$br(), tags$br(),
        "Note: This experiment assumes 365 equally likely birthdays and does not consider leap years.",
        tags$br(), tags$br(),
        tags$a(
          href = "https://medium.com/@menashabrayani/the-birthday-paradox-how-just-23-people-can-surprise-you-f2f41643ab45?sharedUserId=menashabrayani",
          "Read more about Birthday paradox here.",
          target = "_blank"
        )
      )
    )
  ),
  
  # --- Main Experiment Layout ---
  fluidRow(
    # LEFT SIDE: Experiment Setup
    column(
      width = 7,
      wellPanel(
        h3(
          "Experiment Setup",
          class = "section-heading"
        ),
        sliderInput(
          "group_size",
          "Number of people in the group:",
          min = 0,
          max = 100,
          value = 23,
          step = 1
        ),
        sliderInput(
          "repetitions",
          "Number of simulation repetitions:",
          min = 0,
          max = 20000,
          value = 5000,
          step = 100
        ),
        numericInput(
          "seed",
          "Random seed:",
          value = 123,
          min = 1,
          max = 999999
        ),
        fluidRow(
          column(
            6,
            actionButton(
              "run_btn",
              "Run Experiment",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              "reset_btn",
              "Reset Settings",
              width = "100%"
            )
          )
        ),
        hr(),
        h4(
          "Understanding the Experiment",
          class = "section-heading"
        ),
        tags$p(
          "Each simulation creates a group of people and randomly assigns each person a birthday from 1 to 365."
        ),
        tags$p(
          "The experiment checks whether at least two people have the same birthday. This process is repeated according to your selected number of repetitions."
        ),
        tags$p(
          "Try changing the group size to see how quickly the probability of a shared birthday increases."
        )
      )
    ),
    
    # RIGHT SIDE: Results
    column(
      width = 5,
      fluidRow(
        column(
          6,
          tags$div(
            class = "result-box",
            tags$div(
              class = "result-heading",
              "Theoretical Probability"
            ),
            tags$div(
              class = "result-value",
              textOutput("theoretical_result")
            )
          )
        ),
        
        column(
          6,
          tags$div(
            class = "result-box",
            tags$div(
              class = "result-heading",
              "Simulated Probability"
            ),
            tags$div(
              class = "result-value",
              textOutput("simulated_result")
            )
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          tags$div(
            class = "result-box",
            tags$div(
              class = "result-heading",
              "Matching Experiments"
            ),
            tags$div(
              class = "result-value",
              textOutput("matching_result")
            )
          )
        ),
        column(
          6,
          tags$div(
            class = "result-box",
            tags$div(
              class = "result-heading",
              "Total Experiments"
            ),
            tags$div(
              class = "result-value",
              textOutput("total_result")
            )
          )
        )
      ),
      uiOutput("messageBox"),
      br(),
      tags$div(
        style = paste(
          "font-size:16px;",
          "text-align:center;",
          "margin-top:20px;"
        ),
        "💡 Tip: Start with a 10 people and run the experiment several times.",
        tags$br(), tags$br(),
        "Then try 23, 30, 40 and 50 people. Observe how the probability changes!"
      )
    )
  ),
  
  # --- Probability Curve ---
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3(
          "Explore the Birthday Probability Curve",
          class = "section-heading"
        ),
        plotOutput(
          "probability_plot",
          height = "380px"
        )
      )
    )
  ),
  # --- Explanation and Results ---
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3(
          "Experiment Summary",
          class = "section-heading"
        ),
        verbatimTextOutput("summary")
      )
    )
  ),
  tags$div(
    style = paste(
      "text-align:center;",
      "padding:20px;",
      "color:#781d3b;",
      "font-size:14px;"
    ),
    "The Curious World of Statistics | Exploring Paradoxes, Puzzles and Statistical Surprises."
  )
)


# --- SERVER ---

server <- function(input, output, session) {
  results <- reactiveValues(
    theoretical = birthday_probability(23),
    simulated = NA_real_,
    matches = NA_integer_,
    repetitions = NA_integer_,
    group_size = 23,
    has_run = FALSE
  )
  # Run Experiment
  observeEvent(input$run_btn, {
    req(input$group_size, input$repetitions, input$seed)
    set.seed(input$seed)
    n <- input$group_size
    reps <- input$repetitions
    matches <- run_simulation(n, reps)
    results$theoretical <- birthday_probability(n)
    results$simulated <- matches / reps
    results$matches <- matches
    results$repetitions <- reps
    results$group_size <- n
    results$has_run <- TRUE
  })
  
  # Reset Settings
  observeEvent(input$reset_btn, {
    updateSliderInput(
      session,
      "group_size",
      value = 23
    )
    updateSliderInput(
      session,
      "repetitions",
      value = 5000
    )
    updateNumericInput(
      session,
      "seed",
      value = 123
    )
    results$theoretical <- birthday_probability(23)
    results$simulated <- NA_real_
    results$matches <- NA_integer_
    results$repetitions <- NA_integer_
    results$group_size <- 23
    results$has_run <- FALSE
  })
  
  # Theoretical Probability
  output$theoretical_result <- renderText({
    req(results$theoretical)
    paste0(
      round(results$theoretical * 100, 2),
      "%"
    )
  })
  
  # Simulated Probability
  output$simulated_result <- renderText({
    if (!results$has_run) return("—")
    paste0(
      round(results$simulated * 100, 2),
      "%"
    )
  })
  
  # Number of Matching Experiments
  output$matching_result <- renderText({
    if (!results$has_run) return("—")
    results$matches
  })
  
  # Total Experiments
  output$total_result <- renderText({
    if (!results$has_run) return("—")
    results$repetitions
  })
  
  # Interpretation Message
  output$messageBox <- renderUI({
    if (!results$has_run) {
      message <- paste(
        "Choose your settings and click Run Experiment",
        "to explore the Birthday Paradox."
      ) 
    } else {
      difference <- abs(
        results$theoretical - results$simulated
      )
      if (difference < 0.01) {
        message <- paste(
          "Excellent! The simulated probability is",
          "very close to the theoretical probability."
        )
      } else if (difference < 0.03) {
        message <- paste(
          "The simulation is reasonably close to the",
          "theoretical result. Try increasing the number",
          "of repetitions to explore the difference."
        )
      } else {
        message <- paste(
          "The simulated result differs from the theoretical",
          "probability. Random variation is expected.",
          "Try increasing the number of repetitions."
        )
      }
    }
    tags$div(
      class = "message-box",
      message
    )
  })
  
  # Probability Curve
  output$probability_plot <- renderPlot({
    curve_data <- data.frame(
      people = 2:100
    ) %>%
      mutate(
        probability = sapply(
          people,
          birthday_probability
        )
      )
    p <- ggplot(
      curve_data,
      aes(
        x = people,
        y = probability
      )
    ) +
       geom_line(
        colour = "#781d3b",
        linewidth = 1.3
      ) +
      geom_point(
        data = data.frame(
          people = c(23, 50),
          probability = c(
            birthday_probability(23),
            birthday_probability(50)
          )
        ),
        aes(
          x = people,
          y = probability
        ),
        colour = "#c47b35",
        size = 3
      ) +
      geom_hline(
        yintercept = 0.5,
        linetype = "dashed",
        colour = "#9b6575"
      ) +
      geom_vline(
        xintercept = 23,
        linetype = "dotted",
        colour = "#9b6575"
      ) +
      scale_y_continuous(
        labels = function(x) paste0(x * 100, "%"),
        limits = c(0, 1)
      ) +
      scale_x_continuous(
        breaks = seq(0, 100, by = 10)
      ) +
      labs(
        title = "Probability of at Least One Shared Birthday",
        x = "Number of People",
        y = "Probability"
      ) +
      theme_minimal(base_family = "Georgia") +
      theme(
        plot.title = element_text(
          colour = "#58152b",
          face = "bold",
          size = 15
        ),
        axis.title = element_text(
          colour = "#58152b",
          face = "bold"
        ),
        axis.text = element_text(
          colour = "#3b1725"
        ),
        panel.grid.minor = element_blank()
      )
    if (results$has_run) {
      p <- p +
        geom_point(
          data = data.frame(
            people = results$group_size,
            probability = results$simulated
          ),
          aes(
            x = people,
            y = probability
          ),
          colour = "#d28b24",
          size = 4,
          shape = 17
        )
    }
    p
  })
  
  # Experiment Summary
  output$summary <- renderText({
    if (!results$has_run) {
      return(
        paste(
          "No experiment has been run yet.",
          "Select your settings and click Run Experiment."
        )
      )
    }
    paste0(
      "Number of people: ", results$group_size, "\n",
      "Number of repetitions: ", results$repetitions, "\n",
      "Experiments with a shared birthday: ", results$matches, "\n",
      "Theoretical probability: ",
      round(results$theoretical * 100, 2), "%\n",
      "Simulated probability: ",
      round(results$simulated * 100, 2), "%\n",
      "Difference: ",
      round(
        abs(results$theoretical - results$simulated) * 100,
        2
      ),
      " percentage points"
    )
  })
}

# --- RUN APPLICATION ---

shinyApp(ui = ui, server = server)


