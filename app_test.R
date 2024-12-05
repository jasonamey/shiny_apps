# install.packages(c('shiny', 'tidyverse', 'lubridate', 'rsconnect', 'ggplot2'))

library(shiny)
library(tidyverse)
library(lubridate)
library(rsconnect)
runApp()
# deployApp()

# setwd("/Users/jasonamey/Desktop/BARUCH-CLASSES/1-STATS/shiny_apps")
# getwd()

# Load and preprocess data
WAGE_GROWTH_PUBLIC <- read.csv("ECIGVTWAG_data.csv") |>
  mutate(date = as.Date(date), wage_growth_percent = (value - lag(value)) / lag(value)) |>
  drop_na()

COMBINED_RETURNS <- read.csv("COMBINED_SERIES.csv") |>
  drop_na()

# Helper functions
compute_portfolio_rates_retired <- function(data) {
  data |>
    mutate(
      us_equity_rate = 0.6,
      international_equity_rate = 0.2,
      bond_rate = 0.15,
      short_term_bond_rate = 0.05
    )
}

calculate_cumulative_portfolio_retired <- function(data, starting_portfolio_value, withdrawal = 60000) {
  data <- data |>
    mutate(
      us_equity_value = 0,
      international_equity_value = 0,
      bond_value = 0,
      short_term_bond_value = 0,
      total_portfolio_value = 0
    )
  
  initial_us_equity_value <- starting_portfolio_value * 0.6
  initial_international_equity_value <- starting_portfolio_value * 0.2
  initial_bond_value <- starting_portfolio_value * 0.15
  initial_short_term_bond_value <- starting_portfolio_value * 0.05
  
  for (i in seq_len(nrow(data))) {
    if (i == 1) {
      data$us_equity_value[i] <- initial_us_equity_value * (1 + data$SPY_monthly_adjusted_growth[i])
      data$international_equity_value[i] <- initial_international_equity_value * (1 + data$ACWI_monthly_adjusted_growth[i])
      data$bond_value[i] <- initial_bond_value * (1 + data$AGG_monthly_adjusted_growth[i])
      data$short_term_bond_value[i] <- initial_short_term_bond_value * (1 + data$treasury_monthly_return[i])
    } else {
      data$us_equity_value[i] <- data$us_equity_value[i - 1] * (1 + data$SPY_monthly_adjusted_growth[i])
      data$international_equity_value[i] <- data$international_equity_value[i - 1] * (1 + data$ACWI_monthly_adjusted_growth[i])
      data$bond_value[i] <- data$bond_value[i - 1] * (1 + data$AGG_monthly_adjusted_growth[i])
      data$short_term_bond_value[i] <- data$short_term_bond_value[i - 1] * (1 + data$treasury_monthly_return[i])
    }
    data$total_portfolio_value[i] <- data$us_equity_value[i] +
      data$international_equity_value[i] +
      data$bond_value[i] +
      data$short_term_bond_value[i] - withdrawal / 12
  }
  return(data)
}

simulate_portfolio_working <- function(num_years, starting_age, starting_salary, n_bootstrap, 
                                       wage_growth_public = WAGE_GROWTH_PUBLIC, 
                                       combined_returns = COMBINED_RETURNS) {
  months <- num_years * 12
  starting_date <- as.Date("2024-12-01") - years(starting_age)
  combined_returns_no_salary <- combined_returns |>
    select(-salary, -AGG, -SPY, -ACWI, -value) |>
    na.omit()
  bootstrap_returns <- combined_returns_no_salary |>
    slice_sample(n = months * n_bootstrap, replace = TRUE) |>
    mutate(bootstrap_id = rep(1:n_bootstrap, each = months))
  bootstrap_wages <- wage_growth_public |>
    slice_sample(n = months * n_bootstrap, replace = TRUE) |>
    mutate(bootstrap_id = rep(1:n_bootstrap, each = months)) |>
    group_by(bootstrap_id) |>
    mutate(salary = starting_salary * cumprod(1 + lag(wage_growth_percent, default = 0))) |>
    ungroup() |>
    select(-bootstrap_id, -wage_growth_percent)
  returns_wages <- cbind(bootstrap_returns, bootstrap_wages)
  portfolio_working_calculations <- returns_wages |>
    group_by(bootstrap_id) |>
    mutate(
      date = seq.Date(from = starting_date, by = "month", length.out = n()),
      years_of_service = floor((row_number() - 1) / 12 + 1)
    ) |>
    ungroup()
  portfolio_simulations <- portfolio_working_calculations |>
    select(date, bootstrap_id, total_portfolio_value = salary) |>
    pivot_wider(
      names_from = bootstrap_id,
      values_from = total_portfolio_value,
      names_prefix = "bootstrap_"
    )
  return(portfolio_simulations)
}

working_retirement_portfolio <- function(years_working, starting_age, starting_salary, 
                                         retirement_years, annual_withdrawal = 50000, n_bootstrap = 10, 
                                         wage_growth_public = WAGE_GROWTH_PUBLIC, 
                                         combined_returns = COMBINED_RETURNS) {
  working_portfolio <- simulate_portfolio_working(years_working, starting_age, starting_salary, 
                                                  n_bootstrap, wage_growth_public, combined_returns)
  last_date <- tail(working_portfolio$date, 1)
  avg_portfolio_value <- mean(tail(working_portfolio[, -1], 1), na.rm = TRUE)
  retirement_portfolio <- simulate_retirement_portfolio(
    retirement_years, years_working + starting_age, last_date, avg_portfolio_value, 
    annual_withdrawal, n_bootstrap, combined_returns
  )
  return(rbind(working_portfolio, retirement_portfolio))
}

# Define UI
ui <- fluidPage(
  titlePanel("Retirement Portfolio Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("years_working", "Years Working:", min = 5, max = 30, value = 20, step = 1),
      sliderInput("starting_age", "Starting Age:", min = 25, max = 45, value = 30, step = 1),
      numericInput("starting_salary", "Starting Salary:", value = 35000, min = 20000, max = 100000, step = 5000),
      sliderInput("retirement_years", "Retirement Years:", min = 3, max = 20, value = 10, step = 1),
      sliderInput("annual_withdrawal", "Annual Withdrawal:", min = 20000, max = 200000, value = 50000, step = 5000),
      sliderInput("n_bootstrap", "Number of Bootstraps:", min = 20, max = 200, value = 50, step = 1),
      actionButton("run_simulation", "Run Simulation")
    ),
    mainPanel(
      plotOutput("simulation_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run_simulation, {
    portfolio_simulation <- tryCatch({
      working_retirement_portfolio(
        years_working = input$years_working,
        starting_age = input$starting_age,
        starting_salary = input$starting_salary,
        retirement_years = input$retirement_years,
        annual_withdrawal = input$annual_withdrawal,
        n_bootstrap = input$n_bootstrap,
        wage_growth_public = WAGE_GROWTH_PUBLIC,
        combined_returns = COMBINED_RETURNS
      )
    }, error = function(e) {
      showNotification("Error during simulation: Check your inputs.", type = "error")
      return(NULL)
    })
    
    if (!is.null(portfolio_simulation)) {
      output$simulation_plot <- renderPlot({
        portfolio_simulation_long <- portfolio_simulation |>
          pivot_longer(cols = starts_with("bootstrap_"), names_to = "bootstrap", values_to = "value")
        
        ggplot(portfolio_simulation_long, aes(x = date, y = value, color = bootstrap)) +
          geom_line(size = 0.1) +
          labs(title = "Portfolio Simulation Over Time", x = "Date", y = "Portfolio Value") +
          scale_y_continuous(labels = scales::label_dollar()) +
          theme_minimal() +
          theme(legend.position = "none")
      })
    }
  })
}

shinyApp(ui = ui, server = server)
