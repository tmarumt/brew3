library(shiny)
library(shinydashboard)
library(yaml)
library(snakecaser)
library(stringr)
library(lubridate)
library(tidyverse)

gen_step <- function() {
  dat <- pick(everything())

  step <- ""
  if (dat$water_vol > 0) {
    w_temp <- ""
    if (dat$diff_water_temp != 0) {
      w_temp <- str_glue(" of {dat$water_temp} C")
    }

    with_immersion <- ""
    if (dat$immersion) {
      with_immersion <- " with immersion"
    }

    use_pourover_aid <- ""
    if (dat$pourover_aid) {
      use_pourover_aid <- " using pourover aid"
    }

    step <- str_glue("Pour {dat$water_vol} mL{w_temp} up to {dat$cum_water_vol} mL{use_pourover_aid}{with_immersion}")
  } else {
    if (dat$diff_immersion > 0) {
      step <- "Change to immersion"
    } else if (dat$diff_immersion < 0) {
      step <- "Change to drip"
    }
  }

  with_extract_chilling <- ""
  if (dat$extract_chilling) {
    with_extract_chilling <- " with extract chilling"
  }

  and_stir <- ""
  if (dat$stir) {
    and_stir <- " and stir"
  }

  str_glue("{step}{with_extract_chilling}{and_stir}")
}

recipes <- yaml::read_yaml(here::here("recipes.yaml"))

recipe_raw <- rio::import(here::here("recipes", str_c(to_snake_case(recipes[[2]]$name), ".csv"))) |>
  mutate(
    start_time_p = ms(start_time),
    end_time_p = ms(end_time)
  ) |>
  mutate(
    diff_time = end_time_p - start_time_p,
    cum_water_vol = cumsum(water_vol),
    diff_water_temp = water_temp - lag(water_temp, default = first(water_temp)),
    diff_immersion = immersion - lag(immersion, default = first(immersion)),
    diff_extract_chilling = extract_chilling - lag(extract_chilling, default = first(extract_chilling))
  )

tot_water_vol <- recipe_raw$cum_water_vol[nrow(recipe_raw)]

ui <- fluidPage(
  titlePanel("Brew Recipe"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ratio", "Ratio (1:X)", min = 10, max = 20, value = recipes[[2]]$ratio, step = 0.5),
      numericInput("beans", "Beans (g)", value = NA, min = 0, step = 1e-1),
      numericInput("water", "Water (mL)", value = tot_water_vol, min = 0)
    ),
    mainPanel(
      verbatimTextOutput("results"),
      tableOutput("water_table")
    )
  )
)

# ui <- dashboardPage(
#   dashboardHeader(title = "Brew Recipes"),
#   dashboardSidebar(
#     sidebarMenu(
#       map(recipes, \(recipe) menuItem(recipe$name, tabName = recipe$name))
#     )
#   ),
#   dashboardBody(
#     map(recipes, \(recipe) tabItem(
#       tabName = recipe$name,
#       fluidPage(
#         titlePanel(recipe$name),
#         sidebarLayout(
#           sidebarPanel(
#             sliderInput("ratio", "Ratio (1:X)", min = 10, max = 20, value = 17.5, step = 0.5),
#             numericInput("beans", "Beans (g)", value = NA, min = 0, step = 1e-1),
#             numericInput("water", "Water (mL)", value = tot_water_vol, min = 0)
#           ),
#           mainPanel(
#             verbatimTextOutput(str_c("results_", snakecaser::to_snake_case(recipe$name))),
#             tableOutput(str_c("water_table_", snakecaser::to_snake_case(recipe$name)))
#           )
#         )
#       )
#     ))
#   )
# )

server <- function(input, output, session) {
  observeEvent(input$beans, {
    if (!is.na(input$beans) && input$beans > 0) {
      new_water <- input$beans * input$ratio
      updateNumericInput(session, "water", value = new_water)
    }
  })

  observeEvent(input$water, {
    if (!is.na(input$water) && input$water > 0) {
      new_beans <- input$water / input$ratio
      updateNumericInput(session, "beans", value = new_beans)
    }
  })

  observeEvent(input$ratio, {
    if (!is.na(input$water) && input$water > 0) {
      new_beans <- input$water / input$ratio
      updateNumericInput(session, "beans", value = new_beans)
    } else if (!is.na(input$beans) && input$beans > 0) {
      new_water <- input$beans * input$ratio
      updateNumericInput(session, "water", value = new_water)
    }
  })

  output$results <- renderPrint({
    cat("Beans:", input$beans, "g\n")
    cat("Water:", input$water, "mL\n")
    cat("Ratio (1:X):", input$ratio)
  })

  show_recipe <- reactive({
    scale_factor <- input$water / tot_water_vol

    recipe <- recipe_raw |>
      mutate(
        water_vol = round(water_vol * scale_factor, 1e-2),
        cum_water_vol = round(cum_water_vol * scale_factor, 1e-2)
      ) |>
      rowwise() |>
      mutate(
        Time = if_else(diff_time == 0, start_time, str_c(start_time, end_time, sep = " - ")),
        Step = gen_step()
      ) |>
      ungroup() |>
      select(Time, Step, Note) |>
      add_row(Time = recipes[[2]]$time, Step = "Brewing finished", Note = "")
  })

  output$water_table <- renderTable({
    show_recipe()
  })
}

shinyApp(ui = ui, server = server)
