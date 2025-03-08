library(shiny)

ui <- fluidPage(
  titlePanel("Malaria facility visualisation app"),
  sidebarLayout(
    sidebarPanel(
      # 地区の選択用インプットウィジェット
      selectInput(
        inputId = "select_district",
        label = "Select district",
        choices = c(
          "All",
          "Spring",
          "Bolo",
          "Dingo",
          "Barnard"
        ),
        selected = "All",
        multiple = TRUE
      ),
      # 年齢の選択用インプットウィジェット
      selectInput(
        inputId = "select_agegroup",
        label = "Select age group",
        choices = c(
          "All ages" = "malaria_tot",
          "0-4 yrs" = "malaria_rdt_0-4",
          "5-14 yrs" = "malaria_rdt_5-14",
          "15+ yrs" = "malaria_rdt_15"
        ),
        selected = "All",
        multiple = FALSE
      )
    ),
    mainPanel(
      # 流行曲線（エピカーブ）の描画
      plotOutput("malaria_epicurve")
    )
  )
)
