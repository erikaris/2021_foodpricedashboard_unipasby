library(shiny)
library(bs4Dash)
library(highcharter)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "",
      color = NULL,
      href = "https://adminlte.io/themes/v3",
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
    )
  ),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    fluidRow(
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        "Lorem ipsum is so fun!"
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        "Lorem ipsum is so fun!"
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        "Lorem ipsum is so fun!"
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        "Lorem ipsum is so fun!"
      ))
    ),
    fluidRow(
      column(
        width = 6,
        bs4Card(
          width = 12,
          closable = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          "Lorem ipsum is so fun!"
        ),
        bs4Card(
          width = 12,
          closable = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          "Lorem ipsum is so fun!"
        )
      ),
      column(
        width = 6,
        bs4Card(
          width = 12,
          closable = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          highchartOutput("map_1")
        )
      )
    )
  ),
  controlbar = dashboardControlbar()
)

server <- function(input, output, session) {
  output$map_1 <- renderHighchart({
    hcmap(
      "https://code.highcharts.com/mapdata/countries/id/id-all.js",
      download_map_data = FALSE,
      # data = df,
      # value = "sum_potensi_teknikal",
      # joinBy = c("fips", "id_prov"),
      # name = "Potensi Teoritis",
      # dataLabels = list(enabled = TRUE, format = "{point.alamat_provinsi}"),
      # borderColor = "#FAFAFA", borderWidth = 0.05,
      # tooltip = list(valueDecimals = 0, valuePrefix = "~", valueSuffix = " MW")
    )
  })
}

shinyApp(ui, server)