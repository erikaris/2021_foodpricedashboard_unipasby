library(shiny)
library(bs4Dash)
library(highcharter)
library(dplyr)

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
        title = "Komoditas",
        footer = "Komoditas Diteliti",
        uiOutput("card_1")
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Pasar",
        footer = "Pasar Dikunjungi",
        uiOutput("card_2")
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Harga Tertinggi",
        footer = uiOutput("card_max_price_item"),
        uiOutput("card_max_price_rp")
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Harga Terendah",
        footer = uiOutput("card_min_price_item"),
        uiOutput("card_min_price_rp")
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
          # dropdownMenu = dropdownMenu(
          #   badgeStatus = "danger",
          #   type = "notifications",
          #   # notificationItem(
          #   #   inputId = "triggerAction1",
          #   #   message = "message 1",
          #   #   from = "Divad Nojnarg",
          #   #   image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
          #   #   time = "today",
          #   #   color = "lime"
          #   # )
          # ),
          highchartOutput("map_1")
        )
      )
    )
  ),
  controlbar = dashboardControlbar()
)

server <- function(input, output, session) {
  commodity_prices <- reactive({
    read.csv2("commodity_prices.csv")
  })
  
  provinces <- reactive({
    read.csv2("provinces.csv")
  })
  
  markets <- reactive({
    read.csv2("markets.csv")
  })
  
  highchart_map_id <- reactive({
    read.csv("highchart_map_id.csv")
  })
  
  highchart_map_data <- reactive({
    commodity_prices <- commodity_prices()
    provinces <- provinces()
    highchart_map_id <- highchart_map_id()
    
    commodity_prices %>% 
      left_join(provinces, by = c("province" = "id")) %>% 
      left_join(highchart_map_id, by = c("label" = "provinsi"))
  })
  
  output$card_1 <- renderUI(h2(commodity_prices() %>% select(Komoditas..Rp.) %>% n_distinct()))
  output$card_2 <- renderUI(h2(markets() %>% nrow()))
  
  observe({
    commodity_max_price <- commodity_prices() %>% slice_max(harga)
    output$card_max_price_rp <- renderUI(h2(paste("Rp.", commodity_max_price %>% select(harga) %>% unique())))
    output$card_max_price_item <- renderUI(paste(commodity_max_price %>% select(Komoditas..Rp.) %>% unique(), collapase = ", "))
    
    commodity_min_price <- commodity_prices() %>% slice_min(harga)
    output$card_min_price_rp <- renderUI(h2(paste("Rp.", commodity_min_price %>% select(harga) %>% unique())))
    output$card_min_price_item <- renderUI(paste(commodity_min_price %>% select(Komoditas..Rp.) %>% unique(), collapase = ", "))
  })
  
  output$map_1 <- renderHighchart({
    hcmap(
      "https://code.highcharts.com/mapdata/countries/id/id-all.js",
      download_map_data = TRUE,
      data = highchart_map_data(),
      value = "harga",
      joinBy = c("fips", "highchart_prov_id"),
      name = "Harga komoditas",
      dataLabels = list(enabled = TRUE, format = "{point.label}"),
      borderColor = "#FAFAFA", borderWidth = 0.05,
      tooltip = list(valueDecimals = 0, valuePrefix = "Rp.", valueSuffix = "")
    )
  })
}

shinyApp(ui, server)