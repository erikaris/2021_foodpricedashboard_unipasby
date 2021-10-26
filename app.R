library(shiny)

style <- HTML(
  '
    body {
        height: 100%;
        background-image: -webkit-radial-gradient(circle, #3f91cd, #0e314b);
        background-image: radial-gradient(circle, #3f91cd, #0e314b);
        background-repeat: no-repeat;
        background-attachment: fixed;
        padding-bottom: 0 !important;
    }
    body {
        font-family: "Roboto", sans-serif;
        font-size: 13px;
        line-height: 1.42857143;
        color: #ffffff;
        background-color: #3f91cd;
    }
    body {
        margin: 0;
    }
  '
)

ui <- div(
  # tags$head -> untuk menambahkan element di tag <head>
  # tags$style -> untuk membuat tag <style>
  tags$head(tags$style(style)), 
  "asdf"
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)