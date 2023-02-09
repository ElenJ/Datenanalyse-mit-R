library(shiny)
ui <- fluidPage(
  "Hello, my world!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)