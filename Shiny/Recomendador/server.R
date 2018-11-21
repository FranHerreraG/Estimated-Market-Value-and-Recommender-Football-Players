
library(shiny)

shinyServer(function(input, output) {
  output$out <- renderPrint(input$in6)
  
  output$grafico <- renderPlot({
    Graficos(input$in6, input$ps6)
  })
  
})
