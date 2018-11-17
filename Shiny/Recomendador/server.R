

library(shiny)

shinyServer(function(input, output) {
  output$df2<-renderTable(read.csv2("Data/df2.csv",encoding = "ISO-8859-1",dec = ","))
  output$out <- renderPrint(input$in6)
  
  output$grafico <- renderPlot({
    Graficos(input$in6,input$ps6)
  })
  
})
