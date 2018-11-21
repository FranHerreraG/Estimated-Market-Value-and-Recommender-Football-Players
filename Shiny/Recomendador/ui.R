


library(shiny)
fluidPage(br(),
          fluidRow(column(
            10,
            sidebarPanel(
              sliderInput(
                "ps6",
                "Presupuesto",
                min = 0,
                max = 150,
                value = 50,
                step = 5
              )
            ),
            selectizeInput(
              "in6",
              "Options",
              paste(df2$ID, df2$name),
              multiple = TRUE ,
              options = list(maxItems = 3)
            )
          )),
          fluidRow(plotOutput("grafico")))
