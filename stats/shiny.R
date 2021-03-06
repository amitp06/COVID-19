library(shiny)
#ui <- fluidPage()

#server <- function(input, output) {}

#shinyApp(ui = ui, server = server)

choices = df_agg$location_combined

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId='County',
                  label = 'County',
                  choices = choices,
                  selected=choices[1])
    ),
    mainPanel(
      plotOutput("County")
    )
  )
)

server <- function(input, output) {
  output$County <- renderPlot({
    plot(df_agg2[df_agg2$location_combined==input$County,]$mobility_workplaces_change,df_agg2[df_agg2$location_combined==input$County,]$cases_inc,
         xlab="Mobility Change for Workplaces in the County", ylab = "Incremental Case Counts Change in the County")
  })

}

shinyApp(ui = ui, server = server)