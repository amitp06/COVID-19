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
      plotOutput("County"),
      plotOutput("Total")
    )
  )
)

server <- function(input, output) {
  output$County <- renderPlot({
    plot(df_agg2[df_agg2$location_combined==input$County,]$mobility_workplaces_change,df_agg2[df_agg2$location_combined==input$County,]$cases_inc,
         xlab="Mobility Change for Workplaces in the County", ylab = "Incremental Case Counts Change in the County")
  })
  output$Total <- renderPlot({
    plot(df_agg$mean_work_7,df_agg$mean_cases_8,
         xlab="Mean Mobility Change for Workplaces in July for the US", ylab = "Mean Case Count Change in August for the US")
  })
}

shinyApp(ui = ui, server = server)