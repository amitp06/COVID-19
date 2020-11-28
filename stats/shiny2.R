library(shiny)
#ui <- fluidPage()

#server <- function(input, output) {}

#shinyApp(ui = ui, server = server)

choices = c('Retail and Recreation Mobility', 'Grocery and Pharmacy Mobility', 'Parks Mobility',
            'Transit Stations Mobility', 'Workplace Mobility', 'Residential Mobility')

df_agg2 = df_agg[,c('mean_retail_7', 'mean_grocery_7', 'mean_parks_7', 'mean_transit_7','mean_work_7', 'mean_res_7','case_diff_7_8')]

choicelookup = as.data.frame(cbind(choices, c('mean_retail_7', 'mean_grocery_7', 'mean_parks_7', 'mean_transit_7','mean_work_7', 'mean_res_7')))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId='Category',
                  label = 'Mobilty Category',
                  choices = choices,
                  selected=choices[1])
    ),
    mainPanel(
      plotOutput("Category")
    )
  )
)

server <- function(input, output) {
  output$Category <- renderPlot({
    plot(data.frame(df_agg2[,choicelookup[choicelookup$choices==input$Category,]$V2],df_agg2[,"case_diff_7_8"]*100),
         xlab="Mobility Change for Selected Category (July 2020 Mean)", ylab = "Percent Change in Cases (July-August 2020)",main='COVID Case Growth by Mobility Category')
  })

}

shinyApp(ui = ui, server = server)