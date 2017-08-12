# Coursera Data Science Specialization
# Devleoping Data Products
# Scratch Work
########################################################################################
# Demo of manipulate function
library(manipulate)
manipulate(plot(1:x), x = slider(1,100))
manipulate(hist(rnorm(1:x)), x = slider(1, 1000))

########################################################################################
# First Shiny app - starting from template
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose an number",
              value = 25, min = 1, max = 1000),
  plotOutput("hist")
)

server <- function(input, output) {
  output$hist = renderPlot({
    hist(rnorm(input$num))
    })
}

shinyApp(ui = ui, server = server)