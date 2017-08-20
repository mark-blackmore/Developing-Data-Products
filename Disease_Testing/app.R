#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Disease Testing"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Prevalence: "),
        numericInput("prev", "Disease Prevalence (%)", 
                     min = 0, 
                     max = 100,
                     value = 9),
        h3("add text"),
        sliderInput("sens","Sensitivity (%):",
                     min = 0,
                     max = 100,
                     value = 95),
        h3("add text"),
        sliderInput("spec","Specificity (%):",
                     min = 0,
                     max = 100,
                     value = 95)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3("Cases per 100,000 people"), 
        tableOutput("confusion"),
        h3("plots"),
        plotOutput("prediction")
      )
   )
)

# Define server logic 
server <- function(input, output) {

  output$confusion <- renderTable({
    n      <- 100000
    totDis <- input$prev/100 * n
    TP     <- floor(totDis * input$sens/100)
    FN     <- totDis - TP
    totNodis <- 100000 - totDis
    TN     <- floor(totNodis * input$spec/100)
    FP     <- totNodis - TN
    PPV    <- TP / (TP + FP)
    FOR    <- FN / (FN + TN)
    
    
    df <- data.frame(outcome = c("Test Postive", "Test Negative", "Total"),
      Disease = c(TP, FN, totDis), 
      No_Disease = c(FP, TN, totNodis),
      total = c(TP+FP, TN+FN, totDis + totNodis))
    format(df,0)
    })
  
  output$prediction <- renderPlot({

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

