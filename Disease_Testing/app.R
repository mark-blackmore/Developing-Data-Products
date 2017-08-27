# Courera Data Science Specialization
# Developing Data Products
# Course Project - Shiny Web App

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
        h3("Plot"),
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
      Total = c(TP+FP, TN+FN, totDis + totNodis))
    format(df)
    })
  
  output$prediction <- renderPlot({
    n      <- 100000
    totDis <- input$prev/100 * n
    TP     <- floor(totDis * input$sens/100)
    FN     <- totDis - TP
    totNodis <- 100000 - totDis
    TN     <- floor(totNodis * input$spec/100)
    FP     <- totNodis - TN
    
    df <- data.frame(outcome = c("Test Postive", "Test Negative", "Total"),
                     Disease = c(TP, FN, totDis), 
                     No_Disease = c(FP, TN, totNodis),
                     Total = c(TP+FP, TN+FN, totDis + totNodis))
    df2 <- data.frame(outcome = c("PPV", "FOR", "Prevalence"),
                      metric = 100 * df$Disease/df$Total)
    
    ggplot(df2, aes(x = outcome, y = metric)) + geom_bar(stat = "identity", fill="steelblue")+
      geom_text(aes(label = metric), vjust=-0.3, size = 4) +
      theme_minimal()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

