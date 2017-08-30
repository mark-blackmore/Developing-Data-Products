# Courera Data Science Specialization
# Developing Data Products
# Course Project - Shiny Web App

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Disease Screening Tests: After a test, what are the chances that you actually have a disease?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Prevalence: Percentage in a population that have a certain disease"),
        numericInput("prev", "Input: Prevalence (%)", 
                     min = 0, 
                     max = 100,
                     value = 9),
        br(),
        h4("Sensitivity: Diagnostic Test's True Positive Rate "),
        sliderInput("sens","Input: Sensitivity (%):",
                     min = 0,
                     max = 100,
                     value = 41),
        br(),
        h4("Specificity: Diagnostic Test's True Neagative Rate "),
        sliderInput("spec","Input: Specificity (%):",
                     min = 0,
                     max = 100,
                     value = 93),
        h5("Example:"),
        h5("Diabetes has a 9% prevalence, and a typical screening test has 41% sensitivity and 93% specificity"),
        a("https://academic.oup.com/epirev/article/33/1/63/483202/Screening-for-Type-2-Diabetes-and-Dysglycemia")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        strong(h3("Cases per 100,000 people")), 
        tableOutput("confusion"),
        br(),
        h4("False Omission Rate (FOR) - Given a negative test, what is the probability that you have the condition"),
        h4("Positive Predictive Value (PPV): Given a positive test, what is the probaility that you have the condition"),
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
    
    
    df <- data.frame(Outcome = c("Test Postive", "Test Negative", "Total"),
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
    
    df <- data.frame(Outcome = c("Test Postive", "Test Negative", "Total"),
                     Disease = c(TP, FN, totDis), 
                     No_Disease = c(FP, TN, totNodis),
                     Total = c(TP+FP, TN+FN, totDis + totNodis))
    df2 <- data.frame(Outcome = c("PPV", "FOR", "Prevalence"),
                      metric = round(100 * df$Disease/df$Total,2))
    
    ggplot(df2, aes(x = Outcome, y = metric)) + geom_bar(stat = "identity", fill="steelblue")+
      geom_text(aes(label = metric), vjust=-0.3, size = 4) +
      theme_minimal()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

