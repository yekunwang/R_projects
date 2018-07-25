library(ggplot2)
library(data.table)
library(magrittr)
library(bayesAB)
library(shiny)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("A/B Test Simulation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Sample Size:",
                  min = 1,
                  max = 5000,
                  value = 200),
      actionButton("action", "Simulate")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
        tabPanel("Two Sample T-test", 
                 plotOutput(outputId = "sampleTimes"),
                 plotOutput(outputId = "sampleHist"),
                 verbatimTextOutput("tSummary"),
                 verbatimTextOutput("tSampleSize")
                 
        ), 
        
        tabPanel("Bootstrap Means",
                 plotOutput(outputId = "bootsHist"),
                 verbatimTextOutput("bootsSummary")
        ),
        
        tabPanel("Bayesian Test",
                 verbatimTextOutput("bayesSummary"),
                 plotOutput(outputId = "bayesPlot")
                 
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  set.seed(5)
  sampleDat=data.frame(A=rpois(5000, 1.1),
             B=rpois(5000, 1.2))
  
  dataInput = reactive({
    input$action
    isolate({ 
      return(sampleDat[1:input$n,])
    })
  })
  
  output$sampleTimes = renderPlot({
    input$action
    ggplot() +
      geom_line(data=data.frame(x=seq_along(dataInput()$A),dataInput()$A),aes(x=x,y=dataInput()$A),alpha=0.6) +
      geom_line(data=data.frame(x=seq_along(dataInput()$B),dataInput()$B),aes(x=x,y=dataInput()$B),alpha=0.6,color="blue") +
      ylab("counts") + 
      xlab("time unit")
  })
  
  output$sampleHist = renderPlot({
    input$action
    ggplot(dataInput()) +
      geom_histogram(data=data.frame(group="A",dataInput()$A),aes(dataInput()$A), alpha = 0.2) +
      geom_histogram(data=data.frame(group="B",dataInput()$B),aes(dataInput()$B), fill="blue",alpha = 0.2)+
      xlab("Poisson(X)")
  })
  
  output$tSummary = renderPrint({
    t.test(dataInput()$A,dataInput()$B,alternative="two.sided",conf.level = 0.9)
  })
  
  
  output$tSampleSize = renderText({
    var_pooled = (var(dataInput()$A)*(length(dataInput()$A)-1) + var(dataInput()$B)*(length(dataInput()$B)-1))/(length(dataInput()$A) + length(dataInput()$B) -2)
    paste("Sample Size base on alpha = 0.1 and power = 0.8:",(test_sample_size = var_pooled/0.5^2*(qnorm(0.95)-qnorm(0.2))^2*2))
  })
  
  bootsData = reactive({
    input$action
    isolate({ 
    n = 100
    A_sample_mu = rep(NA,n)
    for(i in 1:n){
      A_sample_mu[i] = mean(sample(dataInput()$A,n,replace=TRUE))
    }
    
    B_sample_mu = rep(NA,n)
    for(i in 1:n){
      B_sample_mu[i] = mean(sample(dataInput()$B,n,replace=TRUE))
    }
    return(data.frame(A_sample_mu,B_sample_mu)) 
    })
  })
             
  output$bootsHist = renderPlot({
    ggplot() +
      geom_histogram(data=data.frame(group="A",bootsData()$A_sample_mu),aes(bootsData()$A_sample_mu), alpha = 0.2) +
      geom_histogram(data=data.frame(group="B",bootsData()$B_sample_mu),aes(bootsData()$B_sample_mu), fill="blue",alpha = 0.2) +
      xlab("Sample Mean Distribution")
  })
  
  output$bootsSummary = renderPrint({
    t.test(bootsData()$A_sample_mu,bootsData()$B_sample_mu,alternative="two.sided")
  })
  
  output$bayesSummary = renderPrint({
    bay_test = bayesTest(dataInput()$A,dataInput()$B, priors = c('shape' = 1, 'rate' = 1), distribution = 'poisson')
    summary(bay_test)
  })
  
  output$bayesPlot = renderPlot({
    bay_test = bayesTest(dataInput()$A,dataInput()$B, priors = c('shape' = 1, 'rate' = 1), distribution = 'poisson')
    plot(bay_test)[[3]]
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

