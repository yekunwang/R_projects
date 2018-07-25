library(ggplot2)
library(data.table)
library(magrittr)
library(bayesAB)
library(shiny)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Bayesian A/B Test Simulation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("pa", "Probability of Group A:",value=0.015, min = 0.001, max = 0.999),
      numericInput("pb", "Probability of Group B:",value=0.02, min = 0.001, max = 0.999),
      numericInput("n", "Sample Size:",value=5000, min = 50, max = 10000),
      actionButton("makeSample", "Generate Sample"),
      sliderInput("i","Iteration:",min = 1,max = 5000,value = 100)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
        mainPanel(
          plotOutput(outputId = "densityFun"),
          verbatimTextOutput("cost"),
          verbatimTextOutput("credInterval")
          
        )
        
      )
    )
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  A_par = reactive({
    input$makeSample
    
    isolate({ 
      A_imp=rpois(input$n,3)
      A_conv=rbinom(input$n,A_imp, input$pa)
      A_par=data.frame(alpha=c(1,rep(NA,input$n)),beta=c(1,rep(NA,input$n)))
      
      for(i in 1:input$n){
        A_par$alpha[i+1] = A_par$alpha[i] + A_conv[i]
        A_par$beta[i+1] = A_par$beta[i] + A_imp[i]-A_conv[i]
      }
      
      return(A_par)
      
    })
  })
  
  B_par = reactive({
    input$makeSample
    
    isolate({
      B_imp=rpois(input$n,3)
      B_conv=rbinom(input$n,B_imp, input$pb)
      B_par=data.frame(alpha=c(1,rep(NA,input$n)),beta=c(1,rep(NA,input$n)))
      
      for(i in 1:input$n){
        
        B_par$alpha[i+1] = B_par$alpha[i] + B_conv[i]
        B_par$beta[i+1] = B_par$beta[i] + B_imp[i]-B_conv[i]
      }
      
      return(B_par)
      
    })
  })
  
  A = reactive({
    x=seq(0.01,0.15,0.000025)
    A_df=data.frame(x,density=dbeta(x,A_par()$alpha[input$i],A_par()$beta[input$i]))
  })
  
  
  B = reactive({
    x=seq(0.01,0.15,0.000025)
    B_df=data.frame(x=x,density=dbeta(x,B_par()$alpha[input$i],B_par()$beta[input$i]))
  })
  
  output$densityFun = renderPlot({
    ggplot() +
      geom_line(data=A(),aes(x,density), alpha = 0.2) +
      geom_line(data=B(),aes(x,density), alpha = 0.2,color ="blue") 
  })
  
  
  
  A_sim = reactive({
    return(rbeta(1e6, A_par()$alpha[input$i], A_par()$beta[input$i]))
  })
  
  B_sim = reactive({
    return(rbeta(1e6, B_par()$alpha[input$i], B_par()$beta[input$i]))
  })
  
  output$cost = renderPrint({
    print(paste("Probability that A > B:",(sim <- mean(A_sim() > B_sim()))))
  })
  
  
  credible_interval_approx <- function(a, b, c, d) {
    u1 <- a / (a + b)
    u2 <- c / (c + d)
    var1 <- a * b / ((a + b) ^ 2 * (a + b + 1))
    var2 <- c * d / ((c + d) ^ 2 * (c + d + 1))
    
    mu_diff <- u2 - u1
    sd_diff <- sqrt(var1 + var2)
    
    data.frame(posterior = pnorm(0, mu_diff, sd_diff),
               estimate = mu_diff,
               conf.low = qnorm(.025, mu_diff, sd_diff),
               conf.high = qnorm(.975, mu_diff, sd_diff))
  }
  
  output$credInterval = renderPrint({
    credible_interval_approx(A_par()$alpha[input$i],A_par()$beta[input$i],B_par()$alpha[input$i],B_par()$beta[input$i])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

