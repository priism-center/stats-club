library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Distribution specifications"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("dist", "Distribution:",
                  list("Normal" = "normal",
                       "Uniform" = "uniform", "Beta" = "beta")),
      
      
      conditionalPanel(
        condition = "input.dist == 'normal'",
        sliderInput("mean",
                    "Value of mu:",
                    min = -20,
                    max = 20,
                    value = 0),
        
        sliderInput("sigma",
                    "Value of sigma:",
                    min = 1,
                    max = 10,
                    value = 1)
        
      ),
      
      conditionalPanel(
        condition = "input.dist == 'uniform'",
        sliderInput("min",
                    "Value of min:",
                    min = -20,
                    max = 20,
                    value = 0),
        
        sliderInput("max",
                    "Value of max:",
                    min = -10,
                    max = 30,
                    value = 1)
        
      ),
      
      
      conditionalPanel(
        condition = "input.dist == 'beta'",
        sliderInput("alpha",
                    "Value of alpha:",
                    min = 0.5,
                    max = 100,
                    value = 1),
        
        sliderInput("beta",
                    "Value of beta:",
                    min = 0.5,
                    max = 100,
                    value = 1)
        
      ),
      
      
      
      sliderInput("n",
                  "Sample size",
                  min = 5,
                  max = 2000,
                  value = 500),
      
      sliderInput("numsamples",
                  "Number of samples:",
                  min = 5,
                  max = 2000,
                  value = 500)
    ),
    
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("newPlot")
      
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    if (input$dist == "normal"){
      x    <- rnorm(input$n, mean = input$mean, sd = input$sigma)
    }
    else if (input$dist == "uniform"){
      
      x    <- runif(input$n, min = input$min, max = input$max)
    }
    
    else if (input$dist == "beta"){
      
      x    <- rbeta(input$n, input$alpha, input$beta)
    }
    
    
    # draw the histogram with the specified number of bins
    plot_title = sprintf("Density from a %s with mean %i and Sd %i", input$dist, input$mean, input$sigma)
    plot(density(x), main = plot_title)
  })
  
  
  output$newPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    means = vector()
    for(i in 1:input$numsamples){
      
      if (input$dist == "normal"){
        means[i] = mean(rnorm(input$n, mean = input$mean, sd = input$sigma))
      }
      
      else if(input$dist == "uniform"){
        
        means[i] = mean(runif(input$n, min = input$min, max = input$max))
        
      }
      
      else if(input$dist == "beta"){
        
        means[i] = mean(rbeta(input$n,  input$alpha , input$beta))
      }
      
      
    }
    
    hist_title = sprintf("Histogram of the means of %i samples from %s distribution", input$numsamples, input$dist)
    
    hist(means, main = hist_title)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
