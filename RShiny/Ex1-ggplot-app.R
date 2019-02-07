library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Distribution"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("mean",
                  "Value of mu:",
                  min = -20,
                  max = 20,
                  value = 0),
      
      sliderInput("sigma",
                  "Value of sigma:",
                  min = 1,
                  max = 10,
                  value = 1),
      
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
    x    <- rnorm(input$n, mean = input$mean, sd = input$sigma)
    
    
    # draw the histogram with the specified number of bins
    plot_title = sprintf("Density from a Normal with mean %i and Sd %i", input$mean, input$sigma)
    #plot(density(x), main = plot_title)
    ggplot(as.data.frame(x), aes(x=x))+
      geom_density() +
      labs(title=plot_title)
  })
  
  
  output$newPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    means = vector()
    for(i in 1:input$numsamples){
      
      means[i] = mean(rnorm(input$n, mean = input$mean, sd = input$sigma))
      
    }
    
    #hist_title = sprintf("Histogram of the means of %i samples", input$numsamples)
    
    #hist(means, main = hist_title)
    ggplot(as.data.frame(means), aes(x=means))+
      geom_histogram()+
      labs(title=paste("Histogram of the means of", input$numsamples, "samples"))
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
