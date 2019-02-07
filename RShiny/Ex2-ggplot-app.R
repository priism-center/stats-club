#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Distributions CLT"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(
          "Distribution", "Distribution",
          c(Normal = "rnorm",
            Uniform = "runif",
            Beta = "rbeta")),
        
        # Only show this panel if Normal is selected
        conditionalPanel(
          condition = "input.Distribution == 'rnorm'",
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
          sliderInput("n1",
                      "Sample size",
                      min = 5,
                      max = 2000,
                      value = 500),
          
          sliderInput("numsamples1",
                      "Number of samples:",
                      min = 5,
                      max = 2000,
                      value = 500)
        ),
        # Only show this panel if Uniform is selected
        conditionalPanel(
          condition = "input.Distribution == 'rbeta'",
          sliderInput("alpha",
                      "Value of alpha:",
                      min = 0.5,
                      max = 100,
                      value = 1),
          
          sliderInput("beta",
                      "Value of beta:",
                      min = 0.5,
                      max = 100,
                      value = 10),
          
          sliderInput("n2",
                      "Sample size",
                      min = 5,
                      max = 2000,
                      value = 500),
          
          sliderInput("numsamples2",
                      "Number of samples:",
                      min = 5,
                      max = 2000,
                      value = 500)),
          # Only show this panel if Uniform is selected
          conditionalPanel(
            condition = "input.Distribution == 'runif'",
            sliderInput("min",
                        "Minimum value:",
                        min = 1,
                        max = 100,
                        value = 1),
            
            sliderInput("max",
                        "Maximum value:",
                        min = 1,
                        max = 100,
                        value = 10),
            
            sliderInput("n3",
                        "Sample size",
                        min = 5,
                        max = 2000,
                        value = 500),
            
            sliderInput("numsamples3",
                        "Number of samples:",
                        min = 5,
                        max = 2000,
                        value = 500)
          
          )
        )
      ,
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotOutput("newPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$distPlot <- renderPlot({
      if(input$Distribution=="rnorm"){
       # generate bins based on input$bins from ui.R
       x    <- rnorm(input$n1, mean = input$mean, sd = input$sigma)
       
       
       plot_title = sprintf("Density from a Normal with mean %i and Sd %i", input$mean, input$sigma)
       #plot(density(x), main = plot_title)
       ggplot(as.data.frame(x), aes(x=x))+
         geom_density() +
         labs(title=plot_title)
    }
      else if (input$Distribution=="runif"){
      #get data
      x <- runif(input$n2, min=input$min, max=input$max)
      plot_title = sprintf("Density from a Uniform with min %i and max %i", input$min, input$max)
      #plot(density(x), main = plot_title)
      ggplot(as.data.frame(x), aes(x=x))+
        geom_density() +
        labs(title=plot_title)
      }
      else if (input$Distribution=="rbeta"){
        #get data
        x <- rbeta(input$n3, input$alpha, input$beta)
        
        #plot(density(x), main = plot_title)
        ggplot(as.data.frame(x), aes(x=x))+
          geom_density() +
          labs(title=paste("Density of a Beta with alpha =", input$alpha, "and beta =",input$beta))
      }
     })
     
     
     output$newPlot <- renderPlot({
       if(input$Distribution=="rnorm"){
       # generate bins based on input$bins from ui.R
       means = vector()
       for(i in 1:input$numsamples1){
         
         means[i] = mean(rnorm(input$n1, mean = input$mean, sd = input$sigma))
         
       }
       
       #hist_title = sprintf("Histogram of the means of %i samples", input$numsamples)
       
       #hist(means, main = hist_title)
       ggplot(as.data.frame(means), aes(x=means))+
         geom_histogram()+
         labs(title=paste("Histogram of the means of", input$numsamples1, "samples"))
       }
       else if (input$Distribution=="runif"){
         means = vector()
         for(i in 1:input$numsamples3){
           
           means[i] = mean(runif(input$n3,min=input$min, max=input$max))
           
         }
         
         #hist_title = sprintf("Histogram of the means of %i samples", input$numsamples)
         
         #hist(means, main = hist_title)
         ggplot(as.data.frame(means), aes(x=means))+
           geom_histogram()+
           labs(title=paste("Histogram of the means of", input$numsamples3, "samples"))
       }
       else if (input$Distribution=="rbeta"){
         means = vector()
         for(i in 1:input$numsamples2){
           
           means[i] = mean(rbeta(input$n2,shape1=input$alpha, shape2=input$beta))
           
         }
         
         #hist_title = sprintf("Histogram of the means of %i samples", input$numsamples)
         
         #hist(means, main = hist_title)
         ggplot(as.data.frame(means), aes(x=means))+
           geom_histogram()+
           labs(title=paste("Histogram of the means of", input$numsamples2, "samples"))
       }
     })
     
   }
   
   
   
   # Run the application 
   shinyApp(ui = ui, server = server)
