library(shiny)
require(ggplot2)
require(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Hello Shiny World!"),

    # Sidebar with a slider input for the number of bins
	sidebarPanel(
	    sliderInput("num",
		        "Number of Samples:",
		        min = 1,
		        max = 5000,
		        value = 2500),
	    radioButtons("distribution", label="Select a distribution",
	                 choices=c("Normal", "Gamma"), selected="Normal"),
	    conditionalPanel(
	      condition = "input.distribution== 'Normal'",
  	    numericInput("mean", "Mean: ", value=0),
	      numericInput("sd", "Standard deviation:",
	                 value = 1, min=0.0001)
	      ),
	    conditionalPanel(
	      condition = "input.distribution== 'Gamma'",
	      numericInput("shape", "Shape: ", value=3),
	      numericInput("rate", "Rate:",
	                 value = 1, min=0.0001))
	),

	# Show a plot of the generated distribution
	mainPanel(
	    plotOutput("hist1"),
	    plotOutput("hist2")
	)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$hist1 <- renderPlot({
      dist <- NULL
      lower <- -10
       if(input$distribution == "Normal")
        dist <- rnorm(n = input$num, mean = input$mean, sd = input$sd)
       else {
        dist <- rgamma(n = input$num, shape = input$shape, rate = input$rate)
        lower <- 0
       }
        gg <- data.frame(dist) %>%
          ggplot(aes(x = dist)) + geom_histogram(binwidth = 0.25) +
          xlim(c(lower,10))
        print(gg)
    })

    output$hist2 <- renderPlot({
      dist <- NULL
      if(input$distribution == "Normal")
        dist <- rnorm(n = input$num, mean = input$mean, sd = input$sd)
      else
        dist <- rgamma(n = input$num, shape = input$shape, rate = input$rate)
      gg <- data.frame(dist) %>%
        ggplot(aes(x = dist)) + geom_histogram(binwidth = 0.25) +
        xlim(c(-10,10))
      print(gg)
    })
}

# Bind ui and server together
shinyApp(ui, server)

