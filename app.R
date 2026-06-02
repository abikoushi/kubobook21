#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("架空植物の種子数（久保拓也. 2012. データ解析のための統計モデリング入門. 岩波書店. 2章より）"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          actionButton("do", "click!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("data", inline = FALSE) ,
           textOutput("sim", inline = FALSE) 
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  load("data/data.RData")
  output$data <- renderText({paste("data:", paste(data, collapse = ", "), "\n")})
  m <- mean(data)
  tab <- table(data)
  x = as.integer(names(tab))
  prob = as.numeric(tab/sum(tab))
  
  output$distPlot <- renderPlot({
      plot(x, prob, ylab="probability", xlim=c(0,11), type="h", ylim=c(0,0.3))
      points(0:11, dpois(0:11,m), type = "o", lty=2)
      legend("topright", c("data", "Poisson dist."), pch=c(NA,1), lty=c(1,2), bty = "n")
    })
  
  
  observeEvent(input$do, {
    rand <- rpois(50, m)
    output$distPlot <- renderPlot({
      plot(x, prob, ylab="probability", xlim=c(0,11), type="h", ylim=c(0,0.3))
      points(0:11, dpois(0:11,m), type = "o", lty=2)
      points(rand, runif(50, 0, 0.01), pch = 4)
      legend("topright", c("data", "Poisson dist.","simulation"), pch=c(NA,1,4), lty=c(1,2,NA), bty = "n")
    })
    output$sim <- renderText({paste("simulation:", paste(rand, collapse = ", "))})
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
