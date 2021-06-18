library(shiny)

data <- read.csv("national-history.csv")
data.completed <- na.omit(data)

ui <- fluidPage(
  
  # Application title
  titlePanel("Covid Plot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      plotOutput("distplot")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    time <- data.completed$date
    time = c(1:length(time))
    case <- data.completed$positive
   
    # draw the histogram with the specified number of bins
    plot(x=data.frame(time=time, case=case))
  })
  output$distplot <- renderPlot({
    time <- data.completed$date
    time = c(1:length(time))
    new_death <- data.completed$deathIncrease
    
    # draw the histogram with the specified number of bins
    plot(x=data.frame(time=time, new_death=new_death))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
