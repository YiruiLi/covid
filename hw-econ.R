library(shiny)
library(ggplot2)

data2 <- read.csv("BTC.csv")
data2.completed <- na.omit(data2)

u<- shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do2", "Make 3 plots", value = T)
                ),
                mainPanel("main panel",
                          fluidRow(
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(100,100,100), 
                                        plotOutput("plotgraph1"), 
                                        plotOutput("plotgraph2"),
                                        plotOutput("plotgraph3")
                            )
                          )
                )
  )
)
)
s <- shinyServer(function(input, output){
  time = c(1:366)
  open_price = as.factor(data2.completed$Open)
  close_price = as.factor(data2.completed$Close)
  high_proce = as.factor(data2.completed$High)
  pt1 <- plot(x=data.frame(time=time, open_price=open_price))
  pt3 <- plot(x=data.frame(time=time, close_price=close_price))
  pt2 <- plot(x=data.frame(time=time, high_proce=high_proce))
  output$plotgraph1 = renderPlot({pt1})
  output$plotgraph2 = renderPlot({pt2})
  output$plotgraph3 = renderPlot({pt3}
  )
})

shinyApp(u,s)
