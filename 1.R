data <- read.csv("national-history.csv")
data.completed <- na.omit(data)
data2 <- read.csv("BTC.csv")
data2.completed <- na.omit(data2)
time <- data.completed$date
case <- data.completed$positive
date <- data2.completed$Date
close_price = as.factor(data2.completed$Close)

if (interactive()) {
  # table example
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(12,
               tableOutput('table')
        )
      )
    ),
    server = function(input, output) {
      output$table <- renderTable(data.frame(time = time, case = case, date = date[1:341], close_price = close_price[1:341]))
    }
  )
  
  
  # DataTables example
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(12,
               dataTableOutput('table')
        )
      )
    ),
    server = function(input, output) {
      output$table <- renderDataTable(data.frame(time = time, case = case, date = date[1:341], close_price = close_price[1:341]))
    }
  )
}
