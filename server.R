library(shiny)
shinyServer(function(input, output) {
  model <- reactive({
    cars_data <- brushedPoints(cars, input$brush1,
                                  xvar = "dist", yvar = "speed")
    if(nrow(cars_data) < 2){
      return(NULL)
    }
    lm(speed ~ dist, data = cars_data)
  })
  output$slopeOut <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][2]
    }
  })
  output$intOut <- renderText({
    if(is.null(model())){
      "No Model Found"
    } else {
      model()[[1]][1]
    }
  })
  output$plot1 <- renderPlot({
    plot(cars$dist, cars$speed, xlab = "dist",
         ylab = "speed", main = "Speed and dist of cars",
         cex = 1.5, pch = 16, bty = "n")
    if(!is.null(model())){
      abline(model(), col = "blue", lwd = 2)
    }
  })
})