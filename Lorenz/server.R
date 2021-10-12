#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(deSolve)
library(ggplot2)
library(highcharter)

Lorenz <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
        dXdt <- sigma * (Y - X)
        dYdt <- X * (rho - Z) - Y
        dZdt <- X * Y - beta * Z
        list(c(dXdt, dYdt, dZdt))
    })
}


plot_time_series <- function(df, x, y, line_col){
    ggplot(df, aes_string(x = x, y = y)) + geom_line(col = line_col) + theme_bw()
}

plot_trajectory <- function(df, x, y, line_col){
    ggplot(df, aes_string(x = x, y = y)) + geom_point(col = line_col) + theme_bw()
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    parameters <- list()
    times <- c()
    state <- c()
    
    observe({
        cat("setting parameters \n")
        
        parameters <<- list("sigma" = input$sigma,
                            "rho" = input$rho,
                            "beta" = input$beta)
        times <<- seq(0, input$time, by = input$dt)
        state <<- c(X = input$X, Y = input$Y, Z = input$Z)
        
    })
    
    # observeEvent(input$go,{
    #     cat("setting parameters \n")
    #     parameters <<- list("sigma" = input$sigma,
    #                        "rho" = input$rho,
    #                        "beta" = input$beta)
    #     times <<- seq(0, input$time, by = 0.01)
    #     state <<- c(X = input$X, Y = input$Y, Z = input$Z)
    #     
    # })
    
    solution <- reactiveValues(solution = NULL)
    observeEvent(input$solve,{
        solution[["solution"]] <- NULL
        cat("solving ode \n")
        solution[["solution"]] <<- as.data.frame(ode(y = state, times = times, func = Lorenz, parms = parameters))
        cat("---- solved ode \n")
    })
    
    
    output$x_plot <- renderPlot({
        if(!is.null(solution[["solution"]])){
            cat("plotting \n")
            plot_time_series(solution[["solution"]], x = "time", y = "X",line_col = "green4")
        }
    })

    output$y_plot <- renderPlot({
        if(!is.null(solution[["solution"]])){
            cat("plotting \n")
            plot_time_series(solution[["solution"]], x = "time", y = "Y", line_col = "red4")
        }
    })
    
    output$z_plot <- renderPlot({
        if(!is.null(solution[["solution"]])){
            cat("plotting \n")
           plot_time_series(solution[["solution"]], x = "time", y = "Z", line_col = "blue4")
        }
    })
    
    output$xy_plot <- renderPlot({
        if(!is.null(solution[["solution"]])){
            cat("plotting \n")
            plot_trajectory(solution[["solution"]], x = "X", y = "Y", line_col = "gray4")
        }
    })
    
    output$xz_plot <- renderPlot({
        if(!is.null(solution[["solution"]])){
            cat("plotting \n")
            plot_trajectory(solution[["solution"]], x = "X", y = "Z", line_col = "gray4")
        }
    })
        
    output$yz_plot <- renderPlot({
        if(!is.null(solution[["solution"]])){
            cat("plotting \n")
            plot_trajectory(solution[["solution"]], x = "Y", y = "Z", line_col = "gray4")
        }
    })
    
    # output$x_plot_hc <- renderHighchart({
    #     
    #     highchart() %>% 
    #         hc_add_series(data = solution[["solution"]], hcaes(x = time, y = X), type = "line", size = 0.1)     
    #     
    # })
    # 
    # output$y_plot_hc <- renderHighchart({
    #     
    #     highchart() %>% 
    #         hc_add_series(data = solution[["solution"]], hcaes(x = time, y = Y), type = "line", size = 0.1)     
    #     
    # })
    # 
    # output$z_plot_hc <- renderHighchart({
    #     
    #     highchart() %>% 
    #         hc_add_series(data = solution[["solution"]], hcaes(x = time, y = Z), type = "line", size = 0.1)     
    #     
    # })
    
})
