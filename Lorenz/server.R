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
library(plotly)

Lorenz <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
        dXdt <- sigma * (Y - X)
        dYdt <- X * (rho - Z) - Y
        dZdt <- X * Y - beta * Z
        list(c(dXdt, dYdt, dZdt))
    })
}


# plot_time_series <- function(df, x, y, line_col){
#     ggplot(df, aes_string(x = x, y = y)) + geom_line(col = line_col) + theme_bw()
# }

plotly_time_series <- function(df, x, y, z, t){
    plot_ly(df) %>%
        add_lines(x = ~df[,t], y = ~df[,x], name = "X(t)") %>%
        add_lines(x = ~df[,t], y = ~df[,y], name = "Y(t)") %>%
        add_lines(x = ~df[,t], y = ~df[,z], name = "Z(t)") %>%
        layout(title = "Time series", 
               xaxis = list(title = "Time"), 
               yaxis = list(title = "X, Y, Z "),
               legend = list(orientation = 'h')
               
               )
    
}


# plot_trajectory <- function(df, x, y, line_col){
#     ggplot(df, aes_string(x = x, y = y)) + geom_point(col = line_col) + theme_bw()
# }

plotly_2d_trajectory <- function(df, x, y, state){
    plot_ly() %>%
        add_paths(data = df, x =  df[,x], y = df[,y], name = "trajectory") %>%
        add_markers(x = ~state[x], y = ~ state[y] , name = "initial_state") %>%
        plotly::layout(title = paste0('Lorenz trajectory ',x," - ",y), 
                       xaxis = list(title = x), 
                       yaxis = list(title = y),
                       legend = list(orientation = 'h')
        )
}

plotly_trajectory <- function(df, x, y, z, state, line_col){
    plot_ly() %>%
        add_paths(data = df, x =  df[,x], y = df[,y], z = df[,z], name = "trajectory") %>%
        add_markers(x = ~state[1], y = ~ state[2], z =~ state[3] , name = "initial_state") %>%
        plotly::layout(title = 'Lorenz trajectory', 
                       scene= list(xaxis = list(title = 'X'), 
                                   yaxis = list(title = 'Y'),
                                   zaxis = list(title = 'Z')
                       ),
                       legend = list(orientation = 'h')
        )
    
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
    
    solution <- reactiveValues(solution = NULL)
    observeEvent(input$solve,{
        solution[["solution"]] <- NULL
        cat("solving ode \n")
        solution[["solution"]] <<- as.data.frame(ode(y = state, times = times, func = Lorenz, parms = parameters))
        cat("---- solved ode \n")
    })
    
    
    output$plotly_time_series <- renderPlotly({
        if(!is.null(solution[["solution"]])){
        plotly_time_series(solution[["solution"]], x = "X", y = "Y", z = "Z", t = "time")
        }
    })
    
    output$plotly_3d <- renderPlotly({
        if(!is.null(solution[["solution"]])){
        plotly_trajectory(solution[["solution"]], x = "Y", y = "Z",z = "Z", state = state, line_col = "gray4")
        }
    })
    
    
    output$plotly_xy <- renderPlotly({
        if(!is.null(solution[["solution"]])){
        plotly_2d_trajectory(df = solution[["solution"]], x = "X", y = "Y", state = state)
        }
    })
    
    output$plotly_xz <- renderPlotly({
        if(!is.null(solution[["solution"]])){
        plotly_2d_trajectory(df = solution[["solution"]], x = "X", y = "Z", state = state)
        }
    })
    
    output$plotly_yz <- renderPlotly({
        if(!is.null(solution[["solution"]])){
        plotly_2d_trajectory(df = solution[["solution"]], x = "Y", y = "Z", state = state)
        }
    })
    
})
