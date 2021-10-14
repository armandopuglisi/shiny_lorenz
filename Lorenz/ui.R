#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Lorenz "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("dt", "dt size", min = 0.001, max = 0.1, value = 0.01, step = 0.001),
            sliderInput("time", "Maximum time", min = 10, max = 50, value = 30, step = 0.01),
            sliderInput("sigma","Sigma parameter", min = 1, max = 30, value = 10, step = 0.01),
            sliderInput("rho","Rho parameter", min = 1, max = 30, value = 28, step = 0.01),
            sliderInput("beta","Beta parameter", min = 0, max = 10, value = 8/3, step = 0.01),
            sliderInput("X","X initial state", min = 0, max = 10, value = 1, step = 0.01),
            sliderInput("Y","Y initial state", min = 0, max = 10, value = 1, step = 0.01),
            sliderInput("Z","Z initial state", min = 0, max = 10, value = 1, step = 0.01)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            actionButton("solve", "Solve Lorenz"),
            fluidRow(
                column(6, plotlyOutput("plotly_time_series")),
                column(6, plotlyOutput("plotly_3d"))
            ),
            fluidRow(
                column(4, plotlyOutput("plotly_xy")),
                column(4, plotlyOutput("plotly_xz")),
                column(4, plotlyOutput("plotly_yz"))
            )
        )
    )
))
