library(shiny)
library(car)
source("~/Documents/GitHub/group-project-team-13/final-report/mypackage/R/functions.R")
#source()

ui <- fluidPage(

    titlePanel("Chicago Crime Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "fields", label = "Select independent variables", choices = colnames(df_new)[-1], selected = "", multiple = TRUE),
            actionButton(inputId = "regression", label = "Run Regression")
        ),
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Plot of crimes and arrest by date", plotOutput("CADPlot")),
                tabPanel("Plot of crimes frequency", plotOutput("CFPlot")),
                tabPanel("Plot of number of top reported crimes location", plotOutput("TRCLPlot")),
                tabPanel("Plot of number of crimes by season and year", plotOutput("CSYPlot")),
                tabPanel("Plot of crimes types by season", plotOutput("CSPlot")),
                tabPanel("Plot of number of each individual crime type by season", plotOutput("ICTSPlot")),
                tabPanel("Summary of model fit", verbatimTextOutput("summary")),
                tabPanel("Plot of model fit", plotOutput("AVPlot"))
            )
        )
    )
)
