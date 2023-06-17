library(shiny)
library(car)
#app_dir = system.file("functions", package = mypackage)
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

server <- function(input, output, session) {

  formula = reactive({
    as.formula(paste0("arrest~", paste(input$fields, collapse = "+")))
  })

  fit = eventReactive(input$regression, {
    LRFit(train, formula())
  })

  output$CADPlot = renderPlot({
    CADPlot(df_clean)
  })

  output$CFPlot = renderPlot({
    CFPlot(df_clean)
  })

  output$TRCLPlot = renderPlot({
    TRCLPlot(df_clean)
  })

  output$CSYPlot = renderPlot({
    CSYPlot(df_clean)
  })

  output$CSPlot = renderPlot({
    CSPlot(df_clean)
  })

  output$ICTSPlot = renderPlot({
    ICTSPlot(df_clean)
  })

  output$summary = renderPrint({
    summary(fit())
  })

  output$AVPlot = renderPlot({
    avPlots(fit())
  })

}

shinyApp(ui, server)
