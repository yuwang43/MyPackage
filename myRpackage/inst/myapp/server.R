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

