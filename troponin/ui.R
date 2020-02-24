
### Define UI logic
fluidPage(
    titlePanel("Troponin Top Variant PheWAS + Quant Traits in UKBB"),
    tableOutput("data"),
    
    sidebarPanel(
      selectizeInput('dataset', 'Select dataset', choices = c("choose" = "", c("Quantitative","Phecode"))),
      actionButton(inputId = "go",label = "Build!")
      ),
    

    mainPanel(
        ggvisOutput("plot2"),
        DT::dataTableOutput("mytable1")
    ),
    fluidRow(
        column(3, verbatimTextOutput('x4'))
    )
)


#shinyUI(ui)
