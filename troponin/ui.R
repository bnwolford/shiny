
### Define UI logic
fluidPage(

    titlePanel("Phecodes in UKBB"),
    tableOutput("data"),
    
    mainPanel(
        ggvisOutput("plot2"),
        DT::dataTableOutput("mytable1")
    ),
    fluidRow(
        column(3, verbatimTextOutput('x4'))
    )
)


#shinyUI(ui)