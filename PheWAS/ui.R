
### Define UI logic
fluidPage(

    titlePanel("PheWAS in HRC imputed UKBB"),
    tableOutput("data"),
    
    sidebarPanel(
      selectizeInput('gene', 'Select gene', choices = c("choose" = "", c("GLYAT","PM20D1"))),
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
