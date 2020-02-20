library(shiny)
library(ggvis)
library(optparse)
library('data.table')
library('tidyverse')
library('DT')
library('ggvis')

### Define UI logic
ui <- fluidPage(

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


shinyUI(ui)
