#!/usr/bin/Rscript

##===============================================================================
## Copyright (C) 2020 Will Zehr Overton and Brooke Wolford
## Lab of Dr. Cristen Willer and Dr. Mike Boehnke
##=============================================================================

######## Load packages ##########
library(shiny)
library(ggvis)
library(optparse)

library('data.table')
library('tidyverse')
library('DT')
library('ggvis')

######## Read in arguments ##########
optionList <- list(
    make_option(c("-f", "--file"), type="character", help="PheWAS file"))

parser <- OptionParser(
    usage="%prog -f <file>",
    option_list=optionList
        )

arguments <- parse_args(parser, positional_arguments=TRUE)


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

### Define server logic required to draw a histogram
server <- function(input, output) {
    ##df<-fread(arguments$options$file)
    df<-fread("../file.txt")
    names(df)<-names(df)%>%make.unique()
    names(df)[1]<-'Chr'
    #Change this: select columns you care about for analysis
    df<-df%>%select(Chr,POS,ID,REF,ALT,af,num_cases,num_controls,pval,phecode,group,description)%>%
        mutate(p=-log(pval,base=10),
               rowid=seq(n()))
    
    
    output$mytable1 <- DT::renderDataTable(
        DT::datatable({df%>%select(Chr,POS,ID,REF,ALT,af,num_cases,num_controls,description,pval)%>%unique}, selection = 'single',
                      rownames=FALSE,
                      options = list(
                          pageLength=10,
                          order = list(3, 'desc'),
                          lengthMenu = list(c(100, 500, 1000, -1), list('100', '500', '1000','All')))
        )
    )
    
    output$x4 = renderPrint({
        s <- input$mytable1_rows_selected
        gene = df$ID[s]
        if (length(s)) {
            cat('ID: ')
            cat(gene)
        }else{
            cat('click a variant!')
        }
    })
    
    gene_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$rowid)) return(NULL)
        
        genedf <- df[df$rowid == x$rowid, ]
        
        paste0("<b>", genedf$ID,"</b><br>",
               #print some stuff from genedf
               'p value: ', formatC(signif(genedf$pval,digits=3)),"</b><br>",
               'phecode: ', formatC(signif(genedf$phecode,digits=3)),"</b><br>",
               "<b>",'group: ', genedf$group,"</b><br>",
               'description: ', genedf$description
        )
    }
    
    vis <- reactive({
        s <- input$mytable1_rows_selected
        s<-ifelse(is.null(s),1,s)
        gene = df$ID[s]
        plotdf=df%>%filter(ID==gene)
        plotdf %>%
            ggvis(x = ~phecode, y = ~p) %>%
            layer_points(size := 50, size.hover := 200,
                         fill = ~factor(group),
                         fillOpacity := 0.4, 
                         fillOpacity.hover := 0.5,
                         key := ~rowid) %>%
            layer_lines(x= ~phecode, y= 5, strokeDash:=6) %>% 
            add_tooltip(gene_tooltip, "hover") %>%
            #layer_paths(x=~chr.pos,y=~raf,stroke:='lightgrey') %>%
            add_axis("x", title = 'Phecode') %>%
            add_axis("y", title = '-log p value') %>%
            set_options(width = 1000, height = 550) %>% hide_legend('fill')
    })
    
    vis %>% bind_shiny("plot2")
    
}

# Run the application 
shinyApp(ui = ui, server = server)
