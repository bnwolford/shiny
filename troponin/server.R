

### Define server logic for phecodes
phecode<-function(input,output,session){
  df<-fread("troponin_variants_HRC_lookup_phenoDefs.txt.gz")
  names(df)<-names(df)%>%make.unique()
  names(df)[1]<-'Chr'
    ##Change this: select columns you care about for analysis
  df<-df%>%select(Chr,POS,ID,REF,ALT,af,num_cases,num_controls,pval,phecode,group,description)%>%
        mutate(p=-log(pval,base=10),
               rowid=seq(n()))
  df<-df%>%arrange(ID,pval)
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
        rsID = df$ID[s]
        if (length(s)) {
            cat('ID: ')
            cat(rsID)
        }else{
            cat('click a variant in the table to subset the data')
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
               'description: ', genedf$description,"</b><br>",
               'AF: ', formatC(signif(genedf$af,digits=3))
        )
    }
    
    vis <- reactive({
        s <- input$mytable1_rows_selected
        threshold<- -log10(0.05/(length(unique(df$ID))*length(unique(df$phecode)))) #dynamic bonferroni threshold for variants x traits 
        if (!is.null(s)){
          rsID = df$ID[s]
          plotdf=df%>%filter(ID==rsID)
        } else{
          plotdf=df
        }
        plotdf$threshold<-threshold
        plotdf %>%
            ggvis(x = ~phecode, y = ~p) %>%
            layer_points(size := 50, size.hover := 200,
                         fill = ~factor(group),
                         fillOpacity := 0.4, 
                         fillOpacity.hover := 0.5,
                         key := ~rowid) %>%
            layer_lines(x= ~phecode, y= ~threshold, strokeDash:=6) %>% 
            add_tooltip(gene_tooltip, "hover") %>%
            #layer_paths(x=~chr.pos,y=~raf,stroke:='lightgrey') %>%
            add_axis("x", title = 'Phecode') %>%
            add_axis("y", title = '-log p value') %>%
            set_options(width = 1000, height = 550) %>% hide_legend('fill')
    })
    
    vis %>% bind_shiny("plot2")
    
}

#define server logic for quantitative 
quant<-function(input,output,session){
  df<-fread("troponin_variants_TOPMED_quant.txt.gz")
  names(df)<-names(df)%>%make.unique()
  names(df)[1]<-'Chr'
  ##Change this: select columns you care about for analysis
  df<-df%>%select(Chr,POS,SNPID,Allele1,Allele2,AF_Allele2,N,p.value,trait)%>%
    mutate(p=-log(p.value,base=10),
           rowid=seq(n()))
  df<-df%>%arrange(SNPID,p.value)
  output$mytable1 <- DT::renderDataTable(
    DT::datatable({df%>%select(Chr,POS,SNPID,Allele1,Allele2,AF_Allele2,N,trait,p.value)%>%unique}, selection = 'single',
                  rownames=FALSE,
                  options = list(
                    digits=3,
                    pageLength=10,
                    order = list(3, 'desc'),
                    lengthMenu = list(c(100, 500, 1000, -1), list('100', '500', '1000','All'))) 
              
    ) %>% formatRound(c("AF_Allele2"),digits=2) %>% formatRound(c("p.value"), digits = 10)
  )
  
  output$x4 = renderPrint({
    s <- input$mytable1_rows_selected
    rsID = df$SNPID[s]
    if (length(s)) {
      cat('SNPID: ')
      cat(rsID)
    }else{
      cat('click a variant in the table to subset the data')
    }
  })
  
  gene_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$rowid)) return(NULL)
    
    genedf <- df[df$rowid == x$rowid, ]
    
    paste0("<b>", genedf$SNPID,"</b><br>",
           #print some stuff from genedf
           'p value: ', formatC(signif(genedf$p.value,digits=3)),"</b><br>",
           "<b>",'trait: ', genedf$trait,"</b><br>",
           'AF: ', formatC(signif(genedf$AF_Allele2,digits=3))
    )
  }
  
  vis <- reactive({
    s <- input$mytable1_rows_selected
    threshold<- -log10(0.05/(length(unique(df$SNPID))*length(unique(df$trait)))) #dynamic bonferroni threshold for variants x traits 
    if (!is.null(s)){
      rsID = df$SNPID[s]
      plotdf=df%>%filter(SNPID==rsID)
    } else{
      plotdf=df
    }
    plotdf$threshold<-threshold
    plotdf %>%
      ggvis(x = ~trait, y = ~p) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.4, 
                   fillOpacity.hover := 0.5,
                   key := ~rowid) %>%
      layer_lines(x= ~trait, y= ~threshold, strokeDash:=6) %>% 
      add_tooltip(gene_tooltip, "hover") %>%
      #layer_paths(x=~chr.pos,y=~raf,stroke:='lightgrey') %>%
      add_axis("x", title = 'Quantitative Trait',title_offset=150,
               properties = axis_props(labels = list(angle = 45, align = "left", baseline = "middle"))) %>%
      add_axis("y", title = '-log p value') %>%
      set_options(width = 1000, height = 550) %>% hide_legend('fill')
  })
  
  vis %>% bind_shiny("plot2")
  
}



function(input, output, session) {
  observeEvent(input$go,{
    if (input$dataset=="Phecode"){
      phecode(input,output,session)
    } else if (input$dataset=="Quantitative"){
      quant(input,output,session)
    }
  })
}

#shinyServer(server)
