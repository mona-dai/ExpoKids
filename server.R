server <- function(input, output, session) {
  
  #######################################################################
  #######################################################################
  ##TAB 0 - EXPOKIDS LIFESTAGES
  output$lstab <- renderTable({
    ls_tab <- data.frame(efh, expokids, years)
    colnames(ls_tab) <-
      c("EFH Age Bins", "ExpoKids Lifestage", "Total Years in Lifestage")
    ls_tab
  },
  digits = 0, spacing = c("xs"), width = 800, align = c("c"), rownames =
    FALSE)
  
  ####################################################################### 
  #######################################################################
  ## TAB 1 - UPLOADED TABLE
  output$first_tab <- renderTable({
    req(input$inFile)
    inFile1 <- input$inFile
    readfile <- read_xlsx(inFile1$datapath, 1)
  }, digits = -3)
  
  ####################################################################### 
  ####################################################################### 
  ## TAB 2 - LIFESTAGE TABLES
  # CREATE ADD TABLE BY LIFESTAGES - CHEMFILE
  chemfile <- reactive({ # create reactive function of uploaded data
    req(input$inFile)
    inFile1 <- input$inFile
    datafile <-
      read_xlsx(inFile1$datapath, 1)
    
    chemfile2 <-
      data.frame(matrix(nrow = nrow(datafile), ncol = 3))
    chemfile2[, 1] <- datafile[, 1]
    
    # calc sm infant
    for (i in 2:nrow(datafile)) {
      chemfile2[i, 2] <- ((datafile[1, 2] * datafile[i, 2]) +
                            (datafile[1, 3] * datafile[i, 3]) +
                            (datafile[1, 4] * datafile[i, 4]) +
                            (datafile[1, 5] * datafile[i, 5])
      ) / 1 # sm infant = 1 yr
    }
    
    # calc infant
    for (i in 1:nrow(datafile)) {
      chemfile2[i, 3] <- (datafile[1, 6] * datafile[i, 6] +
                            datafile[1, 7] * datafile[i, 7]) /
        2 # infant = 2 yrs
    }
    
    # copy rest
    for (i in 8:ncol(datafile)) {
      chemfile2[, i - 4] <- datafile[, i]
    }
    
    # RENAME COLUMN HEADERS
    chemfile2 <- chemfile2[2:nrow(chemfile2), ]
    names(chemfile2) <-
      c("Media", "Young Infant", "Infant", "Young Child",
        "Child", "Young Youth", "Youth", "Adult")
    
    # CREATE CHILDHOOD COL
    hood <- data.frame(matrix(nrow = nrow(chemfile2), ncol = 1))
    
    for (i in 1:nrow(chemfile2)) {
      hood[i, 1] <- ((1 * chemfile2[i, 2]) +
                       (2 * chemfile2[i, 3]) +
                       (3 * chemfile2[i, 4]) +
                       (5 * (chemfile2[i, 5] + chemfile2[i, 6] +
                               chemfile2[i, 7]))) / 21 # childhood = 21 yrs
    }
    
    names(hood) <- c("Childhood")
    chemfile <- cbind(chemfile2, hood)
  })
  
  # CREATE LADD TABLE BY LIFESTAGES = WTCHEMFILE
  wtchemfile   <- reactive({
    chemfile   <- chemfile()
    wtchemfile <- chemfile[,1:8]
    for (i in 2:ncol(wtchemfile)) {
      wtchemfile[,i] <- Filter(is.numeric,(wtchemfile[,i]))*lifeyr[i]
    }
    # add childhood col
    wtchemfile$Childhood <- rowSums(Filter(is.numeric,wtchemfile[,1:7]))
    wtchemfile
  })
  
  # ADD LIFETIME TABLE
  finchemfile <- reactive({
    finchemfile <- chemfile()
    wtchemfile  <- wtchemfile()
    #lifetime col
    finchemfile$Lifetime <- rowSums(Filter(is.numeric,wtchemfile[,1:8]))
    finchemfile
  })
  
  ####################################################################### 
  # DISPLAY TABLES
  
  # DISPLAY ADD TABLE
  output$add_tab <- renderTable({
    add_tab <- finchemfile()
    # add agg row to table
    aggrow1 <- colSums(Filter(is.numeric, add_tab))
    add_tab[nrow(add_tab) + 1, 1] <- "Aggregate"
    add_tab[nrow(add_tab), 2:ncol(add_tab)] <- aggrow1
    add_tab
  }, 
  digits=-3,spacing = c("xs"))
  
  # DISPLAY LADD TABLE
  output$ladd_tab <- renderTable({
    ladd_tab <- wtchemfile()
    # add agg row to table
    aggrow<-colSums(Filter(is.numeric,ladd_tab))
    ladd_tab[nrow(ladd_tab)+1,1]<-"Aggregate"
    ladd_tab[nrow(ladd_tab),2:ncol(ladd_tab)]<-aggrow
    ladd_tab
  },
  digits=-3,spacing = c("xs"))
  
  #######################################################################
  ####################################################################### 
  ## GRAPH1 - LIFESTAGE GRAPHS
  # MELT ADD
  meltchemfile <- reactive({
    melt(chemfile(),id.vars = "Media",
         measure.var = c("Young Infant","Infant","Young Child",
                         "Child","Young Youth","Youth","Adult"),
         na.rm = FALSE,
         variable.name = "Lifestage",value.name = "ADD")
  })
  
  ## PLOT ADD BY LIFESTAGE
  output$p_ADD <- renderChart2({
    stack <- nPlot(ADD ~ Lifestage, group = "Media", data = meltchemfile(), 
                   type = "multiBarChart")
    stack$chart(stacked=T,color=colortab1,margin=list(left=85),reduceXTicks=F)
    stack$yAxis(axisLabel = "ADD (mg/kg-day)",width=80,
                tickFormat= "#!function(y){
                return d3.format('.2e')(y)
  }!#")
    stack$xAxis(axisLabel = "Lifestages")
    return(stack)
})
  
  ####################################################################### 
  # MELT LADD
  mwt <- reactive({
    melt(wtchemfile(),id.vars="Media",
         measure.var=c("Young Infant","Infant","Young Child",
                       "Child","Young Youth","Youth","Adult"),
         na.rm=FALSE,
         variable.name="Lifestage",value.name="LADD")
  })
  # PLOT LADD BY LIFESTAGE GRAPH
  output$p_LADD <- renderChart2({
    stack <- nPlot(LADD ~ Lifestage, group = "Media", data = mwt(), 
                   type = "multiBarChart")
    stack$chart(stacked=T,color=colortab1,margin=list(left=85),reduceXTicks=F)
    stack$yAxis(axisLabel = "LADD (mg/kg-day)",width=80,
                tickFormat= "#!function(y){ return d3.format('.2e')(y) }!#")
    stack$xAxis(axisLabel = "Lifestages")
    return(stack)
  })
  
  #######################################################################   
  # CALCULATE & MELT PCT
  mpct <- reactive({
    chemfile <- chemfile()
    lifes <- names(chemfile[2:length(chemfile)])
    pct <- sapply(chemfile[lifes], prop.table)
    pctfile <- chemfile[, 1:ncol(chemfile) - 1]
    pctfile[, 2:ncol(pctfile)] <- pct
    
    melt(pctfile, id.vars = "Media",
         measure.var = c("Young Infant","Infant","Young Child",
                         "Child","Young Youth","Youth","Adult"),
         na.rm = FALSE,
         variable.name = "Lifestage",
         value.name = "Percent"
    )
  })
  
  # LIFESTAGE PIE CHART
  output$p_perlife <- renderUI({
    pielist <- lapply(1:7, function(j) {
      plotpie <- paste("pmedia", j, sep = "")
      showOutput(plotpie,"Highcharts")
    })
    do.call(tagList, pielist)
  })
  
  for (j in 1:7) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_j <- j
      plotpie <- paste("pmedia", my_j, sep = "")
      output[[plotpie]] <- renderChart2({
        chemfile <- chemfile()
        mpct <- mpct()
        lifes <- names(chemfile[2:length(chemfile)])
        lifex <- subset(mpct, mpct[, 2] == lifes[my_j])
        pie_ls <- hPlot(x = "Media",y = "Percent",
                        data = lifex,type = "pie",
                        title = paste("Cumulative Percentage for ", lifex[my_j, 2]))
        pie_ls$colors(colortab2)
        pie_ls$tooltip(
          formatter = "#! function() {
          return '<b>'+ this.point.name +'</b>: '+ Highcharts.numberFormat(this.percentage, 2) +' %';
      } !#"
        )
        return(pie_ls)
      })
    })
  }
  
  ###################################################################
  ###################################################################
  ## GRAPH2 - MEDIA GRAPHS
  ## CREATE CHECKBOXES & SELECT BUTTON
  observe({
    chemfile <- chemfile()
    x <- chemfile[,1]
    if (is.null(x))
      x <- character(0)
    updateCheckboxGroupInput(session,"media2",
                             label="Media",
                             choiceNames = x,
                             choiceValues = x)
  })
  
  # select button
  observeEvent(input$select,{
    chemfile <- chemfile()
    x <- chemfile[,1]
    if (is.null(x))
      x <- character(0)
    updateCheckboxGroupInput(session,"media2",
                             label="Media",
                             choiceNames = x,
                             choiceValues = x,
                             selected = x)
  })
  # deselect button
  observeEvent(input$deselect,{
    chemfile <- chemfile()
    x <- chemfile[,1]
    if (is.null(x))
      x <- character(0)
    updateCheckboxGroupInput(session,"media2",
                             label="Media",
                             choiceNames = x,
                             choiceValues = x)
  })
  ################################################################### 
  ## ADD MEDIA GRAPHS
  # save selected values
  st <- reactiveVal()
  observeEvent(input$button, {
    st(input$media2)
  })
  
  # display UI object
  output$p_addmedia <- renderUI({
    plotList <- lapply(1:length(st()), function(i) {
      plotname <- paste("padd", i, sep = "")
      showOutput(plotname,"nvd3")
    })
    do.call(tagList, plotList)
  })
  
  # save plots into plotList
  plotList <- eventReactive(input$button, {
    st <- st()
    meltchemfile <- meltchemfile()
    plotList <- list()
    for (i in 1:length(st)) {
      mediax <- subset(meltchemfile, meltchemfile$Media == st[i])#meltchemfile[my_i, 1])
      c_i <- match(mediax[1, 1], meltchemfile$Media) # match color
      
      plotm <- nPlot(ADD ~ Lifestage,
                     group = "Media",
                     data = mediax,
                     type = "multiBarChart")
      plotm$chart(
        margin = list(left = 85),
        reduceXTicks = F,
        color = list(colortab1[c_i])
      ) # stacked=F,color=colortab1[my_i],
      plotm$yAxis(axisLabel = "ADD (mg/kg-day)",
                  width = 80,
                  tickFormat = "#!function(y){ return d3.format('.2e')(y) }!#")
      plotm$xAxis(axisLabel = "Lifestages")
      plotm$templates$script <-
        "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      plotm$set(title = paste0("", st[i]))
      
      plotList[[i]] <- plotm
    }
    plotList
  })
  
  # pass plotList to ui
  for (i in 1:length(colortab1)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("padd", my_i, sep = "")
      output[[plotname]] <- renderChart2({
        plotList <- plotList()
        plotList[[my_i]]
      })
    })
  }
  
  #####################################################################
  ## DISPLAY LADD
  # UI object
  output$p_laddmedia <- renderUI({
    plotList1 <- lapply(1:length(st()), function(i) {
      plotname1 <- paste("pladd", i, sep = "")
      showOutput(plotname1,"nvd3")
    })
    do.call(tagList, plotList1)
  })
  
  # create graphs after click
  plotList1 <- eventReactive(input$button, {
    st <- st()
    mwt <- mwt()
    plotList1 <- list()
    for (i in 1:length(st)) {
      mediax <- subset(mwt, mwt$Media == st[i])
      c_i <- match(mediax[1, 1], mwt$Media) # match color
      
      plotl <- nPlot(LADD ~ Lifestage,
                     group = "Media",
                     data = mediax,
                     type = "multiBarChart")
      plotl$chart(
        margin = list(left = 85),
        reduceXTicks = F,
        color = list(colortab1[c_i])
      ) # stacked=F,color=colortab1[my_i],
      plotl$yAxis(axisLabel = "LADD (mg/kg-day)",
                  width = 80,
                  tickFormat = "#!function(y){ return d3.format('.2e')(y) }!#")
      plotl$xAxis(axisLabel = "Lifestages")
      plotl$templates$script <-
        "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
      plotl$set(title = paste0("", st[i]))
      
      plotList1[[i]] <- plotl
    }
    plotList1
  })
  
  # pass list to ui
  for (i in 1:length(colortab1)) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname1 <- paste("pladd", my_i, sep = "")
      output[[plotname1]] <- renderChart2({
        plotList1 <- plotList1()
        plotList1[[my_i]]
      })
    })
  }
  
  #################################################################
  #################################################################
  ## GRAPH3 - PERCENT SUMMARY
  # melt
  msum <- reactive({
    melt(finchemfile(),id.vars="Media",
         measure.var=c("Childhood","Adult","Lifetime"),
         na.rm=FALSE,
         variable.name="Lifestage",value.name="ADD")
  })
  
  # plot
  output$p_sum <- renderChart2({
    stack <- nPlot(ADD ~ Lifestage, group = "Media", data = msum(),
                   type = "multiBarChart")
    stack$chart(stacked=T,color=colortab1,margin=list(left=85),reduceXTicks=F)
    stack$yAxis(axisLabel = "ADD (mg/kg-day)",width=80,
                tickFormat= "#!function(y) {return d3.format('.2e')(y)} !#")
    stack$xAxis(axisLabel = "Lifestages")
    return(stack)
  })
  
  #################################################################
  # melt
  msumpct <- reactive({
    finchemfile <- finchemfile()
    mednoms <- c("Adult","Childhood","Lifetime")
    pct <- data.frame(sapply(finchemfile[mednoms], prop.table))
    pctfile <- cbind(finchemfile[,1],pct)
    colnames(pctfile)[1] <- "Media"
    
    melt(pctfile, id.vars = "Media",
         measure.var = c("Childhood","Adult","Lifetime"),
         na.rm = FALSE,
         variable.name = "Lifestage",
         value.name = "Percent"
    )
  })
  
  # plot
  output$p_persum <- renderChart2({
    stack <- nPlot(Percent ~ Lifestage, group = "Media", data = msumpct(),
                   type = "multiBarChart")
    stack$chart(stacked=T,color=colortab1,margin=list(left=85),reduceXTicks=F)
    stack$yAxis(axisLabel = "Percent (%)",width=80,
                tickFormat= "#!function(y){
                return d3.format(',.1%')(y)
  }!#")
    stack$xAxis(axisLabel = "Lifestages")
    return(stack)
})
  
}