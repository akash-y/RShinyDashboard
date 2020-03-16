install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyjs")
install.packages("shinyWidgets")
install.packages("htmlwidgets")
install.packages("ggplot2")
install.packages("RMySQL")
install.packages("ggthemes")
install.packages("rpivotTable")
install.packages("df2json")
install.packages("scales")
install.packages("tidyverse")
install.packages("base")
install.packages("gmodels")
install.packages("Hmisc")
install.packages("caTools")
install.packages("plotly")


library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(htmlwidgets)
library(ggplot2)
#library(RMySQL)
library(ggthemes)
library(rpivotTable)
library(df2json)
library(scales)
library(tidyverse)
library(base) 
library(gmodels)
library (Hmisc)
library (caTools)
library(plotly)

#source("/home/akash/PerdiX/Shiny - Data Explorer/db_connection_module.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/db_connection_module1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/connectionreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/connectionreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/colchange_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/rowchange_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/colchangereturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/rowchangereturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/target_variable2.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/tarvarreturn.R")
#source("/home/akash/PerdiX/target_variable.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/no_of_variables_valuebox_ui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/no_of_observations_valuebox_ui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_ui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/count_chart.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/countchartreturn_JSON.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/fetch_col.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_return.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_choices.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/fetch_col.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/fetchcallreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/binchoiceui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/bintypeui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/histogramreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/histogramAPI.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/crosstabs.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/crosstabsreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/summarystatsAPI.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/summarystatsreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/stackedchartAPI.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/stackedchartreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/relationshipchart.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/relationshipreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/target_variable1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/targetvar_return.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/scatterchartAPI.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/scatterchartreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/countchartreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/summarystatreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/histogramcountreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/crosstabsreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/stackedchartreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/relationshipreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/tarchange_ui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_choices_new.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_return_new.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_choices_new.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/tarvarreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/tarplotreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/varsel_ui_new.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/tarvarmake_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/tarvarmakereturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/relationshipchart_1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/relationshipchartreturn1.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/binchart_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/returnbin.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/loessreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/loessdata.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/aucdata_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/aucreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/cleanvar_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/cleanvareturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/selcol_fetch_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/selcolfetchreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/crevarreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/cleanvareturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/shorvarui.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/traincreate_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/trainreturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/savedata_API.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/savedatareturn.R")
#source("/home/akash/PerdiX/Shiny - Data Explorer/modelrun_API.R")







ui<- dashboardPage(
  
  dashboardHeader(title = "PerdiX"),
  
  
  dashboardSidebar( 
    
    tags$head(tags$style(".wrapper {overflow: visible !important;}")),
    
    sidebarMenu(
      
      menuItem("Data Upload and Analytics", tabName = "uploaddata", icon = icon("dashboard")),
      menuSubItem("Data Display", tabName = "datadisplay"),
      menuSubItem("Data Explorer", tabName = "dataexplorer"),
      menuSubItem("Build Models", tabName = "modelcreation")
      
    )),
  
  dashboardBody(
    useShinyjs(),
    div( id = "mainbox" ,
         tabItems(
           
           # First tab content
           
           #Displaying Sample Table When This Tab is Open/Clicked
           
           tabItem(tabName = "datadisplay",
                   fluidPage(
                     fluidRow(
                       valueBox(value = tags$p("Loaded Datasets", style = "font-size: 80%;"), "Scores-Delinquencies Information", color = "green", href = NULL, icon = icon("folder-open")),
                       valueBox(value = tags$p("Model:", style = "font-size: 80%;"), "Credit Scoring", color = "red", href = NULL, icon = icon("superscript")),
                       valueBox(value = tags$p("Target Variable:", style = "font-size: 80%;"), "Max Overdue Days", color = "aqua", href = NULL, icon = icon("superscript"))
                     ),
                     fluidRow( 
                       box(width=12,title="Uploaded Data",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           div(style = 'overflow-x: scroll',
                               div(DT::dataTableOutput("sampledata"), style = "font-size: 100%; width: 75%"))
                       )),
                     
                     fluidRow(
                       box(width = 6, title = "Subset Columns",
                           textInput("subcol", "Subset select columns that you want to carry forward for analysis: "),
                           actionButton("colgo", "Subset"),
                           textOutput("coltext")),
                       
                       box(width = 6, title = "Subset Rows",
                           textInput("subrow", "Subset rows from columns that you want to carry forward for analysis: "),
                           actionButton("rowgo", "Subset"),
                           textOutput("rowtext"))
                     ),
                     
                     fluidRow(
                       
                       box(width = 6, title = "Target Variable Calculation",
                           HTML(
                             paste(
                               h5("The target variable selected to carry out the analysis is 'Bad Loans' which is calculated using Max Overdue Days, the formula applied is :"), '</br>',
                               h5("IF(max_od_days > 30) Classify as 1 ELSE IF(max_od_days < 30) Classify as 0")
                             ))
                       ),
                       
                       box(width = 6, title = "Change Target Variable",
                           textInput("subtar", "Make Your Own Target Variable"),
                           actionButton("tarchange1", "Go"),
                           textOutput("tartext")
                       )
                       
                     ),
                     
                     fluidRow(
                       
                       box(width = 12, title = "Target Variable Summary",
                           plotlyOutput("target_variable_summary_plot",height = 200),
                           tableOutput("target_variable_summary")
                       )
                       
                     ))
           ),
           
           tabItem(tabName = "dataexplorer",
                   fluidPage(
                     fluidRow(
                       valueBox(value = tags$p("Loaded Datasets:", style = "font-size: 80%;"), "Scores-Delinquencies Information", color = "green", href = NULL, icon = icon("folder-open")),
                       infoBoxOutput("varlen"),
                       infoBoxOutput("rowlen")
                     ),
                     fluidRow(
                       box(
                         uiOutput("varselui"),
                         actionButton("dispvar", "Go")),
                       
                       box(width = 6, title = "Create Variables",
                           textInput("crevar1", "Create your own variables for the model by combining other variables: "),
                           actionButton("crevargo", "Add to Variable List"),
                           textOutput("crevartext"))
                       
                       
                     ),
                     
                     fluidRow(
                       box(width=12,title="Data Distribution",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           plotlyOutput("countchart")
                       )
                     ),
                     
                     fluidRow(
                       box(width=12,title="Summary Statistics of Selected Variable",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           tableOutput("summarystats")
                       )
                     ),
                     fluidRow(
                       box(width=12,title="Histogram Distribution",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           plotlyOutput("histogramchart")
                       )
                     ),
                     fluidRow(
                       box(width=12,title="Choosing Bins",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           h4("Based on AUC Score and Loess Graph shown below, you can select the number/type of bins that best suit your model:"),
                           uiOutput("binchoiceui"),
                           uiOutput("bintypeui"),
                           actionButton("binchosen", "Go"),
                           actionButton("savedata", "Save the Bins for Analysis"),
                           textOutput("savedataresult")
                           
                       )
                     ),
                     #fluidRow(
                     #box(width=12,title="Comparison of Target Variable Risk Percentiles with Selected Variable Score Percentiles",status="warning",solidHeader=FALSE,collapsible = TRUE,
                     #rpivotTableOutput("crosstabs")
                     #)
                     #),
                     
                     fluidRow(
                       box(width=12,title="Comparison of Target Variable Risk Percentiles with Selected Variable Score Percentiles",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           verbatimTextOutput("crosstabsnew")
                       )
                     ),
                     
                     #fluidRow(
                     #box(width=12,title="Graphical Representation of Comparison of Selected and Target Variables",status="warning",solidHeader=FALSE,collapsible = TRUE,
                     #plotlyOutput("stackedchart")
                     #)
                     #),
                     
                     #fluidRow(
                     #box(width=6,title="Relationship between Selected and Target Variable",status="warning",solidHeader=FALSE,collapsible = TRUE,
                     #plotlyOutput("relationshipgraph")
                     #),
                     
                     fluidRow(
                       box(width=6,title="Relationship between Selected and Target Variable",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           plotlyOutput("loesschart")
                       ),
                       
                       box(width=6,title="Correlation Graph",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           plotlyOutput("scattergraph")
                       )
                     ),
                     
                     fluidRow(
                       box(width=12,title="AUC Score and Correlation",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           plotOutput("auc1"),
                           textOutput("auc2")
                       )
                     ),
                     
                     fluidRow(
                       box(width=12,title="Clean data for this variable and take it forward for analysis:",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           pickerInput("varclean", "Select a method for cleaning:", choices = c("Delete Rows Entirely", "Replace with 0s", "Replace with Mean"), selected = "Delete Rows Entirely", multiple = FALSE),
                           textOutput("cleantext"),
                           actionButton("cleanselect", "Clean and Shortlist"),
                           verbatimTextOutput("markdone")
                       )
                     )
                     
                     
                     
                   )),
           
           tabItem("modelcreation",
                   
                   fluidPage(
                     
                     fluidRow(
                       
                       valueBox(value = tags$p("Model:", style = "font-size: 80%;"), "Credit Scoring", color = "green", href = NULL, icon = icon("folder-open")),
                       infoBoxOutput("shorvar")
                       
                     ),
                     
                     fluidRow(
                       
                       box(width = 6, title = "Divide data for training and testing",
                           sliderInput("traindat", "Provide a percentage range of the original dataset on which you want to train your model: ", min = 0, max = 100, post = "%", value = 70),
                           actionButton("trainsend", "Divide"),
                           textOutput("traintext")),
                       
                       box(width = 6, title = "Model Selection:",
                           pickerInput("modelsel", "Choose amongst the following models for your data: ", choices = c("Logit", "Probit"), selected = NULL, multiple = FALSE),
                           actionButton("compute", "Run the Model"))
                       
                     ),
                     
                     fluidRow(
                       
                       box(width=12,title="Summary of the Model",status="warning",solidHeader=FALSE,collapsible = TRUE,
                           div(style = 'overflow-x: scroll',
                               div(DT::dataTableOutput("modeldata"), style = "font-size: 100%; width: 75%"))
                           
                           
                       ))
                     
                   ))
           
           
           
           
         ))
    
  )
)

server<-function(input,output,session){
  
  myValues<<-reactiveValues()
  
  output$sampledata<-DT::renderDataTable({
    
    colchange<<-input$subcol
    jsoncolchange<-toJSON(colchange)
    rowchange<<-input$subrow
    jsonrowchange<-toJSON(rowchange)
    tarvarchangeui<<-input$subtar
    jsontarvarchangeui<-toJSON(tarvarchangeui)
    crevarchangeui<<-input$crevar1
    jsoncrevarchange<-toJSON(crevarchangeui)
    binwidth<<-input$binwidth
    cleanvar<<-input$varclean
    
    
    data_set
    #data_set<<-connection()
    #data.frame(data_set)
    
    
  })
  
  observeEvent(input$colgo, output$coltext<-renderText({
    
    coloutput<<-colchangereturn()
    
    if(coloutput == "Done!"){
      
      paste("The data has been successfully subsetted")
      
    }
    else{
      
      paste("Data could not be subsetted")
    }
    
  })) 
  
  observeEvent(input$rowgo, output$rowtext<-renderText({
    
    rowoutput<<-rowchangereturn()
    
    if(rowoutput == "Done!"){
      
      paste("The data has been successfully subsetted")
      
    }
    else{
      
      paste("Data could not be subsetted")
    }
    
  }))
  
  observeEvent(input$tarchange1, output$tartext<-renderText({
    
    paste("Done!")
    
    #taroutput<<-tarvarmake()
    
    #if(taroutput == "Done!"){
    
    #paste("The target variable has been successfully created")
    
    #}
    
    #else{
    
    #paste("Target Variable could not be created")
    #}
    
  }))
  
  #output$target_variable_summary_plot<-renderPlotly({
  
  
  #tvar.plot<<-as.data.frame(tarplotreturn())
  
  #plot_ly(y = tvar.plot, type = "box")
  
  
  #})
  
  output$target_variable_summary<-renderTable({
    
    tvar.result<<-as.data.frame(tarvarreturn())
    tvar.result
    
  })
  
  #observeEvent(input$tarchange1, output$target_variable_summary_plot<-renderPlotly({
  
  #tvar.plot<<-as.data.frame(tarplotreturn())
  
  #plot_ly(y = tvar.plot, type = "box")
  
  #}))
  
  observeEvent(input$tarchange1, output$target_variable_summary<-renderTable({
    
    tvar.result<<-as.data.frame(tarvarreturn())
    tvar.result  
    
  }))
  
  output$varlen<-renderValueBox({
    
    no_of_variables_valuebox_ui()
    
  })
  
  output$rowlen<-renderValueBox({
    
    no_of_observations_valuebox_ui()
    
  })
  
  output$varselui<-renderUI({
    
    varselchoicelist<-varselreturn()
    varsel_ui(varselchoicelist)
    
  })
  
  observeEvent(input$crevargo, output$varselui<-renderUI({
    
    crevarchangeui<<-input$crevar1
    
    crevarfunc<-crevarreturn()
    
    varselchoicelistnew<-varselreturnnew()
    varsel_ui_new(varselchoicelist)
    
  }))
  
  observeEvent(input$dispvar, output$countchart<-renderPlotly({
    
    selcol<<-input$varsel
    
    #data_set[[selcol]]<-as.numeric(data_set[[selcol]])
    
    bintable<-countchartreturn1(selcol)
    
    bintable$Bins <- factor(bintable$Bins, levels = unique(bintable$Bins)[order(bintable$Frequency, decreasing = TRUE)])
    
    BinFreq<-plot_ly(bintable, x =~Bins, y=~Frequency, type = "bar") %>%
      
      layout(title = "Distribution of Data Fields from Highest to Lowest",
             xaxis = list(title = "Fields"),
             yaxis = list(title = "Frequency Count"))
    
  })) 
  
  observeEvent(input$dispvar, output$summarystats<-renderTable({
    
    summary_stats<-as.data.frame(summarystatreturn1(selcol))
    summary_stats1<-t(summary_stats)
    summary_stats1
    
  })) 
  
  observeEvent(input$dispvar, output$histogramchart<-renderPlotly({
    
    histocol<-selcolfetchreturn()
    
    histolist<-as.numeric(histogramchartreturn1(selcol))
    
    #data_set[[selcol]]<-as.numeric(data_set[[selcol]])
    
    
    histogramchart<-plot_ly(x = as.numeric(histocol[[1]]), type = "histogram", xbins = list(size = 5), histfunc = 'sum') %>%
      layout(yaxis=list(type='linear'),
             title = "Histogram of Selected Variable")
    
    
    
  }))
  
  observeEvent(input$dispvar, output$binchoiceui<-renderUI({
    
    binchoiceui()
    
  }))
  
  observeEvent(input$dispvar, output$bintypeui<-renderUI({
    
    binchoicemade<<-input$binchoice
    
    
    if(is.null(binchoicemade) == FALSE){  
      
      bintypeui()
      
    }
    
  })) 
  
  observeEvent(input$binchosen, output$crosstabs<-renderRpivotTable({
    
    binwidth<<-input$binwidth
    
    
    data_set[[selcol]]<-as.numeric(data_set[[selcol]])
    
    cross_tabs<-as.data.frame(crosstabreturn1(selcol, binwidth, binchoicemade))
    
    crosstabreturn<-rpivotTable(cross_tabs, rows = "BinCategory", cols = "PercentileRank_SelectedVariableBin", width = "100%")
    
  }))
  
  
  observeEvent(input$savedata, output$savedataresult<-renderText({
    
    savedataoutput<<-savedatareturn()
    
    if(savedatoutput == "Done!"){
      
      paste("The data has been successfully saved for analysis")
      
    }
    else{
      
      paste("Data could not be saved for analysis")
    }
    
    
  }))
  
  
  observeEvent(input$binchosen, output$crosstabsnew<-renderPrint({
    
    binwidth<<-input$binwidth
    
    crosstabdata<-binreturn()
    
    ctab <- CrossTable(crosstabdata$PercentileRank_SelectedVariableBin, crosstabdata$target_variable,
                       prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)
    
  }))
  
  observeEvent(input$binchosen, output$stackedchart<-renderPlotly({
    
    data_set[[selcol]]<-as.numeric(data_set[[selcol]])
    
    stacked_chart<-as.data.frame(stackedchartreturn1(selcol, binwidth, binchoicemade))
    
    stackedplot<-ggplot(stacked_chart,aes(x = Bin_Sizes, y = value,fill = variable)) + 
      geom_bar(position = "fill",stat = "identity") + 
      scale_y_continuous(labels = percent_format())
    
  }))
  
  observeEvent(input$binchosen, output$relationshipgraph<-renderPlotly({
    
    data_set[[selcol]]<-as.numeric(data_set[[selcol]])
    
    relation_chart<-as.data.frame(relationshipchartreturn1(selcol, binwidth, binchoicemade))
    
    monotonic_plot<- ggplot(data = relation_chart, aes(x = Bin_Sizes, y = value, group = variable, color = variable))+
      geom_line()+         
      geom_point( size=4, shape=21, fill="white")
    
  }))
  
  observeEvent(input$dispvar, output$loesschart<-renderPlotly({ 
    
    loessdata<-loessreturn()
    loessdata[[1]]<-as.numeric(loessdata[[1]])
    loessdata[loessdata == "NULL"]<-NA
    loessdata<-loessdata[complete.cases(loessdata),]
    loessdata[[2]]<-as.numeric(loessdata[[2]])
    
    relationplot <- plot_ly(loessdata, x = ~loessdata[[1]], color = I("black")) %>%
      add_markers(y = ~loessdata[[2]], showlegend = FALSE) %>%
      add_lines(y = ~fitted(loess(loessdata[[2]] ~ loessdata[[1]])),
                line = list(color = '#07A4B5'),
                name = "Loess Smoother", showlegend = TRUE) %>%
      layout(xaxis = list(title = 'Selected Variable'),
             yaxis = list(title = 'Target Variable'),
             legend = list(x = 0.80, y = 0.90))
    
    return(relationplot)
    
  }))
  
  observeEvent(input$binchosen, output$auc1<-renderPlot({
    
    aucdata<-aucreturn()
    
    dummyauc<-as.numeric(aucdata[[2]])
    
    auc<-colAUC(dummyauc, aucdata[[1]], plotROC = TRUE)
    
    
    return(auc)
    
  }))
  
  observeEvent(input$binchosen, output$auc2<-renderText({
    
    aucdata<-aucreturn()
    
    dummyauc<-as.numeric(aucdata[[2]])
    
    auc1<-colAUC(dummyauc, aucdata[[1]], plotROC = FALSE)
    
    
    printtext<-print(paste0("The Area Under the Curve for your selected bin size is ", as.numeric(auc1), ". An AUC score above 0.7 is good and represents the accuracy with which bins need to be divided"))
    
    return(printtext)
    
  }))
  
  
  
  
  observeEvent(input$binchosen, output$scattergraph<-renderPlotly({
    
    DelinquentAmountDays<-((data_set$TotalDemandDue)*(data_set$DelinquentDays))
    
    data_set$DelinquentAmountDays<-DelinquentAmountDays
    
    colnames(data_set)[colnames(data_set)=="DelinquentAmountDays"] <- "DelinquentAmountDays"
    
    #colnames(data_set)<<-paste("DelinquentAmountDays")
    
    mysum <- function(x)sum(x,na.rm = any(!is.na(x))) 
    
    Sum_Delinquent_Days<-mysum(as.numeric(DelinquentAmountDays))
    
    target_variable<-(data_set$DelinquentAmountDays/Sum_Delinquent_Days)*100
    
    data_set$target_variable<-target_variable
    
    colnames(data_set)[colnames(data_set)=="target_variable"] <- "target_variable"
    
    scatter_plot <- ggplot(data_set, aes(x=data_set[[selcol]], y=target_variable)) + geom_point()+
      geom_smooth(method=lm)
    
    scatter_plot
    
  }))
  
  observeEvent(input$cleanselect, output$cleantext<-renderText({
    
    cleanoutput<<-cleanvarreturn()
    
    if(cleanoutput == "Done!"){
      
      paste("The data has been successfully cleaned and the variable added to model list")
      
    }
    else{
      
      paste("Data could not be cleaned")
    }
    
    
  }))
  
  observeEvent(input$cleanselect, output$markdone<-renderText({ 
    
    
    if(input$cleanselect > 0){
      myValues$dList <<- c(isolate(myValues$dList), isolate(input$varsel))
      shortlistvar<<-myValues$dList
      json.shortlistvar<<-toJSON(shortlistvar)
      
    }
    
    
  }))
  
  
  output$shorvar<-renderValueBox({
    
    shortlisted_variables_ui()
    
  })
  
  
  observeEvent(input$trainsend, output$traintext<-renderText({
    
    trainpercent<<-input$traindat
    
    trainoutput<-traindonereturn()
    
    
    if(trainoutput == "Done!"){
      
      paste("The data has been successfully subsetted for training and testing")
      
    }
    else {
      
      paste("Data could not be subsetted")
    }
    
    observeEvent(input$compute, output$modeldata<-DT::renderDataTable({
      
      
      selectedmodel<<-input$modelsel
      
      modeloutput<-modelrunreturn()
      
      return(modeloutput)
      
      
    }))
    
  }))
  
}
  
  shinyApp(ui, server)
  

  