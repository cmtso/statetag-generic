# Shiny app for system state tagging of long-term enviornmental monitoring data (generic version)
# Author: Michael Tso (UK Centre for Ecology and Hydrolog, mtso at ceh.ac.uk)
# Date: 22/10/2019

# note: this app will still work without screenshots, logos etc.

#setwd("/data/state-generic/")
packrat::on()



library(tidyverse)
library(plotly)
library(shinycssloaders)
library(DT)


cbPalette <- c("#0072B2","#56B4E9",  "#009E73","#CC79A7","#E69F00",    "#D55E00","#F0E442", "#999999", 
               "#0072B2","#56B4E9",  "#009E73","#CC79A7","#E69F00",    "#D55E00","#F0E442", "#999999",
               "#0072B2","#56B4E9",  "#009E73","#CC79A7","#E69F00",    "#D55E00","#F0E442", "#999999")

ui <- fluidPage(
  titlePanel("System state tagging (generic version)"),
  
  tabsetPanel(id = "mainTabset",
              tabPanel("Uploader", value = "uploader",
                       
                       
                       hr(),
                       sidebarLayout(
                         sidebarPanel(
                           actionButton("helpme", "Help"),
                           hr(),
                           fileInput('target_upload_state', '1. Choose file to upload (state data)',
                                     accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       '.csv'
                                     )),
                           radioButtons("separator_state","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
                           hr(),
                           hr(),
                           hr(),
                           fileInput('target_upload', '2. Choose file to upload (observational data)',
                                     accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       '.csv'
                                     )),
                           radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
                           # radioButtons("tidydata","Data arrangement: ",
                           #              choices = c("one obs. per line","tabular/panel"),
                           #              selected="one obs. per line",inline=TRUE),
                           
                           
                           textOutput('gotoAnalyzeTabButtonText'),
                           actionButton('gotoAnalyzeTabButton',
                                        'Analyze', icon = icon("check-circle"))
                         ),
                         mainPanel(
                           
                           DT::dataTableOutput("sample_table_state"),
                           DT::dataTableOutput("sample_table")
                           
                         )
                       )
              ),
              tabPanel("Clustering", value = "analyzeData",
                       hr(),
                       conditionalPanel(condition = 'output.csv_import_ready',
                                        sidebarLayout(
                                          sidebarPanel(
                                            dateRangeInput("daterange1", "Date range:",
                                                           start = "2008-01-01",
                                                           end   = "2014-06-30",
                                                           min = "1900-1-1",
                                                           max = "2100-12-31"),
                                            sliderInput("n_cluster", "Number of clusters (k):", 5, min = 1, max = 20),
                                            selectInput('MA_choices', 'Choose system state variables for clustering', 
                                                        choices = c("DRYTMP","SOLAR","WSPEED","RAIN"), 
                                                        selected = c("DRYTMP","SOLAR","WSPEED","RAIN"),
                                                        multiple=TRUE, selectize=TRUE),
                                            
                                            
                                            wellPanel(
                                              tags$small("Note: the data is scaled before clustering.")
                                            ), 
                                            actionButton("show", "About"),actionButton("helpme2", "Help"),
                                            hr(),
                                            width = 3),
                                          mainPanel(
                                            
                                            selectInput("plotClusterOpt", "Choose state variable to show:",
                                                        choices = c("DRYTMP","SOLAR"), 
                                                        selected = c("DRYTMP"),
                                                        multiple=FALSE, selectize=FALSE),
                                            plotlyOutput("clusters") %>% withSpinner(color="#0dc5c1")  , 
                                            plotlyOutput("boxplot"),
                                            hr(),
                                            hr(),
                                            plotlyOutput("elbow"),
                                            h4("Likelihood for the next observation to be in a certain state:"),
                                            tableOutput("placeholder") ,
                                            h4("row: state at t, column: state at t+1"),
                                            hr(),
                                            hr(),
                                            selectInput("plotPredOpt", "Choose observation to show:",
                                                        choices = c("TOCA"),
                                                        selected = "TOCA"),
                                            checkboxInput("show_options", "Show plot options",value = FALSE),
                                            
                                            conditionalPanel( condition = 'input.show_options == 1',
                                                              radioButtons("n_sd","Prediction intervals options:", c("68% (1 s.d.)" = 1, "95% (2 s.d.)" = 2,
                                                                                                                     "99.5% (3 s.d.)" = 3 ,"mean only" = 0),
                                                                           selected = 2, inline=T),  
                                                              sliderInput("a_val", "prediction interval opaqueness (%)", 50, min=0, max=100, step = 1),
                                                              checkboxInput("show_pred_int", label="show prediction intervals",value = TRUE),
                                                              checkboxInput("remove_negative", label="crop negative prediction intervals",value = TRUE)
                                            ),
                                            plotlyOutput("Prediction") ,
                                            hr(),
                                            downloadButton('download_1',"Download tagged data"),
                                            hr()
                                          )
                                        )
                       )
              ) 
  )
)   




server <- function(input, output, session) {
  ## need progress bar for startup
  
  paper_cite = "Tso et al. (2019+) State tagging for improved earth and environmental data quality assurance"
  showModal(modalDialog(
    title = "Welcome to CEH apps",
    HTML('<img src="ukceh.png" , height="100%", width="100%", align="centre">'),
    p(paper_cite),
    HTML('<a href="http://www.ceh.ac.uk">&copy; UK Centre for Ecology and Hydrology</a>'),
    easyClose = TRUE,
    footer = NULL
  ))
  
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Welcome to CEH apps",
      HTML('<img src="ukceh.png" , height="100%", width="100%", align="centre">'),
      p(paper_cite),
      HTML('<a href="http://www.ceh.ac.uk">&copy; UK Centre for Ecology and Hydrology</a>'),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$helpme, {
    showModal(modalDialog(
      title = "File upload instructions",
      h4("Acceptable data arrangements (.csv files):"),
      div(tags$b("One obs. per line: Read columns of DATE, FIELDNAME, VALUE. Others are ignored", style = "color: blue;")),
      hr(),
      div(tags$b("Tidy/Panel/Tabular: Read the DATE column and other columns are considered fieldnames", style = "color: green;")),
      hr(),
      p("Make sure dates are specified in a non-trivial way, e.g. YYYY-MM-DD"),
      hr(),
      p("Once files are successfully uploaded, press 'Analyze' and you will be directed to the next tab."),
      hr(),
      HTML('Download example state file: <a href="midas_carlisle_daily.csv">MIDAS data at Carlisle, Cumbria</a>'),
      hr(),
      HTML('Download example observation file: <a href="BLEL_data_1945_2013.csv"> Lakes data at Blelham Tarn</a>'),
      HTML('<a href="https://catalogue.ceh.ac.uk/documents/38f382d6-e39e-4e6d-9951-1f5aa04a1a8c"> (DOI) </a>'),
      easyClose = TRUE,
      footer = NULL
    ))
  }) 
  
  observeEvent(input$helpme2, {
    showModal(modalDialog(
      title = "How to use this app?",
      HTML('<img src="screenshot1.png" , height="100%", width="100%", align="centre">'),
      hr(),
      HTML('<img src="screenshot2.png" , height="100%", width="100%", align="centre">'),
      hr(),
      HTML('<img src="screenshot3.png" , height="100%", width="100%", align="centre">'),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })  
  
  
  output$download_1 <- downloadHandler(
    filename = function(){"obs_and_state.csv"}, 
    content = function(fname){
      write.csv(pred_data_aa(), fname)
    }
  )
  
  
  inputData <- eventReactive (input$gotoAnalyzeTabButton, { 
    
    req(input$target_upload)
    
    dd = read.csv(input$target_upload$datapath, header = TRUE,sep = input$separator)
    
    #if (input$tidydata == "one obs. per line") {
    if ("FIELDNAME" %in% colnames(dd) && "VALUE" %in% colnames(dd) ) {
      
      dd = dd %>%  
        dplyr::select(DATE,FIELDNAME,VALUE)  %>%  
        dplyr::mutate(DATE = as.Date(DATE) )
    } else {
      dd = dd %>%  
        tidyr::gather(FIELDNAME,VALUE,-DATE) %>%  
        dplyr::mutate(DATE = as.Date(DATE) )
    }
    return(dd)
  })
  
  inputDataMA <- eventReactive (input$gotoAnalyzeTabButton, { 
    
    req(input$target_upload_state)
    
    dd = read.csv(input$target_upload_state$datapath, header = TRUE,sep = input$separator_state)
    
    #if (input$tidydata_state == "one obs. per line") {
    if ("FIELDNAME" %in% colnames(dd) && "VALUE" %in% colnames(dd) ) {
      dd = dd %>%  
        dplyr::select(DATE,FIELDNAME,VALUE)  %>%  
        dplyr::mutate(DATE = as.Date(DATE) )
      
      
      
    } else {
      dd = dd %>%  
        tidyr::gather(FIELDNAME,VALUE,-DATE)  %>%  
        dplyr::mutate(DATE = as.Date(DATE) )
    }
    return(dd)
  })
  
  MA_data <- reactive ({
    inputDataMA() %>% 
      # dplyr::filter(SITECODE == input$site)   %>%
      dplyr::filter(as.Date(DATE) >= as.Date(input$daterange1[1]) &
                      as.Date(DATE) <= as.Date(input$daterange1[2]) )   
  })
  
  tidydata  <- reactive ({
    MA_data() %>%
      #dplyr::ungroup(SITECODE) %>%
      #dplyr::filter(!FIELDNAME == "ALBSKY") %>%
      dplyr::select(DATE,FIELDNAME,VALUE)   %>% 
      tidyr::spread(FIELDNAME, VALUE, convert = TRUE)  
  })
  
  ####################    CLUSTERING  ##################
  
  clusterdata = reactive({       # perform clustering
    set.seed(200) # for reproducibility
    
    cluster_data = tidydata() %>%
      dplyr::select(c("DATE",input$MA_choices ))  %>% 
      tidyr::drop_na()
    cluster_data = cluster_data[ complete.cases(cluster_data) ,]
    
    clusters = kmeans( scale( cluster_data %>% dplyr::select(-DATE) ), input$n_cluster)  ##
    cluster_data = cluster_data %>%
      dplyr::mutate(states = as.factor(clusters$cluster))
    
    centers = clusters$centers
    data = tidydata()%>%
      dplyr::left_join(cluster_data %>% dplyr::select(DATE,states),by="DATE")
    
    stats = rbind(apply(tidydata()%>%select(-DATE),2,mean,na.rm=TRUE),
                  apply(tidydata()%>%select(-DATE),2,sd,na.rm=TRUE))
    rownames(stats) <- c("mean","sd")
    
    
    state_ts = data %>% dplyr::select(DATE,states) %>%
      dplyr::mutate(lag1 = dplyr::lag(states)) 
    
    
    lag_stat = matrix(0,input$n_cluster,input$n_cluster)
    for (i in 1:input$n_cluster)
    {
      for (j in 1:input$n_cluster)
      {
        lag_stat[i,j] = max( state_ts %>% drop_na() %>%
                               dplyr::filter(states == i ) %>% dplyr::filter(lag1==j) %>% tally() )
        #print(a)
        #lag_stat[i,j] = 1
      }
    }
    #lag_stat <- apply(lag_stat, 2, function(i) i/sum(i)*100)
    
    rownames(lag_stat) = 1:input$n_cluster
    colnames(lag_stat) = 1:input$n_cluster
    print(lag_stat)
    
    out = list(data = data,centers = centers , stats = stats, lag_stat=lag_stat )
    return( out  )
  })
  
  output$placeholder <- renderTable({ clusterdata()$lag_stat },digits = 0, rownames=T)
  
  pred_data_aa <- reactive({ 
    aa = dplyr::inner_join( inputData() %>% dplyr::filter(FIELDNAME == input$plotPredOpt)
                            ,clusterdata()$data %>% dplyr::select(DATE,states),by="DATE")  %>%
      dplyr::group_by(states) %>% 
      dplyr::mutate(counts.mean = mean(VALUE), 
                    counts.sd = sd(VALUE),
                    counts.max = mean(VALUE)+as.numeric(input$n_sd)*sd(VALUE),
                    counts.min = mean(VALUE)-as.numeric(input$n_sd)*sd(VALUE))
    if(input$remove_negative == TRUE) {
      aa = aa %>% dplyr::mutate(counts.min = replace(counts.min,which(counts.min <0),0)) # input$remove_negative      
    }
    return(aa)
  })
  
  output$Prediction <- renderPlotly({
    #print(head(clusterdata()))
    #print(input$plotPredOpt)
    aa = pred_data_aa() %>% tidyr::drop_na()
    #print(head(aa))
    p1 = ggplot( aa )
    
    if(input$show_pred_int == TRUE) {
      p1 = p1 + geom_ribbon(aes(x = DATE, 
                                ymin=counts.min,
                                ymax=counts.max
      ),fill="steelblue2",color="steelblue2",alpha=input$a_val/100)
    }
    p1 = p1 +
      geom_point(aes(x = DATE, y = VALUE,colour=states)) +
      geom_line(aes(x = DATE, y = VALUE)) +
      scale_colour_manual(values=cbPalette)+
      scale_x_date() +
      ggtitle(str_c("Observed variable and derived prediction intervals")) +
      ylab("Value") + xlab('Date') +
      theme(legend.position = "e",
            axis.title.y = element_text(size = 10),
            plot.title = element_text(lineheight = 0.8, face = "bold")) 
    ggplotly()  %>% 
      plotly::config(toImageButtonOptions = list(format="png"))
  })
  
  output$clusters <- renderPlotly({
    
    ggdata = clusterdata()$data %>% 
      dplyr::select(DATE,input$plotClusterOpt,states) %>%
      dplyr::rename(VALUE = input$plotClusterOpt)
    ggplot( ggdata ) +
      geom_point(aes(x = DATE, y = VALUE, colour= states)) +
      scale_x_date() +
      ylab("Value") + xlab('Date') +
      theme(#panel.background = element_rect(fill = "#b3b3b3",
        legend.position = 'e',
        axis.title.y = element_text(size = 10),
        plot.title = element_text(lineheight = 0.8, face = "bold")) +
      scale_colour_manual(values=cbPalette)
    
    ggplotly()  %>% 
      plotly::config(toImageButtonOptions = list(format="png"))
    
  })
  
  output$boxplot <- renderPlotly({
    
    n_fun <- function(x){
      return(data.frame(y = median(x), label = paste0("n = ",length(x))))
    }
    
    ggdata = clusterdata()$data %>% 
      dplyr::select(DATE,input$plotClusterOpt,states) %>%
      dplyr::rename(VALUE = input$plotClusterOpt) %>% 
      tidyr::drop_na()
    
    ggplot( ggdata , aes(states,VALUE) ) +
      geom_boxplot(fill = "white", colour = "#3366FF") +
      ylab("Value") + xlab('state')  +
      geom_text(stat="count", aes(label=paste0("N=",..count..)), y= 1.1 * max(ggdata$VALUE, na.rm = T)) +
      expand_limits(y=1.15*max(ggdata$VALUE, na.rm = T)) +
      theme(legend.position = 'e',
            axis.title.y = element_text(size = 10),
            plot.title = element_text(lineheight = 0.8, face = "bold"))
    ggplotly()  %>% 
      plotly::config(toImageButtonOptions = list(format="png"))
  })
  
  output$elbow <- renderPlotly({
    cluster_data = tidydata() %>%
      dplyr::select(c("DATE",input$MA_choices )) #("DATE","DRYTMP","SOLAR","WSPEED","RAIN")
    cluster_data = cluster_data[ complete.cases(cluster_data) ,]
    
    k = 1:20
    err = rep(0,length(k))
    
    for (i in 1:length(k)){
      clusters = kmeans( scale( cluster_data %>% dplyr::select(-DATE) ), k[i] )
      err[i] = clusters$tot.withinss
    }
    
    
    ggdata = clusterdata()$data %>% 
      dplyr::select(DATE,input$plotClusterOpt,states) %>%
      dplyr::rename(VALUE = input$plotClusterOpt)
    
    df<-data.frame(k=k,err=err)
    ggplot(df, aes(k,err) ) +
      geom_point() +
      geom_line()+
      ggtitle(str_c("Elbow method for optimal k")) +
      ylab("total sum of suares") + xlab('k') +
      theme(legend.position = 'e',
            axis.title.y = element_text(size = 10),
            plot.title = element_text(lineheight = 0.8, face = "bold"))
    ggplotly()  %>% 
      plotly::config(toImageButtonOptions = list(format="png"))
  })
  
  
  
  ####################    UPLOADER and data handler   ##################
  df_products_upload_state <- reactive({
    inFile <- input$target_upload_state
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator_state)
    
    return(df)
  })
  
  output$sample_table_state<- DT::renderDataTable({
    df <- df_products_upload_state()
    DT::datatable(df)
  })
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
  #################### update input controls #############
  observe({
    x <- input$MA_choices
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "plotClusterOpt", "Choose system state variable to show:",
                      choices = x,
                      selected = x[1])
    
  })
  
  observe({
    
    x <- as.character( {inputData() %>% dplyr::distinct(FIELDNAME) }$FIELDNAME ) # inputData() %>% distinct(FIELDNAME)
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "plotPredOpt", "Choose observation to show:",
                      choices = x,
                      selected = x[1])
    
  })
  
  
  observe({
    
    x <- as.character( {inputDataMA() %>% dplyr::distinct(FIELDNAME) }$FIELDNAME ) # inputData() %>% distinct(FIELDNAME)
    
    # Can use character(0) to remove all choices
    if (is.null(x)) 
      x <- character(0)
    
    updateSelectInput(session, "MA_choices", 'Choose system state variables for clustering',
                      choices = x,
                      selected = x[1:2])
    
  })
  
  # observe({ # this will make to auto-update overides user input
  observeEvent(input$gotoAnalyzeTabButton, {
    # update default date range if not falling on intial default values
    
    if ( nrow(MA_data() %>% distinct(DATE)) < 100) {
      
      cat("it's working\n")
      
      updateDateRangeInput(session, "daterange1", "Date range:",
                           start = inputDataMA() %>% distinct(DATE) %>% arrange() %>% slice(n()-1000) %>% {.[1,1]},
                           end   = inputDataMA() %>% distinct(DATE) %>% arrange() %>% slice(n()     ) %>% {.[1,1]},
                           min = "1900-1-1",
                           max = "2100-12-31")
    }
  })
  
  # Has a file been imported?
  output$csv_import_ready <- reactive({
    return(!is.null(input$target_upload))
  })
  
  outputOptions(output, "csv_import_ready", suspendWhenHidden = FALSE)
  
  
  
  ####################    button controls   ##################
  # If the analyze button is pressed on the select 'browse data' tab, or, if the tab is 
  #  changed to the 'Analyze Data' tab, check that there are sites loaded, and if so
  #  move the selected tab
  
  observeEvent(input$gotoAnalyzeTabButton, {
    updateTabsetPanel(session, "mainTabset",
                      selected = "analyzeData")
  })
  
  # Display a warning that the user needs to select a site before analysis can be performed,
  #  if they move to the analysis site without having done so.
  output$noSitesWarning = renderText({
    sites = input$selectedAnalysisSites
    if (is.null(sites)) {
      "<font color=\"#FF000\">Please select a site, on the 'Browse Data' tab, you cannot perform any analysis without
      at lease one site selected.</font>"
    }else{
      ""
    }    
  })
  
  # Display a warning if the user has selected a site/sites, but their filter criteria
  #  results in no data to analyze.
  output$noDataWarning = renderText({
    
    analyzeButton = input$analyzedata
    
    if (analyzeButton > 0) {
      if (is.null(changepointDataSet())) {
        "<font color=\"#FF000\">The current filter has removed all observations, no analysis can be done with the current settings.</font>"      
      }else if (dim(changepointDataSet())[1] < 2) {     # I CHANGE IT TO 2 FOR MV.CPT
        "<font color=\"#FF000\">The current filter has removed all observations, no analysis can be done with the current settings.</font>"
      }else {
        ""
      } 
    }
  })
  
}
shinyApp(ui = ui, server = server)
