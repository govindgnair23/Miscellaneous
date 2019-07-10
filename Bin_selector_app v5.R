#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(plotly)
library(DT)



options(shiny.maxRequestSize = 100*1024^2)

ui <- fluidPage(
   
  fluidRow(
    
    column(4,
           wellPanel(
             
             h3("Load Dataset"),
             # Input: Select a file ----
             fileInput( inputId = "file1", "Choose CSV File",
                        multiple = TRUE,
                        accept = '.csv'),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Checkbox if file has header ----
             checkboxInput("header", "Header", TRUE),
             
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Select number of rows to display ----
             radioButtons("disp", "Display",
                          choices = c(Head = "head",
                                      All = "all"),
                          selected = "head")
           )       
    ),
    
    column(8,
           tableOutput(outputId= "contents")
    )
    
  ),
  
  br(),
  #Assign variable types to each field in the input dataset
  fluidRow(
    
    column(4,
           wellPanel(
             h3("Identify parameters and filters"),
             #Define attributes 
             uiOutput('select_vartypes')
             
             
           )       
    ),
    
    
    column(8,
           tableOutput(outputId = "variable_types")
    )
  ),
  
  br(),
  
  fluidRow(
    column(4,
           wellPanel(
           h3("Set filters and Select parameter"),  
           uiOutput('set_parms_filters')
           )
           ),
    
    
    column(8,
           verbatimTextOutput(outputId= "Summary"))
    
  ),
  
  br(),
  
  fluidRow(
  column(4,
         wellPanel(
           h3("Customize bins"),
           fixedRow(
             column(width=12,textInput('custom_breaks','Enter breaks for bins 
                                       (comma delimited)',"0,1,2,Inf"))),
           fixedRow(
             column(width=6,actionButton("SubmitValues","Submit")),
             column(width=6,actionButton("SaveValues","Save")))
           
         )
         
         
         ),
  
  
  column(8,
         dataTableOutput(outputId= "Distribution"))
  )
    
  )

  


# Define server logic required to draw a histogram
server <- function(input, output) {
  ###################Utility function##############
  ##Function to concactenate corresponding columns in two dataframes
  concact<- function(df1,df2){
    df <- data.frame(matrix(0,nrow(df1),ncol(df1)))
    for( i in c(1:ncol(df1))){
      
      df[,i] <- paste0(df1[,i],df2[,i])
      
      
    }
    
    
    return(df)
  }
  
  
  
  
  
  ###read in data
  
  input_df <- reactive({
    req(input$file1$datapath)
    read.csv(input$file1$datapath,
             header = TRUE,na.strings="",
             sep = ",")
  })
  
  
  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.


    if(input$disp == "head") {
      return(head(input_df()))
    }
    else {
      return(input_df())
    }

  })
  
  #Get all column headers to determine parameters, filters and others
  
  column_names <- reactive({
    colnames(input_df())
  })
  
  
  ##Select buttons to select variable type for each variable
  output$select_vartypes <- renderUI({
    div(
      lapply(column_names(),function(var_n){
        selectInput(inputId = paste0(var_n,"_type"),label = var_n,choices=list('parameter','filter',
                                                                               'other','effective_flag'))
      }),
      actionButton("submitValues1", "Submit")
    )
  })
  
  

  
  

  #Get information about the input dataset including desired variable types, data,target and predictor variables
  data_info <- eventReactive(input$submitValues1,{
    ncols <- length(column_names())
    values <<- rep(NA,ncols)
    # get the values of each selectInput and store them in "values"
    for(i in c(1:ncols)) {
      inputName <- paste0(column_names()[i],"_type")
      # only get a value if the  input exists
      if (!is.null(inputName))
        values[[i]] <<- input[[inputName]]
    }


    # Convert columns into appropriate formats
    parameter_index <- (values == 'parameter')
    filter_index <- (values== 'filter')
    other_index <- (values== 'other')
    effective_index <- (values== 'effective_flag')

    #Identify parameters
    parameter_variables <-column_names()[parameter_index]
    filter_variables <- column_names()[filter_index]
    effective_variable <- column_names()[effective_index]

    list(vartypes=values,parameters=parameter_variables,filters = filter_variables,effective_flag = effective_variable)
  })
  
  
  #get variable types of varios fields in input data
  observeEvent(input$submitValues1,
               output$variable_types <- renderTable({data.frame(variable = column_names(),
                                                                type = data_info()[['vartypes']])}))

  ###Get unique set of permissible values for each filter
  filter_values  <- reactive({ input_df()%>%
      select(one_of(data_info()[['filters']]))%>%
      apply(2,unique)})
  
  
  ##Populate each filter with appropriate options
  output$set_parms_filters <- renderUI({
    div(
    lapply(data_info()[['filters']],function(var_n){
      selectInput(inputId = paste0(var_n,"_filter"),label = var_n,choices=filter_values()[[eval(quote(var_n))]])
    }),
    selectInput(inputId = 'Parameter',label = 'Select parameter',choices=data_info()[['parameters']]),
    actionButton("submitValues2", "Submit")
    )
  })
  
  
  ####Create a filtered dataset
  
  filtered_data <- eventReactive(input$submitValues2,{
                            ##subset just filter variables            
                            lhs_matrix <- input_df()%>%select(one_of(unlist(data_info()[['filters']])))%>% 
                                            mutate_all(as.character)%>% as.matrix() 
                            
                            rhs <- sapply(unlist(data_info()['filters']),function(x){return(input[[paste0(x,'_filter')]])})
                            rhs_matrix <-  matrix(rep(rhs,each = nrow(lhs_matrix)),ncol = length(rhs))
                            
                            lhs_rhs_matrix <- lhs_matrix == rhs_matrix
                            filter_index <- apply(lhs_rhs_matrix,1,sum)
                            data <- input_df()[filter_index,]
                            list( data=data)
                                          })
    
  
  ## Effectiveness indicator
  
  effective_indicator <- reactive({data_info()[['effective_flag']]})
  ### Select only required parameter  
  filtered_data2 <- reactive({
    filtered_data()[['data']]%>%select(one_of(c(input$Parameter,effective_indicator())))
                     
  })
  
  
  ##Determine bins
  bin <- reactive({

    if(input$SubmitValues){
      custom_breaks <- isolate(as.numeric(unlist(strsplit(input$custom_breaks,","))))
      cut(filtered_data2()[,1],breaks = custom_breaks, include.lowest = TRUE)

    }else{
      cut(filtered_data2()[,1],breaks = unique(quantile(filtered_data2()[,1],probs = seq(0,1,by=0.1))),
          include.lowest = TRUE)}
  })
  
  ##Add bin to dataframe
  filtered_data3 <- reactive({cbind(filtered_data2(),'bin'=bin())})
  
  output$Summary <- renderPrint({summary(filtered_data3())})
  
  ##Get distribution
  distribution <- reactive({
    
    No_alerts <- as.data.frame(table(filtered_data3()[,3]),responseName='No_Alerts')
    No_effective <- tapply(filtered_data3()[,2],filtered_data3()[,3],sum)
    Effectiveness <- No_effective/No_alerts
    final_data <- cbind(No_alerts,No_effective,Effectiveness)
   
    final_data
    })
  



  output$Distribution <- DT::renderDataTable({DT::datatable(distribution(),extensions = "Buttons",rownames=FALSE,
                                                            colnames = c('Bin','No_Alerts','No_Effective_Alerts','% Effectiveness'),
                                                            options = list(
                                                              dom = 'Bfrtip',
                                                              buttons = c('copy', 'csv', 'excel'))) %>% formatRound(4,digits = 3)})
}

# Run the application 
shinyApp(ui = ui, server = server)

