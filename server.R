library(shiny)
library(lubridate)
library(dplyr)
library(plotly)

# Define server logic 
shinyServer(function(input, output) {

  ###################################################################
  ### Cleaned user input Qualtrics Data by service type  ############
  ###################################################################
  
  #clean instruction data, return data frame
  Sortie_instruction <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath, header = input$header)
    
    #now clean data for instruction_data
    instruction <- user_data %>% select(Q3, Q16, Q192.1, Q17, Q21)
    instruction <- instruction[instruction$Q3 == "Instruction",]
    colnames(instruction) <- c("service","date","format","activity",
                               "num_attendants")
    #ensure date data type
    #instruction$date <- mdy(instruction$date)
    #make num_attendants numeric
    instruction$num_attendants <- as.numeric(instruction$num_attendants)
    
    return(instruction)
  })
  
  #clean outreach data, return data frame
  Sortie_outreach <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath, header = input$header)
    
    #now clean data for outreach_data
    outreach <- user_data %>% select(Q3, Q156, Q198, Q174, Q184, 
                                     Q194, Q202, Q196)
    outreach <- outreach[outreach$Q3 == "Outreach",]
    colnames(outreach) <- c("service","date","type","home_program", "attendees",
                            "status","duration","time_prep")
    #ensure date data type
    #outreach$date <- mdy(outreach$date)
    #make attendees numeric
    outreach$attendees <- as.numeric(outreach$attendees)
    
    return(outreach)
  })
  
  #clean consults data, return data frame
  Sortie_consults <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath, header = input$header)
    
    #now clean data for consults_data
    consults <- user_data %>% select(Q3,Q38,Q39,Q40,Q42,Q43,Q44,Q45)
    consults <- consults[consults$Q3 == 'Consultation',]
    colnames(consults) <- c("service","date","location","department",
                            "num_consult","category","time_spent","status")
    #ensure date data type
    #consults$date <- mdy(consults$date)
    #make num_consult numeric
    consults$num_consult <- as.numeric(consults$num_consult)
    #some NAs, lets make these 1 by default
    consults$num_consult[is.na(consults$num_consult)] <- 1
    
    return(consults)
  })
  
  
  ###################################################################
  ### Render Tables  ############
  ###################################################################
  
  #render instruction data frame
  output$instruction_data <- renderTable({
    
    #read in the user data
    instruction_data <- Sortie_instruction() #return Sortie function
    
    #returns last line of output
    return(instruction_data)
    
  })
  
  #render outreach data frame
  output$outreach_data <- renderTable({
    
    #read in the user data
    outreach <- Sortie_outreach() #return Sortie function
    
    #returns last line of output
    return(outreach)
  })
  
  
  ###################################################################
  ###### Text Summary Statistics using Cleaned data  ############
  ###################################################################
  
  #generate instruction text summary stats
  output$instruction_stats <- renderPrint({ 
    
    #read in the user data
    instruction_data <- Sortie_instruction() #return Sortie function
    
    #now the text render part
    ins_num_activities <- nrow(instruction_data)
    ins_num_groups<- length(unique(instruction_data$activity))
    ins_num_students_reached <- sum(instruction_data$num_attendants)
    
    return(paste("There were",ins_num_activities,
                 "instruction acitivites reaching",ins_num_groups,
                 "groups and",ins_num_students_reached,"students"))
    
  })
  
  #generate outreach text summary stats
  output$outreach_stats <- renderPrint({ 
    
    #read in the user data
    outreach <- Sortie_outreach() #return Sortie function
    
    #now the text render part
    out_num_activities <- nrow(outreach)
    out_num_students <- sum(outreach$attendees)
    
    #return text stat
    return(paste("There were",out_num_activities,"outreach activities reaching",
                out_num_students,"students"))
    
  })
  
  ## text summary stats for Consults
  output$consults_stats <- renderPrint({ 
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #now the text render part
    #how many consults in what date range?
    date_min <- consults$date[1]
    date_max <- consults$date[nrow(consults)]
    num_consults <- nrow(consults)
    num_people_consulted <- sum(consults$num_consult)
    
    return(paste("There were",num_consults,"consults from",date_min,"to",
                 date_max, "reaching",num_people_consulted, "people")) 
    
  })
  
  ## About Text
  
  output$about_text <- renderText({
    
    "This is an applet to clean user uploaded Qualtrics Data. 
    Developed by Joshua Weimer for the UCSD Data and GIS Lab."
  })
  
  
  #############################################
  output$consults_graph <- renderPlotly({
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    
    fig <- plot_ly(x = consults$num_consult, 
                   y = consults$department, 
                   type = 'bar', orientation = 'h', height=800)  %>% 
      layout(yaxis = list(categoryorder = "total ascending"))
  })
  
  
})
