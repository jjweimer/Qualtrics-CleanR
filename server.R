library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
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
    user_data <- read.csv(inFile$datapath)
    
    #now clean data for instruction_data
    instruction <- user_data %>% select(Q3, Q16, Q192.1, Q17, Q21)
    instruction <- instruction[instruction$Q3 == "Instruction",]
    colnames(instruction) <- c("service","date","format","activity",
                               "num_attendants")
    #ensure date data type
    instruction$date <- mdy(instruction$date)
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
    user_data <- read.csv(inFile$datapath)
    
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
    user_data <- read.csv(inFile$datapath)
    
    #now clean data for consults_data
    consults <- user_data %>% select(Q3,Q38,Q39,Q40,Q42,Q43,Q44,Q45)
    consults <- consults[consults$Q3 == 'Consultation',]
    colnames(consults) <- c("service","date","location","department",
                            "num_consult","category","time_spent","status")
    #ensure date class
    #consults$date <- mdy(consults$date)
    #make num_consult numeric
    consults$num_consult <- as.numeric(consults$num_consult)
    #some NAs, lets make these 1 by default
    consults$num_consult[is.na(consults$num_consult)] <- 1
    
    #now deal with the departments. Lots of departments have multiple versions
    #of the dept name in the file (example, Non-UCSD and Not UCSD or 
    #Communication and Communications). 
    
    #fix communications
    consults$department[consults$department %in% 
            c("Communication",
            "Communications")] <- "Communications"
    
    #fix non-ucsd affiliations
    consults$department[consults$department %in% 
            c("Non-UCSD (recent grad)",
              "Not UCSD",
              "UCSD Alumni")] <- "Non-UCSD"
    #%in%  c("Non-UCSD (recent grad)", "Not UCSD", "UCSD Alumni")
    
    #fix Data Science
    consults$department[consults$department %in% 
            c("Data science", 
              "Data Science & Engineering")] <- "Data Science"
    #%in%  c("Data science", "Data Science & Engineering")
    
    #fix GPS
    consults$department[consults$department %in%
        c("GPS", "Global Policy and Strategy", 
          "Global Policy and Strategy",
          "Global policy and Strategy", 
          "Global Policy and Stragegy")] <- "School of Global Policy and Strategy"
    
    #fix Business Analytics
    consults$department[consults$department %in% 
            c("Business Intelligence Analysis (Extension)",
              "Business Intelligence Analysis/Extension", 
              "Business Analytics")] <- "Business Intelligence Analysis"
    
    #fix medicine
    consults$department[consults$department %in% 
            c("School of Medicine",
              "Medicine ", "Med School")] <- "Medicine"
    
    #generate counts for each department
    consults <- consults %>% group_by(department) %>%
      mutate(dept_consult_count = n())
    
    return(consults)
  })
  
  
  ###################################################################
  ### Render Tables  ############
  ###################################################################
  
  #render instruction data frame
  output$instruction_data <- renderTable({
    
    #read in the user data
    instruction_data <- Sortie_instruction() #return Sortie function
    
    #returns df
    return(instruction_data)
    
  })
  
  #render outreach data frame
  output$outreach_data <- renderTable({
    
    #read in the user data
    outreach <- Sortie_outreach() #return Sortie function
    
    #returns df
    return(outreach)
  })
  
  
  ###################################################################
  ###### Text Summary Statistics using Cleaned data  ############
  ###################################################################
  
  #generate instruction text summary stats
  output$instruction_stats <- renderText({ 
    
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
  output$outreach_stats <- renderText({ 
    
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
  output$consults_stats <- renderText({ 
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #now the text render part
    #how many consults in what date range?
    date_min <- consults$date[1]
    date_max <- consults$date[nrow(consults)]
    num_consults <- nrow(consults)
    num_people_consulted <- sum(consults$num_consult)
    num_departments <- length(unique(consults$department))
    
    return(paste("There were",num_consults,"consults from",date_min,"to",
                 date_max, "reaching",num_people_consulted, "people in", 
                 num_departments, "unique departments")) 
    
  })
  
  ## About Text
  
  output$about_text <- renderText({
    "This is an applet to clean user uploaded Qualtrics Data.
    To use the app, upload a Qualtircs csv using the left sidebar. 
    Summary statistics, plots and tables will be generated for
    you automatically."
  })
  
  #############################################
  ### PLOTS
  ################################################
  
  #consults department counts
  output$consults_graph <- renderPlotly({
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    consults <- consults[consults$dept_consult_count > 1,]
    
    #possibly add radio buttons for setting dates, cutoffs of dept count
    
    fig <- ggplotly(
      consults %>% group_by(department) %>%
      count(department) %>%
      ggplot(aes(x = reorder(department,n), y = n, fill = department)) +
      geom_col(alpha = 1) +
      #geom_text(aes(label = n), hjust = -1) +
      coord_flip() +
      ggtitle("Most Frequently Consulted Departments (n > 1)") +
      theme_bw() +
      labs(x = NULL, y = "count") +
      theme(legend.position="none")) %>% layout(height = 600) %>%
      config(displayModeBar = F)
      
    return(fig)
    
    
  
  })
  
  ## instruction over time
  output$instruction_time_plot <- renderPlotly({
  
    #read in df
    instruction <- Sortie_instruction()
    #lets make a couple fake events by duplicating observations
    instruction <- rbind(instruction, instruction[3,], instruction[4,],
                         instruction[7,], instruction[7,])
    
    #make daily events count
    instruction <- instruction %>% group_by(date) %>%
      mutate(daily_instruction_events = n())
    
    #make daily people count
    instruction <- instruction %>% group_by(date) %>%
      mutate(daily_people_instructed = sum(num_attendants)) 
   
    #plot
    fig1 <- ggplotly(
      instruction %>% 
        ggplot(aes(x = date, y= daily_instruction_events)) +
        geom_line() +
        ggtitle("Instruction Events over time") +
        ylim(0,5) +
        theme_bw() +
        theme(legend.position="none")) %>% config(displayModeBar = F) %>% 
      layout(height = 600)
    
    return(fig1)
  })
  
  
  
  
})
