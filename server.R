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
    
    #make num_attendants numeric
    instruction$num_attendants <- as.numeric(instruction$num_attendants)
    #ensure date data type
    instruction$date <- mdy(instruction$date)
    
    #week and quarter
    instruction$week <- isoweek(instruction$date)
    instruction <- instruction %>% relocate(week, .after = date)
    
    #get year
    instruction$year <- format(instruction$date,format =  '%Y')
    
    instruction$quarter <- c()
    
    #for 2020
    instruction$quarter[instruction$week >=2 & instruction$week <= 12 &
                          instruction$year == 2020] <- "WI"
    instruction$quarter[instruction$week >=14 & instruction$week <= 24 & 
                          instruction$year == 2020] <- "SP"
    instruction$quarter[instruction$week >=27 & instruction$week <= 36 &
                          instruction$year == 2020] <- "SU"
    instruction$quarter[instruction$week >= 40 & instruction$week <= 51 &
                          instruction$year == 2020] <- "FA"
    
    #for 2021
    instruction$quarter[instruction$week >=1 & instruction$week <= 11 &
                          instruction$year == 2021] <- "WI"
    instruction$quarter[instruction$week >=13 & instruction$week <= 23 & 
                          instruction$year == 2021] <- "SP"
    instruction$quarter[instruction$week >=26 & instruction$week <= 35 &
                          instruction$year == 2021] <- "SU"
    instruction$quarter[instruction$week >= 38 & instruction$week <= 49 &
                          instruction$year == 2021] <- "FA"
    #all other are breaks
    instruction$quarter[is.na(instruction$quarter)] <- "Break"
    
    #filter to selected quarter
    if(input$quarter != "All"){
      instruction <- instruction %>% filter(quarter == input$quarter)
    }
    
    #filter to selected year
    if(input$year != "All"){
      y <- as.numeric(input$year)
      instruction <- instruction %>% filter(year == y)
    }
    
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
    outreach$date <- mdy(outreach$date)
    #make attendees numeric
    outreach$attendees <- as.numeric(outreach$attendees)
    
    #get week of year, year, quarter
    outreach$week <- isoweek(outreach$date)
    outreach <- outreach %>% relocate(week, .after = date)
    
    #get year
    outreach$year <- format(outreach$date,format =  '%Y')
    
    outreach$quarter <- c()
    
    #for 2020
    outreach$quarter[outreach$week >=2 & outreach$week <= 12 &
                       outreach$year == 2020] <- "WI"
    outreach$quarter[outreach$week >=14 & outreach$week <= 24 & 
                       outreach$year == 2020] <- "SP"
    outreach$quarter[outreach$week >=27 & outreach$week <= 36 &
                       outreach$year == 2020] <- "SU"
    outreach$quarter[outreach$week >= 40 & outreach$week <= 51 &
                       outreach$year == 2020] <- "FA"
    
    #for 2021
    outreach$quarter[outreach$week >=1 & outreach$week <= 11 &
                       outreach$year == 2021] <- "WI"
    outreach$quarter[outreach$week >=13 & outreach$week <= 23 & 
                       outreach$year == 2021] <- "SP"
    outreach$quarter[outreach$week >=26 & outreach$week <= 35 &
                       outreach$year == 2021] <- "SU"
    outreach$quarter[outreach$week >= 38 & outreach$week <= 49 &
                       outreach$year == 2021] <- "FA"
    #all other are breaks
    outreach$quarter[is.na(outreach$quarter)] <- "Break"
    
    #filter to selected quarter
    if(input$quarter != "All"){
      outreach <- outreach %>% filter(quarter == input$quarter)
    }
    
    #filter to selected year
    if(input$year != "All"){
      y <- as.numeric(input$year)
      outreach <- outreach %>% filter(year == y)
    }
    
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
    consults$date <- mdy(consults$date)
    #make num_consult numeric
    consults$num_consult <- as.numeric(consults$num_consult)
    #some NAs, lets make these 1 by default
    consults$num_consult[is.na(consults$num_consult)] <- 1
    
    #add week of year,quarters(works for 2021 and 2020, need to check for other)
    consults$week <- isoweek(consults$date)
    consults <- consults %>% relocate(week, .before = location)
    
    #get year
    consults$year <- format(consults$date,format =  '%Y')
    
    consults$quarter <- c()
    #for 2020
    consults$quarter[consults$week >=2 & consults$week <= 12 &
                       consults$year == 2020] <- "WI"
    consults$quarter[consults$week >=14 & consults$week <= 24 & 
                       consults$year == 2020] <- "SP"
    consults$quarter[consults$week >=27 & consults$week <= 36 &
                       consults$year == 2020] <- "SU"
    consults$quarter[consults$week >= 40 & consults$week <= 51 &
                       consults$year == 2020] <- "FA"
    
    #for 2021
    consults$quarter[consults$week >=1 & consults$week <= 11 &
                       consults$year == 2021] <- "WI"
    consults$quarter[consults$week >=13 & consults$week <= 23 & 
                       consults$year == 2021] <- "SP"
    consults$quarter[consults$week >=26 & consults$week <= 35 &
                       consults$year == 2021] <- "SU"
    consults$quarter[consults$week >= 38 & consults$week <= 49 &
                       consults$year == 2021] <- "FA"
    #all other are breaks
    consults$quarter[is.na(consults$quarter)] <- "Break"
    
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
    #fix Data Science
    consults$department[consults$department %in% 
            c("Data science", 
              "Data Science & Engineering")] <- "Data Science"
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
    
    #filter to selected quarter
    if(input$quarter != "All"){
      consults <- consults %>% filter(quarter == input$quarter)
    }
    
    #filter to selected year
    if(input$year != "All"){
      y <- as.numeric(input$year)
      consults <- consults %>% filter(year == y)
    }
    
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
    #make date back to char for better output
    instruction_data$date <- as.character(instruction_data$date)
    
    #returns df
    return(instruction_data)
    
  })
  
  #render outreach data frame
  output$outreach_data <- renderTable({
    
    #read in the user data
    outreach <- Sortie_outreach() #return Sortie function
    
    #make date char
    outreach$date <- as.character(outreach$date)
    
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
    date_min <- as.character(min(consults$date))
    date_max <- as.character(max(consults$date))
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
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #drop NA departments
    consults <- consults[is.na(consults$department)== FALSE,]
    
    #let user select minimum n of dept
    #consults <- consults[consults$dept_consult_count > n,]
    
    #possibly add radio buttons for setting dates, cutoffs of dept count
    
    fig <- ggplotly(
      consults %>% group_by(department) %>%
      count(department) %>%
      ggplot(aes(x = reorder(department,n), y = n, fill = department)) +
      geom_col(alpha = 1) +
      #geom_text(aes(label = n), hjust = -1) +
      coord_flip() +
      ggtitle("Most Frequently Consulted Departments") +
      theme_bw() +
      labs(x = NULL, y = "count") +
      theme(legend.position="none")
      ) %>% layout(height = 600) %>%
        config(displayModeBar = F)
      
    return(fig)
  })
  
  ## instruction over time
  output$instruction_time_plot <- renderPlotly({
  
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in df
    instruction <- Sortie_instruction()
    
    #ensure date class
    #instruction$date <- mdy(instruction$date)
    
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
        theme(legend.position="none")
      ) %>% config(displayModeBar = F) 
    
    return(fig1)
  })
  

})
