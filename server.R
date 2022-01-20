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
    
    #week of quarter
    instruction <- week_to_quarter(instruction)
    
    #week of quarter
    instruction <- week_of_quarter(instruction)
    
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
    
    #week to quarter
    outreach <- week_to_quarter(outreach)
    
    #week of quarter
    outreach <- week_of_quarter(outreach)
    
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
    
    #week to quarter
    consults <- week_to_quarter(consults)
    
    #week of quarter
    consults <- week_of_quarter(consults)
    
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
  
  #############################################################
  ##### Returning updated inputs ##############################
  
  ###### check if "ALL" is selected
  check_all <- reactive({
    #filter to selected quarter
    if(input$quarter == "All" & input$year == "All"){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  #return n
  return_n <- reactive({
    return(input$n)
  })
  
  
  ###################################################################
  ### Render Tables  ############
  ###################################################################
  
  #render instruction data frame
  output$instruction_data <- renderTable({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    instruction_data <- Sortie_instruction() #return Sortie function
    #make date back to char for better output
    instruction_data$date <- as.character(instruction_data$date)
    
    #returns df
    return(instruction_data)
    
  })
  
  #render outreach data frame
  output$outreach_data <- renderTable({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
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
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
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
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    outreach <- Sortie_outreach() #return Sortie function
    
    #now the text render part
    out_num_activities <- nrow(outreach)
    out_num_students <- sum(outreach$attendees)
    
    #return text stat
    return(paste("There were",out_num_activities,
                 "outreach activities reaching",
                out_num_students,"students"))
    
  })
  
  ## text summary stats for Consults
  output$consults_stats <- renderText({ 
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #now the text render part
    #how many consults in what date range?
    date_min <- as.character(min(consults$date))
    date_max <- as.character(max(consults$date))
    num_consults <- nrow(consults)
    num_people_consulted <- sum(consults$num_consult)
    num_departments <- length(unique(consults$department))
    
    condition <- check_all()
    
    if(condition){
      return(paste("There were",num_consults,"consults from",date_min,"to",
                   date_max, "reaching",num_people_consulted, "people in", 
                   num_departments, "unique departments")) 
    }
    else{
      return(paste("There were",num_consults, "consults reaching",
                   num_people_consulted, "people in", 
                   num_departments, "unique departments"))
    }
    
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
    n <- return_n()
    consults <- consults[consults$dept_consult_count >= n,]
    
    
    #make the title  reactive to n
    title <- paste("Most Frequently Consulted Departments (n > ", 
                   n,")" , sep = '')
    
    #possibly add radio buttons for setting dates, cutoffs of dept count
    
    fig <- ggplotly(
      consults %>% group_by(department) %>%
      count(department) %>%
      ggplot(aes(x = reorder(department,n), y = n, fill = department)) +
      geom_col(alpha = 1) +
      #geom_text(aes(label = n), hjust = -1) +
      coord_flip() +
      ggtitle(title) +
      theme_bw() +
      labs(x = NULL, y = "count") +
      theme(legend.position="none")
      ) %>% #layout(height = 600) %>%
        config(displayModeBar = F)
      
    return(fig)
  })
  
  #num consults per week over time
  output$consults_per_week <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    fig <- ggplotly(
      consults %>% group_by(week, year) %>%
        count(week) %>% 
        ggplot(aes(x = week, y = n, fill = year)) +
        geom_bar(stat = "identity", position = 'dodge') +
        ggtitle("Weekly Consults") +
        labs(y = "Number of Consults") +
        theme_bw()
    ) %>% config(displayModeBar = F) 

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
  
  #################################################
  ## FILE DOWNLOADS   #############################
  ################################################
  
  output$downloadConsults <-downloadHandler(
    
    filename = 'consults.csv',
    
    content = function(file){
      write.csv(Sortie_consults(),file,row.names = FALSE)
    }
    
  )
  
  output$downloadInstruction <-downloadHandler(
    
    #filename, can be a function! will make reactvie to Q/year input
    filename = 'instruction.csv',
    
    content = function(file){
      write.csv(Sortie_instruction(),file,row.names = FALSE)
    }
    
  )
  
  output$downloadOutreach <-downloadHandler(
    
    #filename, can be a function! will make reactvie to Q/year input
    filename = 'outreach.csv',
    
    content = function(file){
      write.csv(Sortie_outreach(),file,row.names = FALSE)
    }
    
  )
  
  
  
})

####################################################################
########## HELPER FUNCTIONS ########################################
###################################################################

## Helper function for determining quarters by year
week_to_quarter <- function(df){
  
  #use isoweek(mdy("12/14/19")) (month/day/year) (needs lubridate)
  # while browsing academic calendar to quickly get week cutoffs
  
  #init empty quarter column
  df$quarter <- c()
  
  #for 2019
  df$quarter[df$week >=2 & df$week <= 12 &
               df$year == 2019] <- "WI"
  df$quarter[df$week >=14 & df$week <= 24 & 
               df$year == 2019] <- "SP"
  df$quarter[df$week >=27 & df$week <= 36 &
               df$year == 2019] <- "SU"
  df$quarter[df$week >= 39 & df$week <= 50 &
               df$year == 2019] <- "FA"
  
  #for 2020
  df$quarter[df$week >=2 & df$week <= 12 &
               df$year == 2020] <- "WI"
  df$quarter[df$week >=14 & df$week <= 24 & 
               df$year == 2020] <- "SP"
  df$quarter[df$week >=27 & df$week <= 36 &
               df$year == 2020] <- "SU"
  df$quarter[df$week >= 40 & df$week <= 51 &
               df$year == 2020] <- "FA"
  
  
  #for 2021
  df$quarter[df$week >=1 & df$week <= 11 &
               df$year == 2021] <- "WI"
  df$quarter[df$week >=13 & df$week <= 23 & 
               df$year == 2021] <- "SP"
  df$quarter[df$week >=26 & df$week <= 35 &
               df$year == 2021] <- "SU"
  df$quarter[df$week >= 38 & df$week <= 49 &
               df$year == 2021] <- "FA"
  
  #for 2022
  df$quarter[df$week >=1 & df$week <= 11 &
               df$year == 2022] <- "WI"
  df$quarter[df$week >=13 & df$week <= 23 & 
               df$year == 2022] <- "SP"
  df$quarter[df$week >=26 & df$week <= 35 &
               df$year == 2022] <- "SU"
  df$quarter[df$week >= 38 & df$week <= 49 &
               df$year == 2022] <- "FA"
  
  #all other are breaks
  df$quarter[is.na(df$quarter)] <- "Break"
  
  return(df)
}

#######
## week of quarter

week_of_quarter <- function(df){
  
  #init empty column
  df$week_of_quarter <- c()
  
  #for 2019
  df$week_of_quarter[df$quarter == "WI" & df$year == 2019] <- 
    df$week[df$quarter == "WI" & df$year == 2019] - 1
  df$week_of_quarter[df$quarter == "SP" & df$year == 2019] <- 
    df$week[df$quarter == "SP" & df$year == 2019] - 13
  df$week_of_quarter[df$quarter == "SU" & df$year == 2019] <- 
    df$week[df$quarter == "SU" & df$year == 2019] - 26
  df$week_of_quarter[df$quarter == "FA" & df$year == 2019] <- 
    df$week[df$quarter == "FA" & df$year == 2019] - 39 #we want a 0 week here
  
  
  #for 2020
  df$week_of_quarter[df$quarter == "WI" & df$year == 2020] <- 
    df$week[df$quarter == "WI" & df$year == 2020] - 1
  df$week_of_quarter[df$quarter == "SP" & df$year == 2020] <- 
    df$week[df$quarter == "SP" & df$year == 2020] - 13
  df$week_of_quarter[df$quarter == "SU" & df$year == 2020] <- 
    df$week[df$quarter == "SU" & df$year == 2020] - 26
  df$week_of_quarter[df$quarter == "FA" & df$year == 2020] <- 
    df$week[df$quarter == "FA" & df$year == 2020] - 40 #we want a 0 week here
  
  
  #for 2021
  df$week_of_quarter[df$quarter == "WI" & df$year == 2021] <- 
    df$week[df$quarter == "WI" & df$year == 2021] - 0
  df$week_of_quarter[df$quarter == "SP" & df$year == 2021] <-
    df$week[df$quarter == "SP" & df$year == 2021] - 12
  df$week_of_quarter[df$quarter == "SU" & df$year == 2021] <-
    df$week[df$quarter == "SU" & df$year == 2021] - 25
  df$week_of_quarter[df$quarter == "FA" & df$year == 2021] <-
    df$week[df$quarter == "FA" & df$year == 2021] - 38 #we want a 0 week here
  
  
  #for 2022
  df$week_of_quarter[df$quarter == "WI" & df$year == 2022] <- 
    df$week[df$quarter == "WI" & df$year == 2022] - 0
  df$week_of_quarter[df$quarter == "SP" & df$year == 2022] <-
    df$week[df$quarter == "SP" & df$year == 2022] - 12
  df$week_of_quarter[df$quarter == "SU" & df$year == 2022] <-
    df$week[df$quarter == "SU" & df$year == 2022] - 25
  df$week_of_quarter[df$quarter == "FA" & df$year == 2022] <-
    df$week[df$quarter == "FA" & df$year == 2022] - 38 #we want a 0 week here
  
  return(df)
}
