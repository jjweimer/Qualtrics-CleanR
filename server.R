library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT) #for better tables
library(stringdist) #fuzzy matching
library(rintrojs) #js library for intro

#require fuzzy match, helper functions
source('fuzzy_match.R', local = TRUE)
source("week_quarter_helper.R", local = TRUE)

#max file size 30mb for upload
options(shiny.maxRequestSize = 30*1024^2)

# Define server logic 
shinyServer(function(input, output,session) {
  
  
  #----- TUTORIAL ----------------------------------------------------
  
  observeEvent(input$help,
               introjs(session, 
                       options = list("nextLabel" = "Next",
                                               "prevLabel" = "Previous",
                                               "skipLabel" = ""
                                      ),
                       events = list("oncomplete" = I('alert("Tutorial Complete!")')
                                     )
                       ))

  #----- DATA WRANGLE/CLEANING -------------------------------------------

  #read user inputted data set once
  Sortie_master <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath)
    return(user_data)
    
  })
  
  #clean instruction data, return data frame
  Sortie_instruction <- reactive({
    
    #read in the user data
    user_data <- Sortie_master()
    
    if (is.null(user_data))
      return(NULL)
    
    #now clean data for instruction_data
    instruction <- user_data %>% select(Q2,Q3, Q16, Q192.1, Q17, Q21)
    instruction <- instruction[instruction$Q3 == "Instruction",]
    
    #check that there are nonzero number of rows
    if(nrow(instruction) == 0){
      return(NULL)
    }
    
    colnames(instruction) <- c("entered_by","service","date","format",
                               "activity","num_attendants")
    
    #make num_attendants numeric
    instruction$num_attendants <- as.numeric(instruction$num_attendants)
    #ensure date data type
    instruction$date <- mdy(instruction$date)
    
    #week and quarter
    instruction$week <- isoweek(instruction$date)
    instruction <- instruction %>% relocate(week, .after = date)
    
    #get year
    instruction$year <- format(instruction$date,format =  '%Y')
    
    #clean years
    instruction <- clean_years(instruction)
    
    #week to quarter
    instruction <- week_to_quarter(instruction)
    
    #week of quarter
    instruction <- week_of_quarter(instruction)
    
    #month-day
    instruction <- month_day(instruction)
    
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
   
     #read in the user data
    user_data <- Sortie_master()
    
    if (is.null(user_data))
      return(NULL)
    
    #now clean data for outreach_data
    outreach <- user_data %>% select(Q2,Q3, Q156, Q198, Q174, Q182, Q184, 
                                     Q194, Q202, Q196)
    outreach <- outreach[outreach$Q3 == "Outreach",]
    #check that there are nonzero number of rows
    if(nrow(outreach) == 0){
      return(NULL)
    }
    colnames(outreach) <- c("entered_by","service","date","type","home_program",
                            "topic","attendees","status","duration","time_prep")
    
    #drop empty obs
    outreach <- outreach[!is.na(outreach$date),]
    outreach <- outreach[!outreach$date == "",]
    
    #ensure date data type
    outreach$date <- mdy(outreach$date)
    #make attendees numeric
    outreach$attendees <- as.numeric(outreach$attendees)
    
    #get week of year, year, quarter
    outreach$week <- isoweek(outreach$date)
    outreach <- outreach %>% relocate(week, .after = date)
    
    #get year
    outreach$year <- format(outreach$date,format =  '%Y')
    
    #clean years
    outreach <- clean_years(outreach)
    
    #week to quarter
    outreach <- week_to_quarter(outreach)
    
    #week of quarter
    outreach <- week_of_quarter(outreach)
    
    #month-day
    outreach <- month_day(outreach)
    
    #for some reason without this, it returns a ton of missing values as rows
    #this should do nothing but is like integral somehow
    outreach <- outreach[outreach$quarter %in% c("WI","SP","SU","FA", "Break"),]
    
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
    
    #read in the user data
    user_data <- Sortie_master()
    
    if (is.null(user_data))
      return(NULL)
    
    #now clean data for consults_data
    consults <- user_data %>% select(Q2,Q3,Q38,Q39,Q40,Q42,Q43,Q44,Q45)
    consults <- consults[consults$Q3 == 'Consultation',]
    #check that there are nonzero number of rows
    if(nrow(consults) == 0){
      return(NULL)
    }
    
    colnames(consults) <- c("entered_by","service","date","location",
                            "department", "num_consult","category",
                            "time_spent","status")
    #ensure date class
    consults$date <- mdy(consults$date)
    #make num_consult numeric
    consults$num_consult <- as.numeric(consults$num_consult)
    #some NAs, lets make these 1 by default
    consults$num_consult[is.na(consults$num_consult)] <- 1
    
    #make time spent numeric
    consults$time_spent[consults$time_spent == "greater than 10"] <- '10'
    consults$time_spent <- as.numeric(consults$time_spent)
    consults$time_spent[is.na(consults$time_spent)] <- 0
    
    #add week of year,quarters(works for 2021 and 2020, need to check for other)
    consults$week <- isoweek(consults$date)
    consults <- consults %>% relocate(week, .before = location)
    
    #get year
    consults$year <- format(consults$date,format =  '%Y')
    
    #clean years
    consults <- clean_years(consults)
    
    #week to quarter
    consults <- week_to_quarter(consults)
    
    #week of quarter
    consults <- week_of_quarter(consults)
    
    #month-day
    consults <- month_day(consults)
    
    ## fuzzy match department names
    consults$fuzzy_department <- fuzzy_match(consults$department)
    consults <- consults %>% relocate(fuzzy_department, .after = department)
    
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
  
  Sortie_info_RAD <- reactive({
    
    #read in the user data
    user_data <- Sortie_master()
    
    if (is.null(user_data))
      return(NULL)
    
    #now clean for info/RAD data
    info <- user_data[user_data$Q2 %in% c("RAD","Info Desk"),
                      c("RecordedDate","Q2","Q26","Q27","Q31")]
    #check that there are nonzero number of rows
    if(nrow(info) == 0){
      return(NULL)
    }
    
    #merge columns 27 and 31 into new column
    info$pasted <- paste(info$Q27,info$Q31, sep = "")
    
    #date time conversion
    info$date_time <- as.POSIXct(info$RecordedDate, 
                             format = "%m/%d/%Y %H:%M", 
                             tz = "America/Los_Angeles")
    
    #extract year and week
    info$year <-  format(info$date_time,format =  '%Y')
    info$week <- isoweek(info$date_time)
    
    #select down to the rows we want, and rename cols
    info <- info %>% select(c("year","week","date_time","Q2","pasted"))
    colnames(info) <- c("year","week","date_time","desk","service")
    
    #week of quarter and quarter
    info <- week_to_quarter(info)
    info <- week_of_quarter(info)
    
    #month_day
    info <- month_day(info)
    
    #filter to selected quarter
    if(input$quarter != "All"){
      info <- info %>% filter(quarter == input$quarter)
    }
    
    #filter to selected year
    if(input$year != "All"){
      y <- as.numeric(input$year)
      info <- info %>% filter(year == y)
    }
    
    #return our guy
    return(info)
    
  })
  
  Sortie_data_gis <- reactive({
    
    #read in the user data
    user_data <- Sortie_master()
    
    if (is.null(user_data))
      return(NULL)
    
    #select the columns/rows we need
    gis_lab <- user_data[user_data$Q2 == "Data/GIS Lab",]
    gis_lab <- gis_lab %>% select(c("RecordedDate","Q2","Q49","Q50","Q51",
                                    "Q52","Q53",'Q89','Q90'))
    #check that there are nonzero number of rows
    if(nrow(gis_lab) == 0){
      return(NULL)
    }
    
    #colnames
    colnames(gis_lab) <- c("RecordedDate","location","entry_type","user_status",
                           "department","visit_purpose","question_type",
                           "date","hour")
    
    #convert RecordedDate to useful date-times
    #date time conversion
    gis_lab$date_time <- as.POSIXct(gis_lab$RecordedDate, 
                                 format = "%m/%d/%Y %H:%M", 
                                 tz = "America/Los_Angeles")
    
    #fill in missing "date" values with recordeddate
    gis_lab$date[is.na(gis_lab$date) | gis_lab$date == ""] <- format(gis_lab$date_time[is.na(gis_lab$date) | gis_lab$date == ""], 
                                                                     format = "%m/%d/%Y")
    #convert to date class
    gis_lab$date <- mdy(gis_lab$date)
    
    #extract year and week
    gis_lab$year <-  format(gis_lab$date,format =  '%Y')
    gis_lab$week <- isoweek(gis_lab$date)
    
    #week of quarter and quarter
    gis_lab <- week_to_quarter(gis_lab)
    gis_lab <- week_of_quarter(gis_lab)
    
    #month_day
    gis_lab <- month_day(gis_lab)
    
    ## fuzzy match department names
    gis_lab$fuzzy_department <- fuzzy_match(gis_lab$department)
    gis_lab <- gis_lab %>% relocate(fuzzy_department, .after = department)
    
    #filter to selected quarter
    if(input$quarter != "All"){
      gis_lab <- gis_lab %>% filter(quarter == input$quarter)
    }
    
    #filter to selected year
    if(input$year != "All"){
      y <- as.numeric(input$year)
      gis_lab <- gis_lab %>% filter(year == y)
    }
    
    #return df
    return(gis_lab)
    
  })
  
  #----- CHECK ALL ----------------------------------------------------
  
  check_all <- reactive({
    #filter to selected quarter
    if(input$quarter == "All" & input$year == "All"){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  #----- DATA TABLES ----------------------------------------------------
  
  #instruction DT table
  output$instruction_DT <- DT::renderDataTable(Sortie_instruction(),
                                               options = list(scrollX = TRUE),
                                               rownames = FALSE)
  #outreach data table
  output$outreach_DT <- DT::renderDataTable(Sortie_outreach(),
                                            options = list(scrollX = TRUE),
                                            rownames = FALSE)
  #consults 
  output$consults_DT <- DT::renderDataTable(Sortie_consults(),
                                            options = list(scrollX = TRUE),
                                            rownames = FALSE)
  #info/RAD
  output$info_DT <- DT::renderDataTable(Sortie_info_RAD(),
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
  #info/RAD service counts
  desk_serv_counts <- reactive({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_info_RAD())){
      return(NULL)
    }
    
    #read in the user data
    info <- Sortie_info_RAD() #return Sortie function
    
    #service counts
    serv_counts <- info %>% group_by(service) %>% count(service)
    serv_counts <- arrange(serv_counts,-n)
    
    return(serv_counts)
    
  })
  
  output$serv_counts_DT <- DT::renderDataTable(desk_serv_counts(),
                                               options = list(scrollX = TRUE),
                                               rownames = FALSE)
  #data GIS LAB
  output$data_gis_DT <- DT::renderDataTable(Sortie_data_gis(),
                                            options = list(scrollX = TRUE),
                                            rownames = FALSE)
    
  #----- TEXT STATS ----------------------------------------------------
  
  #generate instruction text summary stats
  output$instruction_stats <- renderText({ 
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_instruction())){
      return("No data found")
    }
    
    #read in the user data
    instruction_data <- Sortie_instruction() #return Sortie function
  
    #now the text render part
    ins_num_activities <- nrow(instruction_data)
    ins_num_groups<- length(unique(instruction_data$activity))
    ins_num_students_reached <- sum(instruction_data$num_attendants[
                                  !is.na(instruction_data$num_attendants)])
    #return text
    return(paste("There were",ins_num_activities,
                 "instruction acitivites reaching",ins_num_groups,
                 "groups and",ins_num_students_reached,"students"))
    
  })
  
  #generate outreach text summary stats
  output$outreach_stats <- renderText({ 
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_outreach())){
      return("No data found")
    }
    
    #read in the user data
    outreach <- Sortie_outreach() #return Sortie function
    
    #now the text render part
    out_num_activities <- nrow(outreach)
    out_num_students <- sum(outreach$attendees[!is.na(outreach$attendees)])
    
    #return text stat
    return(paste("There were",out_num_activities,
                 "outreach activities reaching",
                out_num_students,"students"))
    
  })
  
  ## text summary stats for Consults
  output$consults_stats <- renderText({ 
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_consults())){
      return("No data found")
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    num_hours <- round(sum(consults$time_spent[
                            !is.na(consults$time_spent)]), -1)#round to nearest 10
    num_consults <- nrow(consults)
    num_people_consulted <- sum(consults$num_consult[
                                !is.na(consults$num_consult)])
    num_departments <- length(unique(consults$fuzzy_department))
    
    return(paste("There were",num_consults, "consults reaching",
                 num_people_consulted, "people in", 
                 num_departments, "unique departments. There 
                 were approximately",num_hours,"hours spent on consultations."))
  })
  
  output$info_stats <- renderText({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_info_RAD())){
      return("No data found")
    }
    
    #read in the user data
    info <- Sortie_info_RAD() #return Sortie function
    
    #total desk services
    num_services <- nrow(info)
    
    #total RAD
    num_RAD <- nrow(info[info$desk == "RAD",])
    
    #total info desk
    num_info <- nrow(info[info$desk == "Info Desk",])
    
    return(paste("There were", num_services, "services. Of those,", num_RAD,
                 "were from  the Research Assistance Desk, and", num_info, 
                 "were from the Info Desk."))
    
  })
  
  output$gis_stats <- renderText({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_data_gis())){
      return("No data found")
    }
    
    #read in the user data
    gis_lab <- Sortie_data_gis() #return Sortie function
    
    #total  services
    num_services <- nrow(gis_lab)
    
    #num_questions
    num_questions <- nrow(gis_lab[gis_lab$entry_type == "Question Asked",])
    
    #how many GIS/study/Data uses
    num_GIS <- nrow(gis_lab[gis_lab$visit_purpose == "GIS",])
    num_study <- nrow(gis_lab[gis_lab$visit_purpose == "Study",])
    num_data <- nrow(gis_lab[gis_lab$visit_purpose == "Data",])
    
    
    return(paste("The Data & GIS Lab provided",num_services, "services during 
                 the time specified. Of these,", num_questions, "were answered
                 questions. The Lab had",num_study, "people visit to study,",
                 num_GIS, "people visit for GIS services/software, and",
                 num_data,"people visit for data services/software."))
    
  })
  
  #----- PLOTS ----------------------------------------------------
  
  #consults department counts
  output$consults_graph <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_consults())){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    user_choice <- input$is_fuzzy
    
    if(user_choice == "Matched"){
      #drop NA departments
      consults <- consults[!is.na(consults$fuzzy_department),]
      
      ##get dept counts
      dept_counts <- consults %>% group_by(fuzzy_department) %>%
        count(fuzzy_department) %>% arrange(-n)
      
      #let user select minimum n of dept
      n_department <- input$n
      #filter for n 
      dept_counts <- dept_counts[1:n_department,]
      
      #make the title  reactive to n
      title <- paste("Most Consulted Departments (n >= ", 
                     n_department,")" , sep = '')
      
      fig <- ggplotly(
        dept_counts %>%
          ggplot(aes(x = reorder(fuzzy_department,n), y = n, fill = n)) +
          geom_col(alpha = 1) +
          #geom_text(aes(label = n), hjust = -1) +
          coord_flip() +
          ggtitle("Most Consulted Departments") +
          theme_bw() +
          labs(x = NULL, y = "count") +
          theme(legend.position="none")
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig)
      
    } else if (user_choice == "Raw") {
      
      #drop NA departments
      consults <- consults[!is.na(consults$department),]
      
      ##get dept counts
      dept_counts <- consults %>% group_by(department) %>%
        count(fuzzy_department) %>% arrange(-n)
      
      #let user select minimum n of dept
      n_department <- input$n
      #filter for n 
      dept_counts <- dept_counts[1:n_department,]
      
      #make the title  reactive to n
      title <- paste("Most Consulted Departments (n >= ", 
                     n_department,")" , sep = '')
      
      fig1 <- ggplotly(
        dept_counts %>%
          ggplot(aes(x = reorder(department,n), y = n, fill = n)) +
          geom_col(alpha = 1,) +
          #geom_text(aes(label = n), hjust = -1) +
          coord_flip() +
          ggtitle("Most Consulted Departments") +
          theme_bw() +
          labs(x = NULL, y = "count") +
          theme(legend.position="none")
      ) %>% #layout(height = 600) %>%
        config(displayModeBar = F)
      
      return(fig1)
    }
   
  })
  
  #num consults over time
  output$consults_over_time <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_consults())){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #get user time scale
    user_consults_scale <- input$consults_scale
    
    if(user_consults_scale == "Weekly"){
      
      #aggregate weekly 
      weekly_data <- consults %>% group_by(week,year) %>%
        count(week)
      
      #plot
      fig1 <- ggplotly(
        weekly_data[!is.na(weekly_data$year),] %>% 
          ggplot(aes(x = week, y = n, fill = year)) +
          geom_bar(stat='identity') +
          ggtitle("Weekly Consults") +
          labs(x = NULL, y = "Number of Consults") +
          theme_bw() +
          scale_x_continuous(breaks = month_numeric, 
                             labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig1)
      
    }else if(user_consults_scale == "Daily"){
      
      #aggregate daily
      daily_data <- consults %>% group_by(month_day, year) %>%
        count(month_day)
      #make sure each date occurs at lesat once so axes work well
      daily_data <- all_daily_dates(daily_data)
      
      #plot
      fig2 <- ggplotly(
        daily_data[!is.na(daily_data$year),] %>% 
          ggplot(aes(x = month_day, y = n, fill = year)) +
          geom_bar(stat= 'identity') +
          theme_bw() +
          labs(x = NULL, y = "Number of Consults") +
          ggtitle("Daily Consults") +
          scale_x_discrete(breaks = month_2, 
                           labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig2)
    }
    
  })
  
  #intra-quarter weekly consults
  output$intra_quarter_consults <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_consults())){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    weekly_data <- consults %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- input$quarter
    
    title <- paste("Consults per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data[!is.na(weekly_data$year),] %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity", position = input$consults_position) +
        ggtitle(title) +
        xlim(0,11) +
        labs(y = "Number of Consults", x = "Week") +
        theme_bw() +
        scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11))
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                        "lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
    
  })
  
  #consult locations
  output$consult_locations <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_consults())){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #make location df
    locations_data <- consults %>% 
      group_by(location) %>% 
      count(location)
    
    locations_data$location[
      locations_data$location == 
        "Teleconference session (e.g. Zoom, Skype, IM)"] <- "Teleconference"
    
    #plot
    
    fig <- ggplotly(
            locations_data %>% 
            ggplot(aes(x = n, y = reorder(location,n), fill = location)) +
            geom_col() +
              labs(y= NULL) +
              ggtitle("Most Frequent Consult Locations") +
            theme_bw() +
            theme(legend.position = 'none')
            ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                        "lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
    
  })
  
  ## consult categories
  output$consult_categories <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_consults())){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    ## consults categories
    categories <- consults %>% group_by(category) %>% 
      count(category) %>% arrange(-n)
    
    #let user select minimum n of dept
    n_category <- input$n_category
    categories <- categories[1:n_category,]
    
    # plot as col plot
    fig <- ggplotly(
      categories %>%
      ggplot(aes(x = reorder(category,n), y = n, fill = n)) +
      geom_col() +
      coord_flip() +
      ggtitle("Consult Categories") +
      theme_bw() +
      labs(x = NULL, y = "count") +
      theme(legend.position="none")
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "zoom2d","lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
    
  })
  
  ##instruction by week of quarter
  output$intra_quarter_instruction <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_instruction())){
      return(NULL)
    }
    
    #read in the user data
    instruction <- Sortie_instruction() #return Sortie function
    
    weekly_data <- instruction %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- input$quarter
    
    title <- paste("Instruction Events per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data[!is.na(weekly_data$year),] %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity", position = input$instruction_position) +
        ggtitle(title) +
        xlim(1,10) +
        labs(y = "Number of Instruction Events", x = "Week") +
        theme_bw() +
        scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11))
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "zoom2d","lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
    
  })
  
  ## instruction events over time
  output$instruction_time_plot <- renderPlotly({
  
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_instruction())){
      return(NULL)
    }
    
    #read in df
    instruction <- Sortie_instruction()
    
    #user format selection
    format <- input$instruction_format_selector_2
    #filter by inputted format
    if(format != "All"){
      instruction <- instruction[instruction$format == format,]
    } 
    
    user_instruction_scale <- input$instruction_scale
    
    if(user_instruction_scale == "Weekly"){
      
      #aggregate weekly
      weekly_data <- instruction %>% group_by(week,year) %>%
        count(week)
      
      #plot
      fig1 <- ggplotly(
        weekly_data %>% 
          ggplot(aes(x = week, y = n, fill = year)) +
          geom_bar(stat='identity') +
          ggtitle("Weekly Instruction Events") +
          labs(x = NULL, y = "Number of Instruction Events") +
          theme_bw() +
          scale_x_continuous(breaks = month_numeric, 
                             labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                          "zoom2d","lasso2d",
                                          "pan2d","autoscale2d",
                                          "select2d"))
      
      return(fig1)
      
    } else if(user_instruction_scale == "Daily"){
      
      #aggregate daily
      daily_data <- instruction %>% group_by(month_day, year) %>%
        count(month_day)
      
      #make sure each date occurs at lesat once so axes work well
      daily_data <- all_daily_dates(daily_data)
      
      #plot
      fig2 <- ggplotly(
        daily_data %>% 
          ggplot(aes(x = month_day, y = n, fill = year)) +
          geom_bar(stat= 'identity') +
          theme_bw() +
          ggtitle("Daily Instruction Events")+
          labs(x = NULL, y = "Number of Instruction Events") +
          scale_x_discrete(breaks = month_2, 
                           labels = month_label)
      )%>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                          "zoom2d","lasso2d",
                                          "pan2d","autoscale2d",
                                          "select2d"))
      
      return(fig2)
    }
    
  })
  
  #number of people instructed over time
  output$instruction_num_people_time <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_instruction())){
      return(NULL)
    }
    
    #read in df
    instruction <- Sortie_instruction()
    
    #selector input
    format <- input$instruction_format_selector
    
    if(format == "All"){
      #aggregate data for weekly counts of instructed people
      weekly_instructed <- instruction %>% group_by(week,year) %>%
        mutate(weekly_num_instructed = sum(num_attendants)) %>%
        group_by(week,year) %>% slice(1)
      
     
    } else {
      
      #aggregate data for weekly counts of instructed people
      weekly_instructed <- instruction[instruction$format == format,] %>%
        group_by(week,year) %>%
        mutate(weekly_num_instructed = sum(num_attendants)) %>%
        group_by(week,year) %>% slice(1)
      
    }
    
    #plot
    fig <- ggplotly(
      weekly_instructed %>%
        ggplot(aes(x = week, y = weekly_num_instructed, fill = year)) +
        geom_bar(stat = 'identity') +
        ggtitle("Weekly People Instructed") +
        labs(x = NULL, y = "People Instructed") +
        theme_bw() +
        scale_x_continuous(breaks = month_numeric, 
                           labels = month_label)
      
    )%>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "zoom2d","lasso2d",
                                        "pan2d","autoscale2d",
                                        "select2d"))
    return(fig)
    
  })
  
  #info / RAD week of quarter
  output$intra_quarter_RAD <- renderPlotly({
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_info_RAD())){
      return(NULL)
    }
    
    #read in the user data
    info <- Sortie_info_RAD() #return Sortie function
    
    weekly_data <- info %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- input$quarter
    
    title <- paste("Services per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data[!is.na(weekly_data$year),] %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity", position = input$info_position) +
        ggtitle(title) +
        xlim(0,11) +
        labs(y = "Number of Services", x = "Week") +
        theme_bw() +
        scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11))
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "zoom2d","lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
  })
  
  #info/RAD services over time
  output$info_time_plot <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_info_RAD())){
      return(NULL)
    }
    
    #read in df
    info <- Sortie_info_RAD()
    
    #check user time scale
    user_info_scale <-input$info_scale
    
    if(user_info_scale == "Weekly"){
      
      #aggregate weekly
      weekly_data <- info %>% group_by(week,year) %>%
        count(week)
      
      #plot
      fig1 <- ggplotly(
        weekly_data %>% 
          ggplot(aes(x = week, y = n, fill = year)) +
          geom_bar(stat='identity') +
          ggtitle("Weekly Research Assistance / Info Desk Services") +
          labs(x = NULL, y = "Number of Services") +
          theme_bw() +
          scale_x_continuous(breaks = month_numeric, 
                             labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                          "zoom2d","lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig1)
      
    } else if (user_info_scale == "Daily"){
      
      #aggregate daily
      daily_data <- info %>% group_by(month_day, year) %>%
        count(month_day)
      
      #make sure each date occurs at least once so axes work well
      daily_data <- all_daily_dates(daily_data)
      
      #plot
      #plot
      fig2 <- ggplotly(
        daily_data %>% 
          ggplot(aes(x = month_day, y = n, fill = year)) +
          geom_bar(stat= 'identity') +
          theme_bw() +
          labs(x = NULL, y = "Number of Services") +
          ggtitle("Daily RAD/info Desk Services") +
          scale_x_discrete(breaks = month_2, 
                           labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      return(fig2)
    }
  })
  
  #week of quarter data and GIS lab use
  output$week_of_quarter_gis_lab <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_data_gis())){
      return(NULL)
    }
    
    #read in the user data
    gis_lab <- Sortie_data_gis() #return Sortie function
    
    weekly_data <- gis_lab %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- input$quarter
    
    title <- paste("Data & GIS Lab visits per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity",position = input$gis_position) +
        ggtitle(title) +
        xlim(0,11) +
        labs(y = "Number of Lab Visits", x = "Week") +
        theme_bw() +
        scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11))
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                        "lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
    
  })
  
  #week of year data and gis lab use
  output$gis_lab_per_week <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_data_gis())){
      return(NULL)
    }
    
    #read in the user data
    gis_lab <- Sortie_data_gis() #return Sortie function
    
    weekly_data <- gis_lab %>% group_by(week,year) %>%
      count(week)
    
    #create month labels
    month <- seq(as.Date("2020-01-01"), 
                 as.Date("2020-12-01"), 
                 by = "1 month")
    #splits of when each week count corresponds to change in month
    month_numeric <- as.numeric(format(month, format = "%U"))
    month_numeric <- month_numeric + 1
    #string labels
    month_label <- format(month, format = "%b")
    
    #plot
    fig1 <- ggplotly(
      weekly_data %>% 
        ggplot(aes(x = week, y = n, fill = year)) +
        geom_bar(stat='identity') +
        ggtitle("Weekly Data & GIS Lab Visits") +
        labs(x = NULL, y = "Number of Lab Visits") +
        theme_bw() +
        scale_x_continuous(breaks = month_numeric, 
                           labels = month_label)
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                        "lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig1)
    
  })
  
  #gis lab departments
  output$gis_lab_departments <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_data_gis())){
      return(NULL)
    }
    
    #read in the user data
    gis_lab <- Sortie_data_gis() #return Sortie function
    
    user_choice <- input$is_fuzzy_gis
    
    if(user_choice == "Matched"){
      #drop NA departments
      gis_lab <- gis_lab[!is.na(gis_lab$fuzzy_department),]
      
      ##get dept counts
      dept_counts <- gis_lab %>% group_by(fuzzy_department) %>%
        count(fuzzy_department) %>% arrange(-n)
      
      #let user select minimum n of dept
      n_department <- input$n_gis
      #filter for n 
      dept_counts <- dept_counts[1:n_department,]
      
      #make the title  reactive to n
      title <- paste("Most Consulted Departments (n >= ", 
                     n_department,")" , sep = '')
      
      fig <- ggplotly(
        dept_counts %>%
          ggplot(aes(x = reorder(fuzzy_department,n), y = n, fill = n)) +
          geom_col(alpha = 1) +
          #geom_text(aes(label = n), hjust = -1) +
          coord_flip() +
          ggtitle("Most Frequent Departments") +
          theme_bw() +
          labs(x = NULL, y = "count") +
          theme(legend.position="none")
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig)
      
    } else if (user_choice == "Raw") {
      
      #drop NA departments
      gis_lab <- gis_lab[!is.na(gis_lab$department),]
      
      ##get dept counts
      dept_counts <- gis_lab %>% group_by(department) %>%
        count(fuzzy_department) %>% arrange(-n)
      
      #let user select minimum n of dept
      n_department <- input$n_gis
      #filter for n 
      dept_counts <- dept_counts[1:n_department,]
      
      #make the title  reactive to n
      title <- paste("Most Consulted Departments (n >= ", 
                     n_department,")" , sep = '')
      
      fig1 <- ggplotly(
        dept_counts %>%
          ggplot(aes(x = reorder(department,n), y = n, fill = n)) +
          geom_col(alpha = 1,) +
          #geom_text(aes(label = n), hjust = -1) +
          coord_flip() +
          ggtitle("Most Frequent Departments") +
          theme_bw() +
          labs(x = NULL, y = "count") +
          theme(legend.position="none")
      ) %>% #layout(height = 600) %>%
        config(displayModeBar = F)
      
      return(fig1)
    }
    
  })
  
  #gis lab hourly traffic
  output$gis_lab_hourly <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(Sortie_data_gis())){
      return(NULL)
    }
    
    #read in the user data
    gis_lab <- Sortie_data_gis() #return Sortie function
    
    #aggregate hourly traffic counts
    hourly <- gis_lab %>% group_by(hour) %>%
      count(hour)
    hourly <- hourly[hourly$hour != "",]
    
    #convert to factor for custom ordering
    hourly$hour <- factor(hourly$hour, levels = c("8:00 AM",
                                                  "9:00 AM",
                                                  "10:00 AM",
                                                  "11:00 AM",
                                                  "12:00 PM",
                                                  "1:00 PM",
                                                  "2:00 PM",
                                                  "3:00 PM",
                                                  "4:00 PM",
                                                  "5:00 PM",
                                                  "6:00 PM",
                                                  "7:00 PM",
                                                  "8:00 PM",
                                                  "9:00 PM",
                                                  "10:00 PM",
                                                  "11:00 PM"))
    
    
    #plot
    fig <- ggplotly(
      
      hourly %>%
        ggplot(aes(x = hour, y = n, fill = hour)) +
        geom_bar(stat = 'identity') +
        theme_bw() +
        ggtitle("Hourly Lab Traffic") +
        labs(y = "Visitors", x = NULL) +
        theme(legend.position = NULL,
              axis.text.x = element_text(angle = 45))
    )%>%
      config(displaylogo = FALSE) %>%
      layout(showlegend = FALSE) %>% 
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                        "lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig)
    
  })

  
  #----- FILE DOWNLOADS --------------------------------------------------
  
  output$downloadConsults <-downloadHandler(
    filename = function(){
      paste('consults',input$quarter,input$year,'.csv', sep = '')
    },
    content = function(file){
      write.csv(Sortie_consults(),file,row.names = FALSE)
    }
  )
  
  output$downloadInstruction <-downloadHandler(
    #filename, can be a function! 
    filename = function(){
      paste('instruction',input$quarter,input$year,'.csv', sep = '')
    },
    content = function(file){
      write.csv(Sortie_instruction(),file,row.names = FALSE)
    }
  )
  
  output$downloadOutreach <-downloadHandler(
    filename = function(){
      paste('outreach',input$quarter,input$year,'.csv', sep = '')
    },
    content = function(file){
      write.csv(Sortie_outreach(),file,row.names = FALSE)
    }
  )
  
  output$downloadInfo_RAD <- downloadHandler(
    filename = function(){
      paste('Info_RAD',input$quarter,input$year,'.csv', sep = '')
    },
    content = function(file){
      write.csv(Sortie_info_RAD(),file,row.names = FALSE)
    }
  )
  
  output$downloadDataGISLab <-downloadHandler(
    filename = function(){
      paste('Data_GIS_Lab',input$quarter,input$year,'.csv', sep = '')
    },
    content = function(file){
      write.csv(Sortie_data_gis(),file,row.names = FALSE)
    }
  )
  
})
#----- END SERVER ----------------------------------------------------
