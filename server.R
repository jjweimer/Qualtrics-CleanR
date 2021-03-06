library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT) #for better tables
library(stringdist) #fuzzy matching
library(rintrojs) #js library for intro
library(shinycssloaders)
library(bslib)
#library(thematic)

#require fuzzy match, helper functions
source('functions/fuzzy_match.R', local = TRUE)
source("functions/week_quarter_helper.R", local = TRUE)
source("functions/filter_Sortie.R")
source("functions/dataprep_gis_lab_hourly.R")
source("functions/dataprep_gis_lab_departments.R")
source("functions/clean_desk_service.R")
source("functions/count_comm_act.R")
source("ggtheme/my_ggtheme.R")

# Define server logic 
shinyServer(function(input, output,session) {
  
  #----- TUTORIAL ----------------------------------------------------
  
  observeEvent(input$help,
               introjs(session, 
                       options = list("nextLabel" = "Next",
                                      "prevLabel" = "Previous",
                                      "skipLabel" = ""),
                       events = list("oncomplete" = I('alert("Tutorial Complete!")'))
                       ))

  #----- DATA WRANGLE/CLEANING -------------------------------------------

  #read user inputted data set once
  Sortie_master <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }
    #read in the user data
    user_data <- read.csv(inFile$datapath)
    return(user_data)
  })
  
  #clean instruction data, return data frame
  Sortie_instruction <- reactive({
    if (is.null(Sortie_master()))
      return(NULL)
    #else clean the data
    instruction_names <- #these will be new colnames
      c("entered_by","service","date","format","activity", "home_program", 
        "co_instructors","num_attendants","n_sessions")
    instruction <- 
      Sortie_master() %>%
      select(Q2, Q3, Q16, Q192, Q17, Q14, Q15, Q21, Q191) %>%
      filter(Q3 == "Instruction") %>% 
      rename(!!!setNames(names(.), instruction_names)) %>% 
      mutate(
        num_attendants = as.numeric(num_attendants),
        date = mdy(date),
        week = isoweek(date),
        month = month(date),
        year = format(date, format =  '%Y'),
        n_sessions = as.numeric(n_sessions)
        #sessions_in_person = as.numeric(sessions_in_person),
        #sessions_online = as.numeric(sessions_online)
      ) %>% relocate(week, .after = date) %>%
      clean_years() %>%
      week_to_quarter() %>%
      week_of_quarter() %>%
      month_day() %>% 
      #rowwise() %>% #to allow for summing by row
      #mutate(sessions_total = sum(sessions_in_person, sessions_online, na.rm = T)) %>%
      #ungroup() %>% #ungroup by rows
      #relocate(sessions_total, .after = sessions_online) %>%
      filter_Sortie(qtr = input$quarter, yr = input$year) #filter to selected year/quarter
    return(instruction)
  })
  
  #clean outreach data, return data frame
  Sortie_outreach <- reactive({
    if (is.null(Sortie_master()))
      return(NULL)
    outreach_names <- 
      c("entered_by","service","date","type","home_program","collaborators",
        "attendees","status","duration","outcome1","outcome2")
    outreach <- Sortie_master() %>%
      select(Q2,Q3, Q156, Q198, Q174, Q170 ,Q184,Q194, Q202, Q180, Q180_5_TEXT) %>%
      filter(Q3 == "Outreach") %>%
      rename(!!!setNames(names(.), outreach_names)) %>%
      filter(!is.na(date)) %>%#empty obs checking
      filter(date != "") %>% #empty obs checking
      mutate(
        date = mdy(date),
        attendees = as.numeric(attendees),
        week = isoweek(date),
        year = format(date,format =  '%Y')
      ) %>% 
      relocate(week, .after = date) %>%
      clean_years() %>%
      week_to_quarter() %>%
      week_of_quarter() %>%
      month_day()%>%
      filter_Sortie(qtr = input$quarter, yr = input$year)
    return(outreach)
  })
  
  #clean consults data, return data frame
  Sortie_consults <- reactive({
    if (is.null(Sortie_master()))
      return(NULL)
    #else clean data
    cons_names <- 
      c("entered_by","service","date","location","department", "num_consult",
        "category","time_spent","status","comm")
    consults <- Sortie_master() %>%
      select(Q2,Q3,Q38,Q39,Q40,Q42,Q43,Q44,Q45,Q92) %>%
      filter(Q3 == "Consultation") %>%
      rename(!!!setNames(names(.), cons_names)) %>%
      mutate(
        date = mdy(date),
        week = isoweek(date),
        month = month(date),
        year = format(date, format =  '%Y'),
        num_consult = case_when(
          is.na(num_consult) ~ 1,
          TRUE ~ as.numeric(num_consult)
        ),
        time_spent = case_when(
          time_spent == "greater than 10" ~ 10,
          TRUE ~ as.numeric(time_spent)
        ),
        fuzzy_department = fuzzy_match(department)
      ) %>% clean_years() %>%
      week_to_quarter() %>%
      week_of_quarter() %>%
      month_day() %>%
      relocate(fuzzy_department, .after = department) %>% 
      relocate(week, .before = location) %>% 
      #filter before doing counts!!!!!
      filter_Sortie(qtr = input$quarter, yr = input$year) %>%
      group_by(department) %>%
      mutate(total_dept_consult_count = n()) %>%
      ungroup() %>%
      group_by(fuzzy_department) %>%
      mutate(total_fuzzy_dept_count = n()) %>%
      ungroup() 
    return(consults)
  })
  
  Sortie_info_RAD <- reactive({
    if (is.null(Sortie_master())){
      return(NULL)
    }
    #now clean for info/RAD data
    info <- Sortie_master() %>%
      select(RecordedDate,Q2,Q26,Q27,Q31) %>%
      filter(Q2 %in% c("RAD","Info Desk")) %>%
      mutate(
        service = paste(Q27,Q31, sep = ""),
        date_time = as.POSIXct(RecordedDate, format = "%Y-%m-%d %H:%M:%S"),
        year =  format(date_time,format =  '%Y'),
        week = isoweek(date_time),
        desk = Q2,
      ) %>% 
      select(year,week,date_time,desk,service) %>%
      week_to_quarter() %>%
      week_of_quarter() %>%
      month_day() %>%
      mutate(
        month = month(date_time),
        service = clean_desk_service(service)
      ) %>%
      filter_Sortie(qtr = input$quarter, yr = input$year)
    return(info)
  })
  
  Sortie_data_gis <- reactive({
    if (is.null(Sortie_master()))
      return(NULL)
    
    #select the columns/rows we need
    gis_lab <- Sortie_master() %>%
      select(RecordedDate,Q2,Q49,Q50,Q51,Q52,Q53,Q89,Q90) %>%
      filter(Q2 == "Data/GIS Lab")
    #check that there are nonzero number of rows
    if(nrow(gis_lab) == 0){
      return(NULL)
    }
    #colnames
    colnames(gis_lab) <- 
      c("RecordedDate","location","entry_type","user_status","department",
        "visit_purpose","question_type","date","hour")
    #convert RecordedDate to useful date-times
    gis_lab$RecordedDate <- 
      as.POSIXct(gis_lab$RecordedDate,format = "%Y-%m-%d %H:%M:%S")
    
    #fill in missing "date" values with recordeddate
    gis_lab$date[is.na(gis_lab$date) | gis_lab$date == ""] <- 
      format(gis_lab$RecordedDate[is.na(gis_lab$date) | gis_lab$date == ""], format = "%m/%d/%Y")
    #convert to date class
    gis_lab$date <- mdy(gis_lab$date)
    #extract year and week
    gis_lab$year <-  format(gis_lab$date,format =  '%Y')
    gis_lab$week <- isoweek(gis_lab$date)
    #more date stuff
    gis_lab <- gis_lab %>%
      week_to_quarter() %>%
      week_of_quarter() %>%
      month_day()
    
    ## fuzzy match department names
    gis_lab$fuzzy_department <- fuzzy_match(gis_lab$department)
    gis_lab <- gis_lab %>% relocate(fuzzy_department, .after = department)
    #filter to selected quarter
    gis_lab <- filter_Sortie(df = gis_lab, qtr = input$quarter, yr = input$year)
    return(gis_lab)
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
    num_hours <- round(sum(consults$time_spent[!is.na(consults$time_spent)]), -1)#round to nearest 10
    num_consults <- nrow(consults)
    num_people_consulted <- sum(consults$num_consult[!is.na(consults$num_consult)])
    num_departments <- length(unique(consults$fuzzy_department))
    return(paste("There were",num_consults, "consults reaching",
                 num_people_consulted, "people in", 
                 num_departments, "unique departments. There 
                 were approximately",num_hours,"hours spent on consultations."))
  })
  
  output$info_stats <- renderText({
    
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
      dept_counts <- dept_counts[1:n_department,]
      #plot
      fig <- ggplotly(
        dept_counts %>%
          ggplot(aes(x = reorder(fuzzy_department,n), y = n)) +
          geom_col(alpha = 1, fill = "#00629B") +
          #geom_text(aes(label = n), hjust = -1) +
          coord_flip() +
          ggtitle("Most Consulted Departments") +
          my_ggtheme + #custom theme
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
      dept_counts <- dept_counts[1:n_department,]
      #plot
      fig1 <- ggplotly(
        dept_counts %>%
          ggplot(aes(x = reorder(department,n), y = n)) +
          geom_col(alpha = 1, fill = "#00629B") +
          #geom_text(aes(label = n), hjust = -1) +
          coord_flip() +
          ggtitle("Most Consulted Departments") +
          my_ggtheme + #custom theme
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
      weekly_data <- consults %>% group_by(week,year) %>%count(week)
      #plot
      fig1 <- ggplotly(
        weekly_data[!is.na(weekly_data$year),] %>% 
          ggplot(aes(x = week, y = n, fill = year)) +
          geom_bar(stat='identity') +
          ggtitle("Weekly Consults") +
          labs(x = NULL, y = "Number of Consults") +
          my_ggtheme + #custom theme
          scale_x_continuous(breaks = month_numeric, labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig1)
      
    }else if(user_consults_scale == "Monthly"){
      
      #aggregate 
      monthly_data <- consults %>% group_by(month, year) %>% count(month)
      #make sure each date occurs at least once so axes work well
      #monthly_data <- all_daily_dates(daily_data)
      #plot
      fig2 <- ggplotly(
        monthly_data[!is.na(monthly_data$year),] %>% 
          ggplot(aes(x = month, y = n, fill = year)) +
          geom_bar(stat= 'identity') +
          my_ggtheme +
          labs(x = NULL, y = "Number of Consults") +
          ggtitle("Monthly Consults") +
          scale_x_continuous(breaks = c(1:12), labels = month_label)
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
        my_ggtheme + #custom theme
        scale_x_continuous(breaks = c(0:11))
    ) %>%
      config(displayModeBar = FALSE) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>% 
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
              my_ggtheme + #custom theme
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
      ggplot(aes(x = reorder(category,n), y = n)) +
      geom_col(fill = "#00629B") +
      coord_flip() +
      ggtitle("Consult Categories") +
        my_ggtheme + #custom theme
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
        my_ggtheme + #custom theme
        scale_x_continuous(breaks = c(0:11))
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
          my_ggtheme + #custom theme
          scale_x_continuous(breaks = month_numeric, labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                          "zoom2d","lasso2d",
                                          "pan2d","autoscale2d",
                                          "select2d"))
      
      return(fig1)
      
    } else if(user_instruction_scale == "Monthly"){
      
      #aggregate
      monthly_data <- instruction %>% 
        group_by(month, year) %>%
        count(month)
      
      #make sure each date occurs at lesat once so axes work well
      #daily_data <- all_daily_dates(daily_data)
      
      #plot
      fig2 <- ggplotly(
        monthly_data %>% 
          ggplot(aes(x = month, y = n, fill = year)) +
          geom_bar(stat= 'identity') +
          my_ggtheme + #custom theme
          ggtitle("Monthly Instruction Events")+
          labs(x = NULL, y = "Number of Instruction Events") +
          scale_x_continuous(breaks = c(1:12), labels = month_label)
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
    
    if(is.null(Sortie_instruction())){
      return(NULL)
    }
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
        my_ggtheme + #custom theme
        scale_x_continuous(breaks = month_numeric, labels = month_label)
      
    )%>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "zoom2d","lasso2d",
                                        "pan2d","autoscale2d",
                                        "select2d"))
    return(fig)
    
  })
  
  #consult communication category
  output$consult_comm_category <- renderPlotly({
    if(is.null(Sortie_master())){
      return(NULL)
    } else {
      df <- Sortie_master() #for some reason using master works but consults doesn't ?? not sure why
      df <- count_comm_act(df$Q92)
      #plot
      fig <- df %>%
        ggplot(aes(x = n, y = reorder(comm_act,n))) +
        geom_col( fill = "#00629B") +
        my_ggtheme +
        labs(y = "Communication Activity", x = "n") +
        ggtitle("Scholarly Communication Activity Counts") 
      fig <- ggplotly(fig, tooltip = "n") %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis=list(fixedrange=TRUE)) %>%
        layout(yaxis=list(fixedrange=TRUE)) %>% 
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d",
                                          "lasso2d","pan2d","autoscale2d","select2d"))
      return(fig)  
    }
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
    title <- paste("Services per week of the Quarter (", q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data[!is.na(weekly_data$year),] %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity", position = input$info_position) +
        ggtitle(title) +
        xlim(0,11) +
        labs(y = "Number of Services", x = "Week") +
        my_ggtheme + #custom theme
        scale_x_continuous(breaks = c(0:11))
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
          my_ggtheme + #custom theme
          scale_x_continuous(breaks = month_numeric, labels = month_label)
      ) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                          "zoom2d","lasso2d",
                                          "pan2d","autoscale2d","select2d"))
      
      return(fig1)
      
    } else if (user_info_scale == "Monthly"){
      
      #aggregate 
      monthly_data <- info %>% 
        group_by(month, year) %>%
        count(month)
      
      #make sure each date occurs at least once so axes work well
      #monthly_data <- all_daily_dates(daily_data)
      
      #plot
      #plot
      fig2 <- ggplotly(
        monthly_data %>% 
          ggplot(aes(x = month, y = n, fill = year)) +
          geom_bar(stat= 'identity') +
          my_ggtheme + #custom theme
          labs(x = NULL, y = "Number of Services") +
          ggtitle("Monthly RAD/info Desk Services") +
          scale_x_continuous(breaks = c(1:12), labels = month_label)
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
    weekly_data <- Sortie_data_gis() %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    #plot
    fig <- ggplotly(
      weekly_data %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity",position = input$gis_position) +
        ggtitle("Lab Traffic by Week of Quarter") +
        xlim(0,11) +
        labs(y = "Number of Lab Visits", x = "Week") +
        my_ggtheme + #custom theme
        scale_x_continuous(breaks = c(0:11))
    ) %>% config(displaylogo = FALSE) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>% 
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d","lasso2d",
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
        my_ggtheme + #custom theme
        scale_x_continuous(breaks = month_numeric, labels = month_label)
    ) %>%
      config(displaylogo = FALSE) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>% 
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
    #dataprep
    dept_counts <- dataprep_gis_lab_departments(
      df = Sortie_data_gis(), 
      is_fuzzy = input$is_fuzzy_gis,
      n_gis = input$n_gis)
    #plot
    fig <- dept_counts %>%
      ggplot(aes(x = reorder(department,n), y = n)) +
      geom_col(alpha = 1, fill = "#00629B") +
      coord_flip() +
      ggtitle("Most Frequent Departments") +
      my_ggtheme + #custom theme
      labs(x = NULL, y = "count") +
      theme(legend.position="none")
    #plotly styling
    fig <- ggplotly(fig,tooltip = c("n")) %>%
      config(displaylogo = FALSE) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>% 
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d", "lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    return(fig)
  })
  
  #gis lab hourly traffic
  output$gis_lab_hourly <- renderPlotly({
    
    if(is.null(Sortie_data_gis())){
      return(NULL)
    }
    hourly <- dataprep_gis_lab_hourly(Sortie_data_gis())
    #plot
    fig <- ggplotly(
      hourly %>%
        ggplot(aes(x = hour, y = n)) +
        geom_bar(stat = 'identity', fill = "#00629B") +
        my_ggtheme + #custom theme
        ggtitle("Hourly Lab Traffic") +
        labs(y = "Visitors", x = NULL) +
        theme(legend.position = NULL,
              axis.text.x = element_text(angle = 45))
    )%>%
      config(displaylogo = FALSE) %>%
      layout(showlegend = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>% 
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
