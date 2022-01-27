library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT) #for better tables

#max file size 30mb for upload
options(shiny.maxRequestSize = 30*1024^2)

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
    instruction <- user_data %>% select(Q2,Q3, Q16, Q192.1, Q17, Q21)
    instruction <- instruction[instruction$Q3 == "Instruction",]
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
    user_data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    
    #now clean data for outreach_data
    outreach <- user_data %>% select(Q2,Q3, Q156, Q198, Q174, Q182, Q184, 
                                     Q194, Q202, Q196)
    outreach <- outreach[outreach$Q3 == "Outreach",]
    
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
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath)
    
    #now clean data for consults_data
    consults <- user_data %>% select(Q2,Q3,Q38,Q39,Q40,Q42,Q43,Q44,Q45)
    consults <- consults[consults$Q3 == 'Consultation',]
    colnames(consults) <- c("entered_by","service","date","location",
                            "department", "num_consult","category",
                            "time_spent","status")
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
    
    #clean years
    consults <- clean_years(consults)
    
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
  
  
  Sortie_info_RAD <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath)
    
    #now clean for info/RAD data
    info <- user_data[user_data$Q2 %in% c("RAD","Info Desk"),
                      c("RecordedDate","Q2","Q26","Q27","Q31")]
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
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath)
    
    #select the columns/rows we need
    gis_lab <- user_data[user_data$Q2 == "Data/GIS Lab",]
    gis_lab <- gis_lab %>% select(c("RecordedDate","Q2","Q49","Q50","Q51",
                                    "Q52","Q53"))
    #colnames
    colnames(gis_lab) <- c("RecordedDate","location","entry_type","user_status",
                           "department","visit_purpose","question_type")
    
    #convert RecordedDate to useful date-times
    #date time conversion
    gis_lab$date_time <- as.POSIXct(gis_lab$RecordedDate, 
                                 format = "%m/%d/%Y %H:%M", 
                                 tz = "America/Los_Angeles")
    
    #extract year and week
    gis_lab$year <-  format(gis_lab$date_time,format =  '%Y')
    gis_lab$week <- isoweek(gis_lab$date_time)
    
    #week of quarter and quarter
    gis_lab <- week_to_quarter(gis_lab)
    gis_lab <- week_of_quarter(gis_lab)
    
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
  
  #get quarter input
  get_quarter <- reactive({
    return(input$quarter)
  })
  
  #return n
  return_n <- reactive({
    return(input$n)
  })
  
  
  ###################################################################
  ### Render Tables  ############
  ###################################################################
  
  #instruction DT table
  #
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
    if(is.null(input$file1)){
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
    if(is.null(input$file1)){
      return(NULL)
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
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    #now the text render part
    #how many consults in what date range?
    #date_min <- as.character(min(consults$date[!is.na(consults$date)]))
    #date_max <- as.character(max(consults$date[!is.na(consults$date)]))
    num_consults <- nrow(consults)
    num_people_consulted <- sum(consults$num_consult[
                                !is.na(consults$num_consult)])
    num_departments <- length(unique(consults$department))
    
    return(paste("There were",num_consults, "consults reaching",
                   num_people_consulted, "people in", 
                   num_departments, "unique departments"))
    
  })
  
  output$info_stats <- renderText({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
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
    if(is.null(input$file1)){
      return(NULL)
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
    title <- paste("Most Consulted Departments (n >= ", 
                   n,")" , sep = '')
    
    fig <- ggplotly(
      consults %>% group_by(department) %>%
      count(department) %>%
      ggplot(aes(x = reorder(department,n), y = n, fill = n)) +
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
    
    weekly_data <- consults %>% group_by(week,year) %>%
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
    
  })
  
  #intra-quarter weekly consults
  output$intra_quarter_consults <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    consults <- Sortie_consults() #return Sortie function
    
    weekly_data <- consults %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- get_quarter()
    
    title <- paste("Consults per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity") +
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
    if(is.null(input$file1)){
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
  
  
  ## instruction by week of quarter
  
  output$intra_quarter_instruction <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    instruction <- Sortie_instruction() #return Sortie function
    
    weekly_data <- instruction %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- get_quarter()
    
    title <- paste("Instruction Events per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity") +
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
  
  
  
  ## instruction over time
  output$instruction_time_plot <- renderPlotly({
  
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in df
    instruction <- Sortie_instruction()
    
    weekly_data <- instruction %>% group_by(week,year) %>%
      count(week)
    
    #make daily events count
    #instruction <- instruction %>% group_by(date) %>%
    #  mutate(daily_instruction_events = n())
    
    #make daily people count
    #instruction <- instruction %>% group_by(date) %>%
    #  mutate(daily_people_instructed = sum(num_attendants)) 
    
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
        ggtitle("Weekly Instruction Events") +
        labs(x = NULL, y = "Number of Instruction Events") +
        theme_bw() +
        scale_x_continuous(breaks = month_numeric, 
                           labels = month_label)
    ) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                        "zoom2d","lasso2d",
                                        "pan2d","autoscale2d","select2d"))
    
    return(fig1)
  })
  
  
  #info/RAD services over time
  
  output$info_time_plot <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in df
    info <- Sortie_info_RAD()
    
    weekly_data <- info %>% group_by(week,year) %>%
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
    
  })
  
  
  #week of quarter data and GIS lab use
  output$week_of_quarter_gis_lab <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
      return(NULL)
    }
    
    #read in the user data
    gis_lab <- Sortie_data_gis() #return Sortie function
    
    weekly_data <- gis_lab %>% 
      group_by(week_of_quarter, year) %>%
      count(week_of_quarter)
    
    #get the selected quarter
    q <- get_quarter()
    
    title <- paste("Data & GIS Lab visits per week of the Quarter (",
                   q," Quarter)", sep = '')
    
    fig <- ggplotly(
      weekly_data %>% 
        ggplot(aes(x = week_of_quarter, y = n, fill = year)) +
        geom_bar(stat = "identity") +
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
  #num consults per week over time
  output$gis_lab_per_week <- renderPlotly({
    
    #return null if no file yet, avoids ugly error code
    if(is.null(input$file1)){
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
  
  
  
  #################################################
  ## FILE DOWNLOADS   #############################
  ################################################
  
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
#####################################################################
############# END SERVER   ##########################################
####################################################################
########## HELPER FUNCTIONS ########################################
###################################################################

## Helper function for determining quarters by year
week_to_quarter <- function(df){
  
  #use isoweek(mdy("12/14/19")) (month/day/year) (needs lubridate)
  # while browsing academic calendar to quickly get week cutoffs
  
  #init empty quarter column
  df$quarter <- NA
  
  #for 2018
  df$quarter[df$week >=2 & df$week <= 12 &
               df$year == 2018] <- "WI"
  df$quarter[df$week >=14 & df$week <= 24 & 
               df$year == 2018] <- "SP"
  df$quarter[df$week >=27 & df$week <= 36 &
               df$year == 2018] <- "SU"
  df$quarter[df$week >= 39 & df$week <= 50 &
               df$year == 2018] <- "FA"
  
  
  
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
  df$week_of_quarter <- NA
  
  #for 2019
  df$week_of_quarter[df$quarter == "WI" & df$year == 2018] <- 
    df$week[df$quarter == "WI" & df$year == 2018] - 1
  df$week_of_quarter[df$quarter == "SP" & df$year == 2018] <- 
    df$week[df$quarter == "SP" & df$year == 2018] - 13
  df$week_of_quarter[df$quarter == "SU" & df$year == 2018] <- 
    df$week[df$quarter == "SU" & df$year == 2018] - 26
  df$week_of_quarter[df$quarter == "FA" & df$year == 2018] <- 
    df$week[df$quarter == "FA" & df$year == 2018] - 39 #we want a 0 week here
  
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


clean_years <- function(df){
  
  #drop rows from before 2018 for now
  df <- df[df$year >= 2018,]
  
  df$year[df$year == 2091] <- 2019
  df$year[df$year == 2109] <- 2019
  df$year[df$year == 2921] <- 2021
  
  return(df)
}
