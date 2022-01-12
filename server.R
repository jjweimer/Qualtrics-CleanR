library(shiny)
library(lubridate)
library(dplyr)

# Define server logic 
shinyServer(function(input, output) {

  
  #generate instrcution data frame
  output$instruction_data <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
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
    
    #returns last line of output
    instruction <- instruction
    
  })
  
  #generate outreach data frame
  output$outreach_data <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #read in the user data
    user_data <- read.csv(inFile$datapath, header = input$header)
    
    #now clean data for outreach_data
    outreach <- user_data %>% select(Q3, Q156, Q198, Q174, Q184, Q194, Q202, Q196)
    outreach <- outreach[outreach$Q3 == "Outreach",]
    colnames(outreach) <- c("service","date","type","home_program", "attendees",
                            "status","duration","time_prep")
    #ensure date data type
    #outreach$date <- mdy(outreach$date)
    #make attendees numeric
    outreach$attendees <- as.numeric(outreach$attendees)
    
    #returns last line of output
    outreach <- outreach
  })
  
  
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header)
  })

})
