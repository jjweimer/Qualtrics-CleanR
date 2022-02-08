library(shiny)
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)
library(DT) #for better tables

#max file size 30mb for upload
options(shiny.maxRequestSize = 30*1024^2)

#UI
shinyUI(fixedPage(theme = shinytheme("yeti"),
                  
    #navbar
    navbarPage("Qualtrics Service Stats Cleaner"),
                  
    # Alternative way to render title
    #titlePanel("Qualtrics Data Cleaner"),

    # Sidebar
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        selectInput("quarter", "Choose A Quarter:", #this is our selector
                    c("All" = "All",
                      "Winter" = "WI",
                      "Spring" = "SP",
                      "Summer" = "SU",
                      "Fall" = "FA",
                      "Break" = "Break")),
        selectInput("year", "Choose A Year:", #this is our selector
                    c("All" = "All",
                      "2022" = "2022",
                      "2021" = "2021",
                      "2020" = "2020",
                      "2019" = "2019",
                      "2018" = "2018")),
        tags$hr(),
        h3("Data Export"),
        downloadButton("downloadConsults", "Download Consults"),
        helpText("Clean Consults data"),
        downloadButton("downloadInstruction", "Download Instruction"),
        helpText("Clean Instruction data"),
        downloadButton("downloadOutreach", "Download Outreach"),
        helpText("Clean Outreach data"),
        downloadButton("downloadInfo_RAD", "Download Info/RAD"),
        helpText("Clean Info / RAD data"),
        #downloadDataGISLab
        downloadButton("downloadDataGISLab", "Download Data & GIS"),
        helpText("Clean Data & GIS Lab Data")
        
      ), #end sidebar
      
      #Main Panel
      mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel("Welcome",
                             #HTML('<center><img src="the_library.png"></center>'), #render an image with html formatting
                             h2("Welcome to the Qualtrics Service Stats Cleaner!"),
                             h4("To get started, upload a Qualtrics .csv export using the \"Choose a CSV file\" button. 
                                       Summary statistics, plots and tables will be generated for
                                       you automatically. Use the above tab panels to naviagte the app, and to view the 
                                       plots and statistics for each category."),
                             h4("To subset your data by quarter or year 
                                       (or both) you can use the \"Choose a Quarter\" and 
                                       \"Choose a Year\" boxes. These subsets take effect for all 
                                       plots, stats, and data exports."),
                             h4("The download buttons can be used to easily export
                                       cleaned versions of your uploaded service stats. These data
                                       exports are reactive to the selected quarter/year, so you can
                                       filter as you wish, or set both options to \'All\' for all 
                                       observations"),
                             h4("Tip: Plots are interactive! Hover your mouse over observations
                                       to learn more about the data. You can also use the toolbars in 
                                       the top-right corner of each plot to download the plot, reset the 
                                      axes, and compare data on hover."),
                             helpText("Developed by Joshua Weimer for the UCSD Data & GIS Lab. Please contact j2weimer@ucsd.edu or slabou@ucsd.edu 
                                      with any questions/concerns.")
                             ),
                    
                    tabPanel("Consults",
                             #tags$hr(),
                             h3("Consults Statistics"),
                             textOutput("consults_stats"),
                             tags$hr(),
                             plotlyOutput("intra_quarter_consults"),
                             selectInput("consults_position", "Position:",
                                         c("Stacked" = "stack",
                                           "Dodged" = "dodge")
                             ),
                             tags$hr(),
                             plotlyOutput("consults_over_time"),
                             selectInput("consults_scale", "Aggregation Scale:",
                                         c("Weekly" = "Weekly",
                                           "Daily" = "Daily")
                             ),
                             tags$hr(),
                             plotlyOutput("consults_graph"),
                             numericInput("n","Minimum Count of Department:"
                                          ,100,min = 1),
                             selectInput("is_fuzzy", "Select Matched or Raw Departments:", 
                                         c("Matched" = "Matched",
                                           "Raw" = "Raw")
                             ),
                             tags$hr(),
                             #this plot is shy ??
                             #plotlyOutput("consult_locations"),
                             #tags$hr(),
                             plotlyOutput("consult_categories"),
                             numericInput("n_category","Minimum Count of Category:"
                                          ,100,min = 1),
                             tags$hr(),
                             DT::dataTableOutput("consults_DT")
                             ),
                    tabPanel("Instruction",
                             #tags$hr(),
                             h3("Instruction Statistics"),
                             textOutput("instruction_stats"),
                             tags$hr(),
                             plotlyOutput("intra_quarter_instruction"),
                             selectInput("instruction_position", "Position:",
                                         c("Stacked" = "stack",
                                           "Dodged" = "dodge")
                             ),
                             tags$hr(),
                             plotlyOutput("instruction_time_plot"),
                             selectInput("instruction_scale", "Aggregation Scale:", 
                                         c("Weekly" = "Weekly",
                                           "Daily" = "Daily")
                             ),
                             tags$hr(),
                             DT::dataTableOutput("instruction_DT")
                             ),
                    tabPanel("Outreach",
                             #tags$hr(),
                             h3("Outreach Statistics"),
                             textOutput("outreach_stats"),
                             tags$hr(),
                             DT::dataTableOutput("outreach_DT"),
                             ),
                    tabPanel("Info / RAD",
                             #tags$hr(),
                             h3("RAD / Info Desk Statistics"),
                             textOutput("info_stats"),
                             tags$hr(),
                             plotlyOutput("intra_quarter_RAD"),
                             selectInput("info_position", "Position:",
                                         c("Stacked" = "stack",
                                           "Dodged" = "dodge")
                             ),
                             tags$hr(),
                             plotlyOutput("info_time_plot"),
                             selectInput("info_scale", "Aggregation Scale:", 
                                         c("Weekly" = "Weekly",
                                           "Daily" = "Daily")
                             ),
                             tags$hr(),
                             DT::dataTableOutput("serv_counts_DT"),
                             tags$hr(),
                             DT::dataTableOutput("info_DT")
                             ),
                    tabPanel("Data & GIS Lab",
                             #tags$hr(),
                             h3("Data & GIS Lab Statistics"),
                             textOutput("gis_stats"),
                             tags$hr(),
                             plotlyOutput("week_of_quarter_gis_lab"),
                             selectInput("gis_position", "Position:",
                                         c("Stacked" = "stack",
                                           "Dodged" = "dodge")
                             ),
                             tags$hr(),
                             plotlyOutput("gis_lab_per_week"),
                             tags$hr(),
                             plotlyOutput("gis_lab_departments"),
                             numericInput("n_gis","Minimum Count of Department:"
                                          ,20,min = 1),
                             selectInput("is_fuzzy_gis", "Select Matched or Raw Departments:", 
                                         c("Matched" = "Matched",
                                           "Raw" = "Raw")
                             ),
                             tags$hr(),
                             DT::dataTableOutput("data_gis_DT"))
                    )
      ) #end main panel   
    
))
