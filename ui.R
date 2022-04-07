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

#max file size 30mb for upload
options(shiny.maxRequestSize = 30*1024^2)
#spinner options from shinycssloaders
options(spinner.type = 3,
        spinner.color.background  = "#ffffff",
        spinner.color = "#00629B")

#UI
shinyUI(fixedPage(
  introjsUI(),
  
  #load custom css from file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),
  
  #bslib theming
  #SEE https://brand.ucsd.edu/using-the-brand/web/index.html for brand guidelines
  theme = bslib::bs_theme(
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#00629B",
    secondary = "#FFCD00",
    base_font = font_google("Roboto")
  ),
  
  #header
  fluidRow(
    column(
      12,
      align = "center",
      introBox(
        h1("Service Statistics Personal Insights Tool"),
        data.step = 1,
        data.intro = 
        "This is an app for cleaning the 
        Qualtrics Service Stats Export and returning 
        personal insights."
      ),
    ) 
  ),
  
  #top bar with file upload
  fluidRow(
    column(
      4,
      offset= 4,
      align = "center",
      introBox(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        data.step = 2,
        data.intro = "Upload your Qualtrics .csv export here."
      )
    )
  ),
  
  #filters and tutorial button row
  fluidRow(
    column(
      2,
      offset = 3,
      introBox(
        selectInput("quarter", "Quarter:", #this is our selector
                    c("All" = "All",
                      "Winter" = "WI",
                      "Spring" = "SP",
                      "Summer" = "SU",
                      "Fall" = "FA",
                      "Break" = "Break")),
        #introbox args
        data.step = 3,
        data.intro = "Use these dropdowns to filter data by quarter or year."
      )
    ),
    column(
      2,
      selectInput("year", "Year:", #this is our selector
                  c("All" = "All",
                    "2022" = "2022",
                    "2021" = "2021",
                    "2020" = "2020",
                    "2019" = "2019",
                    "2018" = "2018"))
    ),
    column(
      4,
      style = "margin-top: 30px;",
      actionButton("help","Press here for Tutorial")
    )
  ),
  tags$hr(),
  
  #tabset panels
  fluidRow(
    column(
      12,
      tabsetPanel(type='pills',
                  tabPanel("Consults",
                           tags$hr(),
                           fluidRow(
                             column(
                               7,
                               h2("Consults Statistics")
                             ),
                             column(
                               5,
                               style = "margin-top:20px",
                               align = "right",
                               downloadButton("downloadConsults", "Download Consults")
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                               textOutput("consults_stats"),
                               tags$hr()
                             )
                           ),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("intra_quarter_consults") %>% withSpinner()
                             ),
                             column(
                               2,
                               selectInput("consults_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge")),
                             )
                           ),
                           tags$hr(),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("consults_over_time") %>% withSpinner()
                             ),
                             column(
                               2,
                               selectInput("consults_scale", "Aggregation Scale:",
                                           c("Weekly" = "Weekly",
                                             "Daily" = "Daily")),
                             )
                           ),
                           tags$hr(),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("consults_graph") %>% withSpinner()
                             ),
                             column(
                               2,
                               numericInput("n","Top n Departments:",10,min = 1),
                               selectInput("is_fuzzy", "Departments:", 
                                           c("Matched" = "Matched",
                                             "Raw" = "Raw")),
                             )
                           ),
                           tags$hr(),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("consult_categories") %>% withSpinner()
                             ),
                             column(
                               2,
                               numericInput("n_category","Top n Categories:",10,min = 1)
                             )
                           ),
                           tags$hr(),
                           DT::dataTableOutput("consults_DT") %>% withSpinner()
                  ),
                  tabPanel("Instruction",
                           tags$hr(),
                           fluidRow(
                             column(
                               7,
                               h2("Instruction Statistics")
                             ),
                             column(
                               5,
                               style = "margin-top:20px",
                               align = "right",
                               downloadButton("downloadInstruction", "Download Instruction")
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                               textOutput("instruction_stats"),
                               tags$hr()
                             )
                           ),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("intra_quarter_instruction") %>% withSpinner()
                               ),
                             column(
                               2,
                               selectInput("instruction_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge"))
                             )
                           ),
                           tags$hr(),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("instruction_time_plot") %>% withSpinner()
                             ),
                             column(
                               2,
                               selectInput("instruction_scale", "Aggregation Scale:", 
                                           c("Weekly" = "Weekly",
                                             "Daily" = "Daily")),
                               selectInput("instruction_format_selector_2", "Format:",
                                           c("All Formats" = "All",
                                             "Online" = "Online only",
                                             "In Person" = "In-person only",
                                             "Hybrid" = "Hybrid (partially online, partially in-person)")),
                             )
                           ),
                           tags$hr(),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("instruction_num_people_time") %>% withSpinner()
                             ),
                             column(
                               2,
                               selectInput("instruction_format_selector", "Format:",
                                           c("All Formats" = "All",
                                             "Online" = "Online only",
                                             "In Person" = "In-person only",
                                             "Hybrid" = "Hybrid (partially online, partially in-person)")),
                             )
                           ), 
                           tags$hr(),
                           DT::dataTableOutput("instruction_DT") %>% withSpinner()
                  ),
                  tabPanel("Outreach",
                           tags$hr(),
                           fluidRow(
                             column(
                               7,
                               h2("Outreach Statistics")
                             ),
                             column(
                               5,
                               style = "margin-top:20px",
                               align = "right",
                               downloadButton("downloadOutreach", "Download Outreach")
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                               textOutput("outreach_stats"),
                               tags$hr(),
                               DT::dataTableOutput("outreach_DT") %>% withSpinner()
                             )
                           ),
                  ),
                  tabPanel("Info / RAD",
                           tags$hr(),
                           fluidRow(
                             column(
                               7,
                               h2("RAD / Info Desk Statistics")
                             ),
                             column(
                               5,
                               style = "margin-top:20px",
                               align = "right",
                               downloadButton("downloadInfo_RAD", "Download Info/RAD")
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                               textOutput("info_stats"),
                               tags$hr()
                             )
                           ),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("intra_quarter_RAD") %>% withSpinner()
                             ),
                             column(
                               2,
                               selectInput("info_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge")) 
                             )
                           ),
                           tags$hr(),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("info_time_plot") %>% withSpinner(),
                             ),
                             column(
                               2,
                               selectInput("info_scale", "Aggregation Scale:", 
                                           c("Weekly" = "Weekly",
                                             "Daily" = "Daily"))
                             )
                           ),
                           tags$hr(),
                           h3("Service Type Counts"),
                           DT::dataTableOutput("serv_counts_DT") %>% withSpinner(),
                           tags$hr(),
                           DT::dataTableOutput("info_DT") %>% withSpinner()
                  ),
                  tabPanel("Data & GIS Lab",
                           tags$hr(),
                           fluidRow(
                             column(
                               7,
                               h2("Data & GIS Lab Statistics")
                               ),
                             column(
                               5,
                               style = "margin-top:20px",
                               align = "right",
                               downloadButton("downloadDataGISLab", "Download Data & GIS")
                             )
                           ),
                           fluidRow(
                             column(
                               12,
                               textOutput("gis_stats"),
                               tags$hr()
                               )
                           ),
                           fluidRow(
                             column(
                               12,
                               plotlyOutput("gis_lab_hourly") %>% withSpinner(),
                               tags$hr()
                             )
                           ),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("week_of_quarter_gis_lab") %>% withSpinner()
                             ),
                             column(
                               2,
                               selectInput("gis_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge"))
                               )
                           ),
                           fluidRow(
                             column(
                               12,
                               tags$hr(),
                               plotlyOutput("gis_lab_per_week") %>% withSpinner(),
                               tags$hr(),
                             )
                           ),
                           fluidRow(
                             column(
                               10,
                               plotlyOutput("gis_lab_departments") %>% withSpinner(),
                             ),
                             column(
                               2,
                               numericInput("n_gis","Top n Departments:",10,min = 1),
                               selectInput("is_fuzzy_gis", 
                                           "Departments:", 
                                           c("Matched" = "Matched",
                                             "Raw" = "Raw"))
                             )
                           ),
                           tags$hr(),
                           DT::dataTableOutput("data_gis_DT") %>% withSpinner()
                  )
      ) # end tabset panels
      ) #end column 12
  ) #end fluidrow
  
           
))
