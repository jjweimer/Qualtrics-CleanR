####################################################################
#############  HELPER OBJECTS  #####################################
####################################################################

#create month labels for weekly aggregation
month <- seq(as.Date("2020-01-01"), 
             as.Date("2020-12-01"), 
             by = "1 month")
#splits of when each week count corresponds to change in month
month_numeric <- as.numeric(format(month, format = "%U"))
month_numeric <- month_numeric + 1 #add one so weeks start at 1 not 0
#string labels
month_label <- format(month, format = "%b")

#for daily data aggregation
#month scale
month_2 <- c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01",
             "07-01", "08-01", "09-01", "10-01", "11-01", "12-01")

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

#get month-days
#format(date, format = "%m-%d")
month_day <- function(df){
  df$month_day <- format(df$date, format= "%m-%d")
  return(df)
}

#manually clean weird year inputs
clean_years <- function(df){
  
  #drop rows from before 2018 for now
  df <- df[df$year >= 2018,]
  
  df$year[df$year == 2091] <- 2019
  df$year[df$year == 2109] <- 2019
  df$year[df$year == 2921] <- 2021
  
  return(df)
}




