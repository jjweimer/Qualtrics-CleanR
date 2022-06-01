dataprep_gis_lab_hourly <- function(df){
  #aggregate hourly traffic counts
  hourly <- df %>% group_by(hour) %>% count(hour)
  hourly <- hourly[hourly$hour != "",]
  #convert to factor for custom ordering
  hourly$hour <- 
    factor(
      hourly$hour, 
      levels = 
        c("8:00 AM", "8AM",   
          "9:00 AM", "9AM",
          "10:00 AM", "10AM",
          "11:00 AM", "11AM",
          "12:00 PM", "12PM",
          "1:00 PM", "1PM",
          "2:00 PM", "2PM",
          "3:00 PM","3PM",
          "4:00 PM", "4PM",
          "5:00 PM", "5PM",
          "6:00 PM", "6PM",
          "7:00 PM", "7PM",
          "8:00 PM", "8PM",
          "9:00 PM", "9PM",
          "10:00 PM","10PM",
          "11:00 PM", "11PM")
    )
  return(hourly)
}