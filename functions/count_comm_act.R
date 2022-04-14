count_comm_act <- function(raw_comm_act){
  #normalize and count each comm activity
  comm_act <- c("Copyright, IP, Fair Use & Author rights",
                "Research Impact, Metrics, Scholarly Reputation",
                "Publishing Practices: new modes & tools",
                "Funder & UC Public Access Policies",
                "Open Access and Author Publishing Charges",
                "Other")
  container <- c()
  for (i in 1:length(comm_act)){
    #use grepl to normalize using needles pre defined
    cleaned <- raw_comm_act # do this to preserve original list
    cleaned[grepl(comm_act[i],cleaned, fixed = TRUE)] <- comm_act[i] # redefine on new list
    #count
    count <- sum(cleaned == comm_act[i])
    #store that count in a list
    container[i] <- count
  }
  df <- data.frame(comm_act,container)
  colnames(df)[2] <- "n"
  return(df)
}