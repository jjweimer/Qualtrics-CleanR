clean_desk_service <- function(raw_service){
  needles <- c("Printing","VPN")
  for(i in needles){
    raw_service[grepl(i,raw_service, fixed = TRUE)] <- i
  }
  return(raw_service)
}