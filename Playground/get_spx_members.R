install.packages(c("Rblpapi", "dplyr")) #Install needed packages

Rblpapi::blpConnect() #Connect to Bloomberg

get_spx_members <- function(date){

  date <- format(date, "%Y%m%d")
  
  print(date) # Just for showing which date we retrieve
  
members <- Rblpapi::bds("SPX Index", "INDX_MWEIGHT_HIST", overrides = c("END_DATE_OVERRIDE" = date)) #Get members

members_date <- data.frame("members" = members$`SPX Index`$`Index Member`, 
                           "date" = date) 

return(members_date)

}

#Generate Sequence
dates <- seq(as.Date("1990-01-31"), as.Date("2021-12-01"), by = "month")

#Run function over all dates
historical_members <- do.call(rbind, lapply(dates, get_spx_members))

# Aggregate with min and max dates
members_aggregated <- historical_members %>% 
  dplyr::group_by(members) %>% 
  dplyr::summarize(min_date = min(as.Date(date)),
                   max_date = max(as.Date(date)))

#Write as CSV
write.csv(members_aggregated, file = "your/preferred/path/yourfilename.csv")
