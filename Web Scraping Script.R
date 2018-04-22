## Scott Morrison
## L0013687
## Web scraping the top 1000 most expensive house for sale in ireland from the zoopla website

Get_Most_Expensive <- function(url){
  library('rvest')
  
  for(i in 1:10){
    if(i == 1){
      webpage <- read_html(url)
      
      Prices <- data.frame(Price = webpage %>% html_nodes(".text-price") %>% html_text()) 
      Address <- data.frame(Address = webpage %>% html_nodes(".listing-results-address") %>% html_text())
      Expensive_houses <- cbind(Address, Prices)
    } else {
      webpage <- read_html(paste0(url,"&amp;new_homes=include&amp;include_sold=false&amp;pn=",i))
      Prices <- data.frame(Price = webpage %>% html_nodes(".text-price") %>% html_text()) 
      Address <- data.frame(Address = webpage %>% html_nodes(".listing-results-address") %>% html_text())
      temp <- cbind(Address, Prices)
      Expensive_houses <- rbind(Expensive_houses, temp)
      
    }
  } # End of for loop
  return(Expensive_houses)
} # End of function

House_Data <- Get_Most_Expensive('https://www.zoopla.co.uk/overseas/property/ireland/?keywords=-POA&results_sort=highest_price&page_size=100')

## Removing unwanted characters with regex to extract price as numeric field
House_Data$Price <- gsub(".*£","", House_Data$Price)
House_Data$Price <- sub(" .*", "", House_Data$Price)
House_Data$Price <- as.numeric(gsub(",", "", House_Data$Price))

## Removing where price = NA, this is houses listed as POA
House_Data <- House_Data[!is.na(House_Data$Price),]

## Predefined list of counties
Counties <- data.frame( County = c("Antrim","Armagh","Carlow","Cavan","Clare","Cork","Derry","Donegal","Down",
                                   "Dublin","Fermanagh","Galway","Kerry","Kildare","Kilkenny","Laois","Leitrim",
                                   "Limerick","Longford","Louth","Mayo","Meath","Monaghan","Offaly","Roscommon",
                                   "Sligo","Tipperary","Tyrone","Waterford","Westmeath","Wexford","Wicklow"))


## Add County to the dataset, joins a predefined list of counties to the dataset based on partially related rows
House_Data$Address <- as.character(House_Data$Address)
Counties$County <- as.character(Counties$County)

library(fuzzyjoin)
House_Data <- House_Data %>% regex_inner_join(Counties, by = c(Address = "County"))

## Return the number of houses in each county from the 1000 most expensive
Count <- data.frame(table(House_Data$County))
head(Count[order(Count$Freq, decreasing = TRUE),])

str(House_Data)