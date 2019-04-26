## ----setup, include = FALSE----------------------------------------------
library(tidyverse)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(realEstAnalytics)
set_zillow_web_service_id('X1-ZWz181enkd4cgb_82rpe')
YOURAPIKEYHERE = getOption('ZillowR-zws_id')

## ---- eval = FALSE-------------------------------------------------------
#  #installing and loading realEstAnalytics
#  devtools::install_github('xiyuansun/realEstAnalytics')
#  
#  library(realEstAnalytics)

## ---- eval=FALSE---------------------------------------------------------
#  #set the ZWS_ID
#  set_zillow_web_service_id('YOUR_API_KEY')

## ------------------------------------------------------------------------
#retrieve the current ZWS_ID in use
zapi_key = getOption('ZillowR-zws_id')

## ------------------------------------------------------------------------
GetDeepSearchResults('600 S. Quail Ct.', city='Newton',state='KS', zipcode=NULL,
                     api_key=getOption('ZillowR-zws_id'))

## ------------------------------------------------------------------------
GetDeepSearchResults('600 S. Quail Ct.', zipcode=67114,
                     rentzestimate=TRUE, api_key=getOption('ZillowR-zws_id'))

## ------------------------------------------------------------------------
library(dplyr)
library(magrittr)
addresses <- c('733 Normandy Ct.',
  '600 S. Quail Ct.',
  '105 S Logan St.',
  '1412 W. 8th St.',
  '2801 Goldenrod Rd.',
  '2309 Ivy Ave.',
  '121 S Hess Ave.',
  '321 E Vesper St.',
  '6219 NW Parkview St.',
  '623 Meadowlark Ln.'
  )

cities <- c(rep('Newton', times=4),'North Newton','North Newton',
            'Hesston','Hesston','Park City', 'Newton')

zips <- c(rep(67114, times =4), 67117,67117,67062,67062,67219,67114) %>%
  as.character()

state <- rep('KS', times=length(zips))

addex <- data.frame(address=addresses,zipcode=zips,city=cities,state=state)

newtonaddresses <- GetComps(1340244, count=20, api_key = getOption('ZillowR-zws_id')) %>%
  select(address,zipcode,city,state) %>% mutate_all(as.character) %>% 
  rbind(c('3425 Locust St.', '64109', 'Kansas City', 'MO'), addex) %>%
  sample_n(size=32)

## ----eval=FALSE----------------------------------------------------------
#  #there are 32 addresses, some in different zipcodes, to look up
#  #GetDeepSearchResults_dataframe will get the info for us:
#  
#  GetDeepSearchResults_dataframe(.df=newtonaddresses,
#                                 col.address=1 , col.zipcode=2 , col.city=3 , col.state=4,
#                                 api_key=getOption('ZillowR-zws_id'))

## ------------------------------------------------------------------------

#retrieve the zpid from GetDeepSearchResults
zpidex <- GetDeepSearchResults('600 S. Quail Ct.', zipcode=67114,
                     rentzestimate=TRUE, api_key=getOption('ZillowR-zws_id'))$zpid

#GetComps for the '600 S. Quail Ct.' address
GetComps(zpidex, count=10, rentzestimate=TRUE, api_key = getOption('ZillowR-zws_id'))

#GetDeepComps returns the same information as GetComps, with additional property data
GetDeepComps(zpidex, count=10, rentzestimate=FALSE, api_key = getOption('ZillowR-zws_id'))

## ------------------------------------------------------------------------
#GetZestimate with a vector input
GetZestimate(zpids=c(zpidex,109818062,1341669,1341715) ,
             rentzestimate=TRUE , api_key=getOption('ZillowR-zws_id'))

## ------------------------------------------------------------------------
library(purrr)
#build a dataset in one sequence of commands
#starting from one address
richdata <- GetDeepSearchResults('600 S. Quail Ct.',
                                 zipcode=67114,
                     rentzestimate=TRUE,
                     api_key=getOption('ZillowR-zws_id')) %>%
  dplyr::select(zpid) %>%
  purrr::as_vector("character") %>% 
  GetComps(count=10, api_key=getOption('ZillowR-zws_id')) %>%
  dplyr::select(zpid) %>%
  purrr::as_vector("character") %>% 
  lapply(GetDeepComps,
         count=10,
         api_key=getOption('ZillowR-zws_id')) %>% 
  dplyr::bind_rows() %>% 
  dplyr::distinct()

head(richdata)
dim(richdata)

## ------------------------------------------------------------------------
library(XML)
#Get Chart returns a list with the API's response
#The chart URL is in the `response` element in the `url` attribute
chartex <- GetChart(zpid = 1340244, unit_type = 'dollar', width = 600, height = 300,
          chartDuration = '10years', zws_id = getOption('ZillowR-zws_id'))
XML::names.XMLNode(chartex$response)

## ---- eval=F, include=FALSE----------------------------------------------
#  #NOT RUN
#  #In R, we can get the chart using a few manipulations
#  library(magick)
#  library(stringr)
#  charturl <-'https://www.zillow.com:443/app?chartDuration=10years&amp;chartType=partner&amp;height=300&amp;page=webservice%2FGetChart&amp;service=chart&amp;width=600&amp;zpid=1340244'
#  
#  charturl.fix <- stringr::str_remove_all(charturl, 'amp\\;')
#  
#  #magick will display the chart
#  magick::image_read(charturl.fix)

## ---- echo = T, results = 'hide'-----------------------------------------
#What data do we want to filter?
GetDeepSearchResults('600 S. Quail Ct.', zipcode=67114,
                      rentzestimate=TRUE, api_key=getOption('ZillowR-zws_id')) %>%
  dplyr::select(zipcode,city,state,bedrooms,zestimate)

#Pull the data by state and zipcode for 4 bedrooms
cityseries <- get_ZHVI_series(bedrooms=4,geography="Zip") %>%
  dplyr::filter(RegionName=='67114')

Stateseries <- get_ZHVI_series(bedrooms=4,geography="State") %>% 
  dplyr::filter(RegionName=='Kansas')

#Also, collect all top-tier home values in the city and state
citytop <- get_ZHVI_series(allhomes=TRUE, tier='T', geography="Zip") %>%
  dplyr::filter(RegionName=='67114')

## ------------------------------------------------------------------------
names(citytop)[1:8]

dim(citytop)

## ---- echo=TRUE, results='hide', warning=F-------------------------------
#melting the data using reshape2 and zoo
topmelted <- citytop %>% 
  reshape2::melt(id=1:7, variable.name='Date', value.name='MedianPrice') %>%
  dplyr::mutate(Date=(zoo::as.yearmon(Date)))

statemelted <- Stateseries %>%
  reshape2::melt(id=1:3, variable.name='Date', value.name='MedianPrice') %>%
  dplyr::mutate(Date=(zoo::as.yearmon(Date)))

citymelted <- cityseries %>% 
  reshape2::melt(id=1:7, variable.name='Date', value.name='MedianPrice') %>%
  dplyr::mutate(Date=(zoo::as.yearmon(Date)))

## ---- echo=FALSE,fig.width=8.5, fig.height=7, warning=F------------------
tscomb <- data.frame(Date=topmelted$Date, Zip=citymelted$MedianPrice, State=statemelted$MedianPrice,
                     TopTier=topmelted$MedianPrice) %>%
 reshape2::melt(id=1, variable.name="geography", value.name='price') 

plotZHVI<- function(ts.melted, date.min=NULL, date.max=NULL){
  if(is.null(date.min)) date.min = min(ts.melted$Date)
  if(is.null(date.max)) date.max = max(ts.melted$Date)
  ts.melted %>% dplyr::filter(dplyr::between(Date, zoo::as.yearmon(date.min), zoo::as.yearmon(date.max))) %>%
    ggplot() + geom_line(aes(x=Date, y=price, col=geography),size=2)  +
    zoo::scale_x_yearmon(n=30) +
  scale_y_continuous(breaks=seq(round(min(ts.melted$price)-10000,-3),max(ts.melted$price)+10000, by=10000)) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5),
          legend.title=element_text(size=12, face = "bold"),
          legend.text=element_text(size=12),
          axis.title.y = element_text(size=16, angle=90, vjust=0.5),
          axis.title.x = element_text(size=16),
          axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x='Date',y="Median Home Price")
}


plotZHVI(ts.melted=tscomb) + ggtitle("Newton Kansas Median Home Values")

## ---- echo=TRUE, results='hide'------------------------------------------
#The most recent rental listing value for 4BR homes in Kansas
KSrentals <- get_rental_listings(bedrooms=4, rate='PerSqFt',geography="State") %>%
  dplyr::filter(RegionName=='Kansas')

## ------------------------------------------------------------------------
KSrentals %>% dplyr::last()

#How does our target property compare?
GetDeepSearchResults('600 S. Quail Ct.', zipcode=67114,
                     rentzestimate=TRUE, api_key=getOption('ZillowR-zws_id')) %>%
  dplyr::mutate(rentpersqft = rentzestimate/finishedSqFt) %>%
  select(rentpersqft)

## ------------------------------------------------------------------------
GetDeepSearchResults('600 S. Quail Ct.', zipcode=67114,
                     rentzestimate=TRUE, api_key=getOption('ZillowR-zws_id'),
                     raw=TRUE) %>% xml2::xml_children()

