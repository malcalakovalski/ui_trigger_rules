# Scratch code at the end of the file allows you to play with the actions within loops
# tables are read in chunks by year because years have different data formatting 
# years are read in chunks because the website will block high volume activity 
setwd("S:/Hamilton_Data/2021/triggersupdate")
# this r file retrieves variables X1 X2 X3 
# you manually set colname within the get_data function

# Note that you will also have to replace wherever you find "x1" with your new variable 

#------------------------------------------------------------------------------------
#Setup
#------------------------------------------------------------------------------------
rm(list = ls())
library("XML")
library("rvest")
library("dplyr")
library('RSelenium')
library('purrr')
#------------------------------------------------------------------------------------
#Bones
#------------------------------------------------------------------------------------
#PREPARE NATIONAL DATA FRAME
state<- c("Alabama", "Alaska","Arizona", "Arkansas", "California", "Colorado",
          "Connecticut", "Delaware", "District of Col", "Florida", "Georgia",
          "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
          "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
          "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
          "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island",
          "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
          "Virgin Islands", "Virgnia", "Washington", "West Virginia", "Wisconsin", 
          "Wyoming")
nationalframe <- data.frame(state)

#FUNCTION TO GET LINK
get_links<- function(year){
  #str(string)
  # year_list = c('2006','2007','2008')
  # year = year_list[year_index]
  # print(year)
  # str(year)
  #year <- '2006'
  year<- as.character(year)
  # url <- paste("http://oui.doleta.gov/unemploy/trigger/", year, "/", sep = "")
  # urlRead <- read_html(url)
  # urlLinks<- urlRead %>% html_nodes("a") %>% html_attr("href")
  # yearurlLinks<- paste(year,"/", urlLinks, sep="")
  # links<- c(urlLinks, yearurlLinks)
  # links<- subset(links, nchar(links) < 23 & nchar(links)>19)
  
  # Download binaries, start driver, and get client object.
  #rd$close()
  #rd$server$stop()
  
  rd <- rsDriver(browser = "chrome", chromever= "92.0.4515.107" , port = 4444L)
  ffd <- rd$client
  # Navigate to page.
  ffd$navigate("https://oui.doleta.gov/unemploy/claims_arch.asp")
  

  ##select the extended benefits trigger notices
  select_notice <- ffd$findElement(using = "css selector", "select")  #content > form > table > tbody > tr:nth-child(2) > td:nth-child(1) > select
  select_notice$clickElement()
  print("notice") #was previously "notice" trying this because it looks like the website changed
  
  Sys.sleep(.1)
  ##choose yearrm(list = ls())
   dropdown <- paste0("//*/option[@value = '", year, "']") #"//*/option[@value = '2006']"
   select_year <- ffd$findElement(using = "xpath", dropdown)  #paste0("//*/option[@value = '", year,"']")) #"//*/option[@value = '2006']") # "//*[(@id = 'year')]/option[@value = '", year, "']"))  
   select_year$clickElement()
   print("year")
   # 
   # "//*[(@id = 'year')]/option[@value = '02']"
   # #year > option:nth-child(3)
   # //*[@id="year"]/option[3]
   
  #click submit
  submit <- ffd$findElement(using = "xpath", "//*[@id='content']/form/table/tbody/tr[3]/td/input")  #//*[@id="content"]/form/table/tbody/tr[3]/td/input
  submit$clickElement()
  print('submit')
  
  
  
  
  # Get HTML data and parse
  html_data <- ffd$getPageSource()[[1]]
  
  html_links <- html_data %>% 
    read_html() %>% 
    html_nodes( "a") %>% 
    html_attr("href") %>%
    grep('/unemploy/trigger/', ., value=TRUE)
  
  ffd$close()
  rd$server$stop()
  rd$server$process
  return(html_links)
  
  }

#FUNCTION TO GET TABLE DATA
get_data<- function(links, colnum, mycol, start, end){
  for (i in start:end){
    Sys.sleep(.25)
    #read file 
    file<- as.character(links[i])
    url<- paste("http://oui.doleta.gov/", file, sep = "")
    url<- read_html(url)
    
    #get data
    mydata<- url %>%
      html_nodes("td") %>%
      html_text()
    data<- data.frame(matrix(mydata, ncol = colnum, byrow=TRUE))
    
    #extract relevant data
    data<-data[c(1:53),]
    colname<-paste("X", mycol, sep = "")
    status_data<- data[mycol]
    
    #append to national dataset 
    nationalframe<- cbind(nationalframe, status_data)
    #rename the column as the date of the trigger notice 
    date<- substr(file, 29, 34)
    
    for (j in 1:length(colname)) {
      colnames(nationalframe)[colnames(nationalframe) == colname[j]] <- paste0(colname[j],"_", as.character(date))
    }
  }
  return(nationalframe)
}



#------------------------------------------------------------------------------------
# scrape trigger notices
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# Scrape Part 1: 2006 2007 2008 2009 2010 2011
#------------------------------------------------------------------------------------
#STORE LINKS TO ALL TRIGGER NOTICES
links<- numeric(0)
years <- c(2006, 2007, 2008, 2009, 2010, 2011)
gc()
links<- unlist(lapply(years, get_links))

#READ IN TABLE FROM EACH LINK
#NOTE: formatting changes in 021311
which(links=="/unemploy/trigger/2011/trig_021311.html")[1]

df1<-get_data(links, 11, c(1,2,3,5,7,11), 1,  59)
df2<-get_data(links, 11, c(1,2,3,5,7,11), 60, 104)
df3<-get_data(links, 11, c(1,2,3,5,7,11), 105, 209)
df4<-get_data(links, 11, c(1,2,3,5,7,11), 210, 267)
#altered for exceptions in 2011
df5<-get_data(links, 13, c(1,2,3,6,8,13), 268, length(links))

#------------------------------------------------------------------------------------
# Scrape part 2: 2012 2013 2014
#------------------------------------------------------------------------------------
#STORE LINKS TO ALL TRIGGER NOTICES
links<- numeric(0)
years <- c(2012, 2013, 2014)
links<- unlist(lapply(years, get_links))

#READ IN TABLE FROM EACH LINK
#formatting changes in 011914
which(links=="2014/trig_011914.html")[1]

df6<-get_data(links, 13, c(1,2,3,6,8,13), 1,  107)
#altered for year 2014
df7<-get_data(links, 10, c(1,2,3,4,6,10), 108, length(links))

#------------------------------------------------------------------------------------
# Scrape part 3: 2015, 2016, 2017, 2018, 2019
#------------------------------------------------------------------------------------
#STORE LINKS TO ALL TRIGGER NOTICES
links<- numeric(0)
years <- c(2015, 2016, 2017, 2018, 2019, 2020)
links<- unlist(lapply(years, get_links))

#READ IN TABLE FROM EACH LINK
df8<-get_data(links, 10, c(1,2,3,4,6,10), 1,  110)
df9<-get_data(links, 10, c(1,2,3,4,6,10), 111, length(links))

#------------------------------------------------------------------------------------
# Clean dataframe, Write as csv
#------------------------------------------------------------------------------------
#merge dataframes 
final<- data.frame(cbind(df1, df2, df3, df4, df5, df6, df7, df8, df9))
#save first column = locations
colnames(final)[1] <- "location"
#drop duplicate state columns
final<-final[, -grep("state", colnames(final))]

#trim white space 
final[] <- apply(final,2,function(x)gsub('\\s+', '',x))

#write csv
write.csv(final, file = "/triggerdata.csv", row.names=FALSE)





# 
# 
# 
# #------------------------------------------------------------------------------------
# # 13 Weeks Insured Unemployment Rate
# #------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------
# # Scrape Part 1: 2006 2007 2008 2009 2010 2011
# #------------------------------------------------------------------------------------
# #STORE LINKS TO ALL TRIGGER NOTICES
# links<- numeric(0)
# years <- c(2006, 2007, 2008, 2009, 2010, 2011)
# links<- unlist(lapply(years, get_links))
# 
# #READ IN TABLE FROM EACH LINK
# #NOTE: formatting changes in 021311
# which(links=="2011/trig_021311.html")[1]
# 
# df1<-get_data(links, 11, 5, 1,  59)
# df2<-get_data(links, 11, 5, 60, 104)
# df3<-get_data(links, 11, 5, 105, 209)
# df4<-get_data(links, 11, 5, 210, 267)
# #altered for exceptions in 2011
# df5<-get_data(links, 13, 6, 268, length(links))
# 
# #------------------------------------------------------------------------------------
# # Scrape part 2: 2012 2013 2014
# #------------------------------------------------------------------------------------
# #STORE LINKS TO ALL TRIGGER NOTICES
# links<- numeric(0)
# years <- c(2012, 2013, 2014)
# links<- unlist(lapply(years, get_links))
# 
# #READ IN TABLE FROM EACH LINK
# #formatting changes in 011914
# which(links=="2014/trig_011914.html")[1]
# 
# df6<-get_data(links, 13, 6, 1,  107)
# #altered for year 2014
# df7<-get_data(links, 10, 4, 108, length(links))
# 
# #------------------------------------------------------------------------------------
# # Scrape part 3: 2015, 2016, 2017, 2018, 2019
# #------------------------------------------------------------------------------------
# #STORE LINKS TO ALL TRIGGER NOTICES
# links<- numeric(0)
# years <- c(2015, 2016, 2017, 2018, 2019)
# links<- unlist(lapply(years, get_links))
# 
# #READ IN TABLE FROM EACH LINK
# df8<-get_data(links, 10, 4, 1,  110)
# df9<-get_data(links, 10, 4, 111, length(links))
# 
# #------------------------------------------------------------------------------------
# # Clean dataframe, Write as csv
# #------------------------------------------------------------------------------------
# #merge dataframes 
# final<- data.frame(cbind(df1, df2, df3, df4, df5, df6, df7, df8, df9))
# #save first column = locations
# colnames(final)[1] <- "location"
# #drop duplicate state columns
# final<-final[, -grep("state", colnames(final))]
# 
# #trim white space 
# final[] <- apply(final,2,function(x)gsub('\\s+', '',x))
# 
# #write csv
# write.csv(final, file = "/Users/catherinepeng/Desktop/work/stateEB_scrapings/StateEB_x5.csv", row.names=FALSE)
# #------------------------------------------------------------------------------------
# # 3 months S.A.TUR
# #------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------
# # Scrape Part 1: 2006 2007 2008 2009 2010 2011
# #------------------------------------------------------------------------------------
# #STORE LINKS TO ALL TRIGGER NOTICES
# links<- numeric(0)
# years <- c(2006, 2007, 2008, 2009, 2010, 2011)
# links<- unlist(lapply(years, get_links))
# 
# #READ IN TABLE FROM EACH LINK
# #NOTE: formatting changes in 021311
# which(links=="2011/trig_021311.html")[1]
# #find that this is 268 in links 
# 
# df1<-get_data(links, 11, 7, 1,  59)
# df2<-get_data(links, 11, 7, 60, 104)
# df3<-get_data(links, 11, 7, 105, 209)
# df4<-get_data(links, 11, 7, 210, 267)
# #altered for exceptions in 2011
# df5<-get_data(links, 13, 8, 268, length(links))
# 
# #------------------------------------------------------------------------------------
# # Scrape part 2: 2012 2013 2014
# #------------------------------------------------------------------------------------
# #STORE LINKS TO ALL TRIGGER NOTICES
# links<- numeric(0)
# years <- c(2012, 2013, 2014)
# links<- unlist(lapply(years, get_links))
# 
# #READ IN TABLE FROM EACH LINK
# #formatting changes in 011914
# which(links=="2014/trig_011914.html")[1]
# 
# df6<-get_data(links, 13, 8, 1,  107)
# #altered for year 2014
# df7<-get_data(links, 10, 6, 108, length(links))
# 
# #------------------------------------------------------------------------------------
# # Scrape part 3: 2015, 2016, 2017, 2018, 2019
# #------------------------------------------------------------------------------------
# #STORE LINKS TO ALL TRIGGER NOTICES
# links<- numeric(0)
# years <- c(2015, 2016, 2017, 2018, 2019)
# links<- unlist(lapply(years, get_links))
# 
# #READ IN TABLE FROM EACH LINK
# df8<-get_data(links, 10, 6, 1,  110)
# df9<-get_data(links, 10, 6, 111, length(links))
# 
# #------------------------------------------------------------------------------------
# # Clean dataframe, Write as csv
# #------------------------------------------------------------------------------------
# #merge dataframes 
# final<- data.frame(cbind(df1, df2, df3, df4, df5, df6, df7, df8, df9))
# #save first column = locations
# colnames(final)[1] <- "location"
# #drop duplicate state columns
# final<-final[, -grep("state", colnames(final))]
# 
# #trim white space 
# final[] <- apply(final,2,function(x)gsub('\\s+', '',x))
# 
# #write csv
# write.csv(final, file = "/Users/catherinepeng/Desktop/work/stateEB_scrapings/StateEB_x7.csv", row.names=FALSE)
# #------------------------------------------------------------------------------------
# 
# #------------------------------------------------------------------------------------
# # clean data
# #------------------------------------------------------------------------------------
# dfx1<- read.csv("/Users/catherinepeng/Desktop/work/stateEB_scrapings/StateEB_x1.csv")
# #find what you need to clean out 
# valuesx1<- as.vector(as.matrix(dfx1))
# unique(valuesx1)
# #clean
# dfx1[] <- apply(dfx1,2,function(x)gsub('&nbsp', '',x))
# #rewrite .csv
# write.csv(dfx1, file = "/Users/catherinepeng/Desktop/work/stateEB_scrapings/StateEB_x1.csv", row.names=FALSE)
