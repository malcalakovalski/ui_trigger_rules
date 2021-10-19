# Setup -------------------------------------------------------------------
library('lubridate')
library('tidyverse')
library('rvest')
library('clock')
library('purrr')
library('janitor')
library('glue')
library('dplyr')
library('writexl')

##the original code uses selenium which wasn't working properly for either of us and it wasn't necessary for this scrape
#the code below allows you to use the url pattern and release dates to just pull together all of the files
# Find urls -------------------------------------------------------
#allows us to select only the dates we need to suse to grab the URLS
pick_wkday <- function(wkday, start, end = lubridate::today()) {
  start <- lubridate::as_date(start)
  end <- lubridate::as_date(end)
  wkday <- switch(wkday,
                  'Sunday' = 0,
                  'Monday' = 1,
                  "Tuesday" = 2,
                  "Wednesday" = 3,
                  "Thursday" = 4,
                  "Friday" = 5,
                  "Saturday" = 6)
  fwd_7 <- start + 0:6
  first_day <- fwd_7[as.numeric(format(fwd_7,"%w")) == wkday]
  seq.Date(first_day, end, by="week")
}
##Select your start date, it will then run the pick weekday function up to today
##i had to break up dates into smaller chunks because DOL blocks you from scraping massive amounts apparently
dates <- pick_wkday('Sunday', '2014-01-19', today())
dates1<-pick_wkday('Sunday', '2011-02-13', '2014-01-12')
dates2<-pick_wkday('Sunday', '2003-01-05', '2007-12-30')
# they made an error with the link here: https://oui.doleta.gov/unemploy/trigger/2008/trig_010607.html will fix later.
dates3<-pick_wkday('Sunday', '2008-01-13', '2011-02-06')
#Glue allows us to insert the dates in the same sort of way we would in Python
urls <- glue::glue('https://oui.doleta.gov/unemploy/trigger/{year(dates)}/trig_{format(dates, "%m%d%y")}.html')
#some of the earlier years are set up differently so I will clean them separately
urls1<-glue::glue('https://oui.doleta.gov/unemploy/trigger/{year(dates1)}/trig_{format(dates1, "%m%d%y")}.html')

urls2 <- glue::glue('https://oui.doleta.gov/unemploy/trigger/{year(dates2)}/trig_{format(dates2, "%m%d%y")}.html')
urls3 <- glue::glue('https://oui.doleta.gov/unemploy/trigger/{year(dates3)}/trig_{format(dates3, "%m%d%y")}.html')
urls4 <- glue::glue('https://oui.doleta.gov/unemploy/trigger/2008/trig_010607.html')
# Scrape ------------------------------------------------------------------
##next we read in the data with rvest from our url and clean it with janitor
##for 2014-01-19 to today
read_data <- function(urls) {
  date <- stringr::str_extract(urls, '\\d{6}')
  html <- rvest::read_html(urls)
  html %>%
    html_element('table') %>%
    html_table(header = FALSE) %>%
    select(1:last_col()) %>%
    janitor::row_to_names(row_number = 10) %>%
    janitor::clean_names() %>%
    select(
      on_threetur=1,
      no_sixtur=2,
      noturoption=3,
      state = 4,
      unemployment_rate = 5,
      prior_years_percent = 6,
      tur_sa = 7,
      percent_of_last_year = 8,
      percent_of_second_last_year = 9,
      available_weeks = 10,
      status = 11
    ) %>%
    slice(-1) %>%
    mutate(across(
      c(unemployment_rate:percent_of_second_last_year),
      as.numeric
    )) %>%
    drop_na(unemployment_rate) %>%
    mutate(
      date = lubridate::as_date(date, format = '%m%d%y'),
      .before = everything(),
      state = as_factor(state)
    )
}


read_data1 <- function(urls1) {
  date <- stringr::str_extract(urls1, '\\d{6}') 
  html <- rvest::read_html(urls1)
  html %>%
    html_element('table') %>%
    html_table(header = FALSE) %>%
    select(1:last_col()) %>%
    janitor::row_to_names(row_number = 13) %>% #changed rows to 10
    janitor::clean_names() %>%
    select(
      on_threetur=1,
      no_sixtur=2,
      noturoption=3,
      state = 4,
      iur=5,#added iur
      tur=6,#added tur
      unemployment_rate = 7,
      prior_years_percent = 8,
      tur_sa = 9,
      percent_of_last_year = 10,
      percent_of_second_last_year = 11,
      percent_of_third_last_year=12, #added 3rd year
      available_weeks = 13,
      status = 14
    ) %>%
    slice(-1) %>%
    mutate(across(
      c(iur:available_weeks), #changed to iur and available weeks as numeric
      as.numeric
    )) %>%
    drop_na(iur) %>% #changed to iur see undropped by # to see why.
    mutate(
      date = lubridate::as_date(date, format = '%m%d%y'),
      .before = everything(),
      state = as_factor(state)
    )
}

read_data2 <- function(urls2) {
  date <- stringr::str_extract(urls2, '\\d{6}')
  html <- rvest::read_html(urls2)
  html %>%
    html_element('table') %>%
    html_table(header = FALSE) %>%
    select(1:last_col()) %>%
    janitor::row_to_names(row_number = 11) %>%
    janitor::clean_names() %>%
      select(
        on_threetur=1,
        no_sixtur=2,
        noturoption=3,
        state = 4,
        blank=5,
        unemployment_rate = 6,
        prior_years_percent = 7,
        tur_sa = 8,
        percent_of_last_year =9,
        percent_of_second_last_year = 10,
        available_weeks = 11,
        status = 12
    ) %>%
    slice(-1) %>%
    mutate(across(
      c(unemployment_rate:available_weeks),
      as.numeric
    )) %>%
    drop_na(unemployment_rate) %>%  
    mutate(
      date = lubridate::as_date(date, format = '%m%d%y'),
      .before = everything(),
      state = as_factor(state)
    )
}

read_data3 <- function(urls3) {
  date <- stringr::str_extract(urls3, '\\d{6}')
  html <- rvest::read_html(urls3)
  html %>%
    html_element('table') %>%
    html_table(header = FALSE) %>%
    select(1:last_col()) %>%
    janitor::row_to_names(row_number = 8) %>%
    janitor::clean_names() %>%
    select(
      on_threetur=1,
      no_sixtur=2,
      noturoption=3,
      state = 4,
      blank=5,
      unemployment_rate = 6,
      prior_years_percent = 7,
      tur_sa = 8,
      percent_of_last_year =9,
      percent_of_second_last_year = 10,
      available_weeks = 11,
      status = 12
    ) %>%
    slice(-1) %>%
    mutate(across(
      c(unemployment_rate:percent_of_second_last_year),
      as.numeric
    )) %>%
    drop_na(unemployment_rate) %>%
    mutate(
      date = lubridate::as_date(date, format = '%m%d%y'),
      .before = everything(),
      state = as_factor(state)
    )
}
# Combine and export ------------------------------------------------------

ui_trigger_data <- purrr::map_dfr(.x = urls,
                                  .f = read_data)
ui_trigger_data1 <- purrr::map_dfr(.x=urls1,
                                   .f=read_data1)
ui_trigger_data2<- purrr::map_dfr(.x=urls2,
                                  .f=read_data2)
ui_trigger_data3<- purrr::map_dfr(.x=urls3,
                                  .f=read_data3)
ui_trigger_data4<- purrr::map_dfr(.x=urls4,
                                  .f=read_data2)
###ignore errors for data4 will manually fix 
ui_trigger_data4clean<-ui_trigger_data4%>%drop_na(unemployment_rate)%>%add_column(iur= NA)%>%add_column(tur= NA)%>%add_column(percent_of_third_last_year= NA)
ui_trigger_data4clean['date'][ui_trigger_data4clean['date'] == '2007-01-06'] <- as_date('2008-01-06')

#making all columns the same so I can append everything
ui_trigger_dataclean<-ui_trigger_data%>%add_column(iur= NA)%>%add_column(tur= NA)%>%add_column(blank= NA)%>%add_column(percent_of_third_last_year= NA)
ui_trigger_data1clean<-ui_trigger_data1%>%add_column(blank= NA)
ui_trigger_data2clean<-ui_trigger_data2%>%add_column(iur= NA)%>%add_column(tur= NA)%>%add_column(percent_of_third_last_year= NA)
ui_trigger_data3clean<-ui_trigger_data3%>%add_column(iur= NA)%>%add_column(tur= NA)%>%add_column(percent_of_third_last_year= NA)

ebtriggerscrape<-rbind(ui_trigger_dataclean, ui_trigger_data1clean, ui_trigger_data2clean, ui_trigger_data3clean, ui_trigger_data4clean)

#cleaning up some of those cells that were formatted weirdly.

ebtriggerscrapeclean<-ebtriggerscrape %>%
  filter(on_threetur != 'Total Number "ON":  3')
ebtriggerscrapeclean<-arrange(ebtriggerscrapeclean,date)

##CHANGE FILE PATH TO NEW LOCATION
writexl::write_xlsx(ebtriggerscrapeclean, 'C:/Users/18145/OneDrive/Desktop/THP/ui_data/ui_trigger_2003_2021.xlsx')

