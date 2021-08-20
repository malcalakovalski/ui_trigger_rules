# Setup -------------------------------------------------------------------

library('lubridate')
library('tidyverse')
library('rvest')
library('clock')

# Find urls -------------------------------------------------------

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

dates <- pick_wkday('Sunday', '2011-01-01', today())

urls <- glue::glue('https://oui.doleta.gov/unemploy/trigger/{year(dates)}/trig_{format(dates, "%m%d%y")}.html')

# Scrape ------------------------------------------------------------------

read_data <- function(url) {
  date <- stringr::str_extract(url, '\\d{6}')
  html <- rvest::read_html(url)
  html %>%
    html_element('table') %>%
    html_table(header = FALSE) %>%
    select(4:last_col()) %>%
    janitor::row_to_names(row_number = 7) %>%
    janitor::clean_names() %>%
    select(
      state = 1,
      unemployment_rate = 2,
      prior_years_percent = 3,
      tur_sa = 4,
      percent_of_last_year = 5,
      percent_of_second_last_year = 6,
      available_weeks = 7,
      status = 8
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
writexl::write_xlsx(ui_data, 'data/ui_trigger_data.xlsx')
