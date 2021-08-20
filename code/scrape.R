# Setup -------------------------------------------------------------------

library('lubridate')
library('tidyverse')
library('rvest')
library('clock')

# Find urls -------------------------------------------------------

# TODO: Generalize method below by finding every Sunday for arbitrary vector of years
# TODO: Make end value be the last Sunday from the present date

`2020` <-
  seq(date_build(2020, 01, 05), date_build(2020, 12, 26), by = 7)
format('%m%d%y')
`2021` <-
  seq(date_build(2021, 01, 03), date_build(2021, 08, 15), by = 7)

dates <- c(`2020`, `2021`)

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
