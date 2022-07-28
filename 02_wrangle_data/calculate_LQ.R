# IMPORT CLEAN DATA ----

# Goal
# Import data to build economic dashboard of Russell Investment
# https://russellinvestments.com/us/resources/financial-professionals/economic-indicators-dashboard

# Set up

# Core
library(tidyverse)
library(tidyquant)

# Time series
library(timetk)

# Interactive visualization
library(plotly)
library(tidytext)


# Import data ----

gdp <- read_rds("00_data/realGDP_states_a_NAICS_tbl.rds")


# Clean data ----

# Note: state total is the sum of the private industry, excluding NAICS 92: Public Administration

gdp_us_tbl <- gdp %>%

    # filter(year %in% c(min(year), max(year))) %>%
    filter(place == "the United States") %>%

    # Add year
    mutate(year = year(date)) %>%

    # Get all industry total for us
    add_tally(value, name = "total") %>%
    mutate(pct_of_total_us = value / total)

gdp_us_tbl

gdp_state_tbl <- gdp %>%

    # filter(year %in% c(min(year), max(year))) %>%
    filter(place != "the United States") %>%

    # Add year
    mutate(year = year(date)) %>%

    select(year, value, NAICS_code, NAICS_desc, place) %>%

    # Calculate state total
    add_count(place, wt = value, name = "state_total") %>%

    # Calculate industry pct of state total
    group_by(place) %>%
    mutate(pct_of_total_state = value / state_total) %>%
    ungroup()

gdp_state_tbl

# Calculate LQ ----

gdp_LQ_tbl <- gdp_state_tbl %>%

    select(year, NAICS_code, NAICS_desc, place, value, state_total, pct_of_total_state) %>%

    left_join(gdp_us_tbl %>% select(year, NAICS_code, pct_of_total_us)) %>%

    # Calculate LQ
    group_by(place) %>%
    mutate(LQ = pct_of_total_state / pct_of_total_us) %>%
    ungroup() %>%

    # Calculate LQ in $
    mutate(value_wouldbe = state_total * pct_of_total_us) %>%
    mutate(LQ_dollar = value - value_wouldbe) %>%

    # Create rank within year and place
    group_by(year, place) %>%
    arrange(desc(LQ)) %>%
    mutate(LQ_rank = row_number()) %>%
    ungroup() %>%

    # Create rank within year and place
    group_by(year, place) %>%
    arrange(desc(LQ_dollar)) %>%
    mutate(LQ_dollar_rank = row_number()) %>%
    ungroup() %>%

    mutate(label_lq = str_glue("State: {place}
                                Year: {year}
                                NAICS: {NAICS_desc}
                                real GDP: {value %>% scales::dollar(suffix = 'M')}
                                Location Quotient: {LQ %>% round(digits = 1)}
                                It is the {LQ_rank}th most important industry in {place},
                                measured by location quotients.")) %>%

    mutate(label_LQ_dollar = str_glue("State: {place}
                                      Year: {year}
                                      NAICS: {NAICS_desc}
                                      real GDP: {value %>% scales::dollar(suffix = 'M')}
                                      Location Quotient in $: {LQ_dollar %>% scales::dollar(suffix = 'M')}
                                      It is the {LQ_dollar_rank}th most important industry in {place},
                                      measured by location quotients in dollars."))

gdp_LQ_tbl

write_rds(gdp_LQ_tbl, "00_data/gdp_LQ_tbl.rds")

