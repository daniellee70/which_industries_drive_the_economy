# IMPORT CLEAN DATA ----

# Goal
# Import GDP data for the location quotient analysis

# Set up ----

# Core
library(tidyverse)

# FRED data
library(eFRED)

# Time series
library(timetk)


api_key <- "11c92c9630cc2c5c83eb5da3b01a59f0"
set_fred_key(api_key)


# Import GDP data ----

# GDP by state by industry
# Search using naics code instead of state small states like NH doesn't show up in top 1000 results

## Create a custom function to download data ----
NAICS_code <- "NAICS 31-33"
# freq_txt: "A" or "Q"

get_realGDP_by_NAICS <- function(NAICS_code, freq_txt = "A") {

    search_results <- fred_search(text = NAICS_code, key = api_key)

    data <- search_results %>%

        # Get id
        filter(str_detect(title, str_glue("\\({NAICS_code}\\)"))) %>%
        filter(str_detect(title, "Real Gross Domestic Product")) %>%
        filter(!str_detect(title, "Region|District of Columbia")) %>%
        filter(frequency_short == freq_txt) %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%

        # Transform it to long form
        pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%

        # Add meta data
        left_join(search_results %>% select(id, title, units, seasonal_adjustment)) %>%

        # Clean title
        mutate(title = title %>% str_remove(" \\(Except Government and Government Enterprises\\)")) %>%
        separate(title, into = c("variable", "V2"), sep = " \\(") %>%
        separate(variable, into = c("variable", "NAICS_desc"), sep = ": ") %>%
        separate(V2, into = c("NAICS_code", "place"), sep = " in ") %>%
        mutate(NAICS_code = NAICS_code %>% str_remove_all("NAICS |\\)")) %>%

        # Move date to the last day of the year
        mutate(date = date %+time% "2 months" %>% lubridate::rollforward()) %>%
        select(-id)

    # Suspend before the search function
    Sys.sleep(30)

    return(data)

}

gdp <- get_realGDP_by_NAICS(NAICS_code = "NAICS 31-33", freq_txt = "A")

dump(list = c("get_realGDP_by_NAICS"),
     file = "00_scripts/get_realGDP_by_NAICS.R")

## Download data ----

# Get 2 digit NAICS labels
# Remove NAICS 92 Public Administration: No data in FRED
naics_numbers <- c("11", "21", "22", "23", "31-33", "42", "44-45", "48-49",
                   "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")

NAICS_code_vec <- str_glue("NAICS {naics_numbers}") %>% as.vector()

# Download data for all industries
realGDP_states_a_NAICS_tbl <- NAICS_code_vec %>%

    map(.x = ., .f = ~get_realGDP_by_NAICS(NAICS_code = .x, freq_txt = "A")) %>%

    # Collapse list into a tibble
    reduce(bind_rows)


realGDP_states_a_NAICS_tbl

write_rds(realGDP_states_a_NAICS_tbl, "00_data/realGDP_states_a_NAICS_tbl.rds")














# *************************************** ----
# Old code ----




# Industry Indicators ----

# * GDP by state by industry ----
# Search using naics code instead of state small states like NH doesn't show up in top 1000 results

NAICS_code <- "NAICS 31-33"
# freq_txt: "A" or "Q"

get_realGDP_by_NAICS <- function(NAICS_code, freq_txt = "A") {

    search_results <- fred_search(text = NAICS_code, key = api_key)

    data <- search_results %>%

        # Get id
        filter(str_detect(title, str_glue("\\({NAICS_code}\\)"))) %>%
        filter(str_detect(title, "Real Gross Domestic Product")) %>%
        filter(!str_detect(title, "Region|District of Columbia")) %>%
        filter(frequency_short == freq_txt) %>%
        pull(id) %>%

        # Download
        fred(key = api_key) %>%

        # Transform it to long form
        pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%

        # Add meta data
        left_join(search_results %>% select(id, title, units, seasonal_adjustment))

    # Suspend before the search function
    Sys.sleep(40)

    return(data)

}

gdp <- get_realGDP_by_NAICS(NAICS_code = "NAICS 31-33", freq_txt = "Q")

# Get 2 digit NAICS labels
# Remove NAICS 92 Public Administration: No data in FRED
naics_numbers <- c("11", "21", "22", "23", "31-33", "42", "44-45", "48-49",
                   "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")

NAICS_code_vec <- str_glue("NAICS {naics_numbers}") %>% as.vector()

# Download data
realGDP_states_q_NAICS_raw_tbl <- NAICS_code_vec %>%

    map(.x = ., .f = ~get_realGDP_by_NAICS(NAICS_code = .x, freq_txt = "Q")) %>%

    # Collapse list into a tibble
    reduce(bind_rows)


realGDP_states_q_NAICS_tbl <- realGDP_states_q_NAICS_raw_tbl %>%

    # Clean title
    mutate(title = title %>% str_remove(" \\(Except Government and Government Enterprises\\)")) %>%
    separate(title, into = c("variable", "V2"), sep = " \\(") %>%
    separate(variable, into = c("variable", "NAICS_desc"), sep = ": ") %>%
    separate(V2, into = c("NAICS_code", "place"), sep = " in ") %>%
    mutate(NAICS_code = NAICS_code %>% str_remove_all("NAICS |\\)")) %>%

    # Move date to the last day of the year
    mutate(period = str_glue("{year(date)}.{quarter(date)}")) %>%
    select(period, everything(), -date, -id)

realGDP_states_q_NAICS_tbl

write_rds(realGDP_states_q_NAICS_tbl, "00_data/realGDP_states_q_NAICS_tbl.rds")




