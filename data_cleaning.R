library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(httr2)
library(jsonlite)

## From P4
fetch_data <- function(endpoint) {
  response <- request(endpoint) %>%
    req_url_query("$limit" = 10000000) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    as.data.frame()
  
  return(response)
}
cases_url  <- "https://data.cdc.gov/resource/pwn4-m3yp.json"
hosp_url   <- "https://data.cdc.gov/resource/39z2-9zu6.json"
deaths_url <- "https://data.cdc.gov/resource/r8kw-7aab.json"
vax_url    <- "https://data.cdc.gov/resource/rh2h-3yt2.json"
cases_raw  <- fetch_data(cases_url)
hosp_raw   <- fetch_data(hosp_url)
deaths_raw <- fetch_data(deaths_url)
vax_raw    <- fetch_data(vax_url)
# doing the same thing as p4, can delete if not necessary
# this create a fake population dataset, later merge with our dataset
make_pop_data <- function() {
  states <- c(
    "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
    "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
    "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
    "VA","WA","WV","WI","WY","DC"
  )
  
  state_names <- c(
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
    "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
    "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan",
    "Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
    "New Hampshire","New Jersey","New Mexico","New York","North Carolina",
    "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
    "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
    "Virginia","Washington","West Virginia","Wisconsin","Wyoming",
    "District of Columbia"
  )
  
  pop_data <- data.frame()
  
  for (year in c(2020, 2021, 2022)) {
    for (i in seq_along(states)) {
      # Seeding the random generator to keep the data consistent each run
      set.seed(i * 100 + year)
      population <- round(runif(1, 500000, 40000000))
      region_index <- ceiling(i / 10)
      region_name <- case_when(
        region_index == 1 ~ "Northeast",
        region_index == 2 ~ "Mid-Atlantic",
        region_index == 3 ~ "Southeast",
        region_index == 4 ~ "Great Lakes",
        region_index == 5 ~ "Heartland",
        region_index == 6 ~ "South Central",
        region_index == 7 ~ "Central Plains",
        region_index == 8 ~ "Mountain West",
        region_index == 9 ~ "Pacific",
        region_index == 10 ~ "Pacific Northwest",
        TRUE ~ "Other"
      )
      # We add one row of data for each state & year
      pop_data <- rbind(
        pop_data,
        data.frame(
          state_name = state_names[i],
          year       = year,
          pop        = population,
          state      = states[i],
          region     = region_index,
          region_name = region_name,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  return(pop_data)
}
# Create the fake population dataset
population <- make_pop_data()

#function to clean dataset
clean_to_monthly <- function(df, type) {
  if (type == "cases") {
    # sum the new cases by month + year + state.
    data_cases <- df %>%
      select(state, start_date, new_cases) %>%
      mutate(
        start_date = as.Date(start_date),
        month = month(start_date),
        year  = year(start_date),
        cases = as.numeric(new_cases)
      ) %>%
      filter(year %in% c(2021, 2022), state != "United States") %>%
      group_by(state, year, month) %>%
      summarize(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
      arrange(state, year, month)
    return(data_cases)
    
  } else if (type == "hosp") {
    # sum new hospitalizations by month + year + state.
    data_hosp <- df %>%
      select(jurisdiction, collection_date, new_covid_19_hospital) %>%
      mutate(
        collection_date = as.Date(collection_date),
        month = month(collection_date),
        year  = year(collection_date),
        hosp  = as.numeric(new_covid_19_hospital)
      ) %>%
      filter(
        year %in% c(2021, 2022), 
        jurisdiction != "United States",
        !grepl("Region", jurisdiction) # removing region-level data
      ) %>%
      group_by(jurisdiction, year, month) %>%
      summarize(hosp = sum(hosp, na.rm = TRUE), .groups = "drop") %>%
      arrange(jurisdiction, year, month)
    return(data_hosp)
    
  } else if (type == "deaths") {
    # sum reported deaths by month + year + state.
    # MMWR week is converted to a month by approximating weeks->month.
    data_deaths <- df %>%
      select(state, year, mmwr_week, covid_19_deaths) %>%
      mutate(
        # If year has something like "2021/2022" convert to 2022
        year = case_when(
          str_detect(year, '/') ~ str_extract(year, "(?<=/)[0-9]+"),
          TRUE ~ year
        ),
        year      = as.numeric(year),
        mmwr_week = as.numeric(mmwr_week),
        deaths    = as.numeric(covid_19_deaths),
        # Convert >12 month to 12
        month     = ceiling(mmwr_week * 12 / 52),
        # Anything above 12 is set to 12
        month     = ifelse(month > 12, 12, month)
      ) %>%
      filter(year %in% c(2021, 2022), state != "United States") %>%
      group_by(state, year, month) %>%
      summarize(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
      arrange(state, year, month)
    return(data_deaths)
    
  } else if (type == "vax") {
    # sum daily counts by month + year + location (state).
    data_vax <- df %>%
      filter(date_type == "Admin") %>%
      select(location, date, series_complete_daily, booster_daily) %>%
      mutate(
        date    = as.Date(date),
        month   = month(date),
        year    = year(date),
        series  = as.numeric(series_complete_daily),
        booster = as.numeric(booster_daily),
        # If it's NA make it zero
        series  = ifelse(is.na(series), 0, series),
        booster = ifelse(is.na(booster), 0, booster)
      ) %>%
      filter(
        year %in% c(2021, 2022),
        !location %in% c("United States", "US")
      ) %>%
      group_by(location, year, month) %>%
      summarize(
        series  = sum(series, na.rm = TRUE),
        booster = sum(booster, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(location, year, month)
    return(data_vax)
  }
}
# clean each dataset with the function
cases_clean  <- clean_to_monthly(cases_raw,  "cases")
hosp_clean   <- clean_to_monthly(hosp_raw,   "hosp")
deaths_clean <- clean_to_monthly(deaths_raw, "deaths")
vax_clean    <- clean_to_monthly(vax_raw,    "vax")

# Create a template that has rows for each combination of (month, year, state). Deal with missing data in some dates.
monthly_template <- expand.grid(
  month = 1:12,
  year  = 2021:2022,
  stringsAsFactors = FALSE
) %>%
  # I used 15th as a placeholder date in each month
  mutate(date = as.Date(paste(year, month, 15, sep = "-"))) %>%
  arrange(year, month) %>%
  # get all states
  crossing(data.frame(state = unique(population$state))) %>%
  # merge with the fake population data
  left_join(population, by = c("state", "year")) %>%
  arrange(state, year, month)

# merge all cleaned data into the template
merged_data <- monthly_template %>%
  left_join(cases_clean, 
            by = c("state", "year", "month")) %>%
  left_join(hosp_clean, 
            by = c("state" = "jurisdiction", "year", "month")) %>%
  left_join(deaths_clean, 
            by = c("state_name" = "state", "year", "month")) %>%
  left_join(vax_clean, 
            by = c("state" = "location", "year", "month"))

# Set all NA to 0
merged_data <- merged_data %>%
  mutate(
    cases   = ifelse(is.na(cases),   0, cases),
    hosp    = ifelse(is.na(hosp),    0, hosp),
    deaths  = ifelse(is.na(deaths),  0, deaths),
    series  = ifelse(is.na(series),  0, series),
    booster = ifelse(is.na(booster), 0, booster)
  )
# add some extra columns that we might use, can delete if not needed
# Caculate rates
# Track cumulative vaccinations
# Add lags
merged_data <- merged_data %>%
  mutate(
    death_rate = (deaths / pop) * 100000,
    case_rate  = (cases  / pop) * 100000,
    hosp_rate  = (hosp   / pop) * 100000
  ) %>%
  group_by(state) %>%
  arrange(state, year, month) %>%
  mutate(
    # total of fully vaccinated and boosted
    series_total  = cumsum(series),
    booster_total = cumsum(booster),
    # Convert to rate
    vax_percent     = (series_total / pop) * 100,
    booster_percent = (booster_total / pop) * 100,
    # Some lag variables: data from 1 month ago
    prev_cases      = lag(cases, 1, default = 0),
    prev_hosp       = lag(hosp, 1, default = 0),
    prev_deaths     = lag(deaths, 1, default = 0),
    prev_death_rate = lag(death_rate, 1, default = 0),
    # Lags from 3 months ago
    prev3_cases  = lag(cases, 3, default = 0),
    prev3_hosp   = lag(hosp, 3, default = 0),
    prev3_deaths = lag(deaths, 3, default = 0),

    # Can use for predict future death rate
    next_death_rate = lead(death_rate, 1)
  ) %>%
  ungroup()
write.csv(merged_data, "merged_covid_data.csv", row.names = FALSE)
saveRDS(merged_data, "merged_covid_data.rda")