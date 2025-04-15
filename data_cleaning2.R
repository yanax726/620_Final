# Data derived from United States Census Bureau
# Import raw population data (2020-2024)
population_raw <- read_csv("/Users/kellyli/Downloads/NST-EST2024-ALLDATA.csv")

# Clean the population data
library(tidyverse)
population <- population_raw |>
  filter(NAME %in% c(state.name, "District of Columbia", "Puerto Rico")) |>
  mutate(State_Abb = case_when(NAME == "District of Columbia" ~ "DC",
                               NAME == "Puerto Rico" ~ "PR",
                               TRUE ~ state.abb[match(NAME, state.name)]),
         State = NAME) |>
  select(State, State_Abb, starts_with("POPESTIMATE")) |>
  rename_with(.fn = ~ str_replace(., "POPESTIMATE", "POP_"), 
              .cols = starts_with("POPESTIMATE")) |>
  pivot_longer(cols = starts_with("POP_"), 
               names_to = "Year", 
               values_to = "Population") |>
  mutate(Year = str_remove(Year, "POP_") %>% as.integer()) |>
  select(State, State_Abb, Year, Population)

# Import region data
library(jsonlite)
library(purrr)
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url)
regions <- regions |>
  mutate(Region_Name = case_when(
    region_name == "New York and New Jersey, Puerto Rico, Virgin Islands" ~ 
      "NY,NJ,PR,VI", TRUE ~ region_name)) |>
  mutate(Region_Name = as.factor(Region_Name),
         region = as.factor(as.character(region))) |>
  unnest(states) |>
  rename(State = states)

# Wrangle into a final population data
population2 <- right_join(population, regions, by = "State")
population2 <- population2 |>
  rename(Region = region) |>
  select(-region_name)

# Write a function that helps retrieve data using API
get_cdc_data <- function(api){
  request(api) |>
    req_url_query("$limit" = 10000000) |>
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)}

# Import related data
cases_raw <- get_cdc_data("https://data.cdc.gov/resource/pwn4-m3yp.json")
hosp_raw <- get_cdc_data("https://data.cdc.gov/resource/39z2-9zu6.json")
deaths_raw <- get_cdc_data("https://data.cdc.gov/resource/r8kw-7aab.json")
vax_raw <- get_cdc_data("https://data.cdc.gov/resource/rh2h-3yt2.json")

# Wrangle each dataset
library(lubridate)
## 1) Cases
cases <- cases_raw |>
  mutate(MMWR_Week = epiweek(ymd_hms(start_date)),
         MMWR_Year = epiyear(ymd_hms(start_date)),
         MMWR_Month = month(as.Date(start_date)),
         New_Cases = as.numeric(new_cases)) |>
  group_by(state, MMWR_Year, MMWR_Month, MMWR_Week) |>
  summarise(Cases = sum(New_Cases, na.rm = TRUE), .groups = "drop") |>
  rename(State = state) |>
  select(State, MMWR_Year, MMWR_Month, MMWR_Week, Cases)

## 2) Hospitalization
hosp <- hosp_raw |>
  mutate(MMWR_Week = epiweek(ymd_hms(collection_date)),
         MMWR_Year = epiyear(ymd_hms(collection_date)),
         MMWR_Month = month(as.Date(collection_date))) |>
  group_by(jurisdiction, MMWR_Year, MMWR_Month, MMWR_Week) |>
  summarise(New_Hosp = sum(as.numeric(new_covid_19_hospital), na.rm = TRUE),
            Total_Hosp = sum(as.numeric(total_hospitalized_covid), na.rm = TRUE),
            ICU_Hosp = sum(as.numeric(covid_19_icu_bed_occupancy), na.rm = TRUE),
            .groups = "drop") |>
  rename(State = jurisdiction) |>
  select(State, MMWR_Year, MMWR_Month, MMWR_Week, 
         New_Hosp, Total_Hosp, ICU_Hosp)

## 3) Deaths
deaths <- deaths_raw |>
  mutate(MMWR_Week = as.numeric(mmwr_week),
         MMWR_Year = epiyear(ymd_hms(start_date)),
         MMWR_Month = month(as.Date(start_date)),
         Covid_Deaths = as.numeric(covid_19_deaths)) |>
  rename(State = state) |>
  filter(State != "United States") |>
  select(State, MMWR_Year, MMWR_Month, MMWR_Week, Covid_Deaths)

## 4) Vaccination
vax <- vax_raw |>
  filter(date_type == "Admin") |>
  mutate(MMWR_Week = as.numeric(mmwr_week),
         MMWR_Year = epiyear(ymd_hms(date)),
         MMWR_Month = month(as.Date(date))) |>
  group_by(location, MMWR_Year, MMWR_Month, MMWR_Week) |>
  summarise(Series_Complete = sum(as.numeric(series_complete_daily),
                                  na.rm = TRUE),
            Series_Complete_Pct = mean(as.numeric(series_complete_pop_pct),
                                  na.rm = TRUE),
            Booster = sum(as.numeric(booster_daily), na.rm = TRUE),
            Bivalent_Booster = sum(as.numeric(bivalent_booster_cumulative),
                                     na.rm = TRUE),
            Bivalent_Booster_Pct = mean(as.numeric(bivalent_booster_pop_pct),
                                     na.rm = TRUE), .groups = "drop") |>
  rename(State = location) |>
  select(State, MMWR_Year, MMWR_Month, MMWR_Week, Series_Complete, 
         Series_Complete_Pct, Booster, Bivalent_Booster, Bivalent_Booster_Pct)

# Make dates in population dataset
all_dates <- data.frame(date = seq(make_date(2020, 1, 25),
                                   make_date(2023, 12, 31), 
                                   by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1),
         MMWR_Year = epiyear(date),
         MMWR_Week = epiweek(date),
         MMWR_Month = month(date))

dates_and_pop <- cross_join(all_dates, 
                            data.frame(State = unique(population2$State))) |>
  left_join(population2, by = c("State", "MMWR_Year" = "Year")) |>
  rename(State = State_Abb, State_Full = State)

# Join population data with the others
final_data <- dates_and_pop |>
  left_join(cases, by = c("State", "MMWR_Year", "MMWR_Month", "MMWR_Week")) |>
  left_join(hosp, by = c("State", "MMWR_Year", "MMWR_Month", "MMWR_Week")) |>
  left_join(deaths, by = c("State_Full" = "State", "MMWR_Year", "MMWR_Month", "MMWR_Week")) |>
  left_join(vax, by = c("State", "MMWR_Year", "MMWR_Month", "MMWR_Week")) |>
  mutate(Cases = replace_na(Cases, 0), New_Hosp = replace_na(New_Hosp, 0),
         ICU_Hosp = replace_na(ICU_Hosp, 0), 
         Total_Hosp = replace_na(Total_Hosp, 0), 
         Covid_Deaths = replace_na(Covid_Deaths, 0))

# Export the final data into a csv file
write.csv(final_data, "/Users/kellyli/Downloads/final_data.csv")
