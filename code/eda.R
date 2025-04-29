library(readr)
library(tidyverse)
library(ggplot2)

# Read the data
final_data <- read_csv("/Users/kellyli/Downloads/final_data.csv")

# Divide the pandemic period into waves
## Trend plot 
final_data |>
  pivot_longer(cols = c(Cases, New_Hosp, Covid_Deaths),
               names_to = "Outcome", values_to = "Number") |>
  mutate(rate = (Number/Population)*100000) |>
  ggplot(aes(x = date, y = rate, color = Region_Name)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.15, linewidth = 0.5) +
  #geom_line(alpha = 0.6) +
  labs(title = "Trend plot for Cases, Hospitalizations and Deaths by Region", 
       x = "Date", y = "Rates per 100,000 people") +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 1)

# Deaths rate by state for each period
final_data |>
  ggplot(aes(x = date, y = Covid_Death_Rate, color = Region_Name)) +
  #geom_line(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess", span = 0.15, linewidth = 0.5) +
  facet_wrap(~ MMWR_Year, scales = "free", ncol = 1) +
  labs(title = "COVID-19 Death Rate Over Time by Region",
       x = "Date",
       y = "Deaths per 100,000 People",
       color = "State")

# If COVID-19 became less or more virulent across the different periods

# Estimate excess mortality for each week for each state
baseline_deaths <- final_data |>
  filter(date < as.Date("2020-03-07")) |> 
  group_by(State, MMWR_Week) |>
  summarise(Expected_Deaths = mean(Total_Deaths, na.rm = TRUE), 
            .groups = "drop")

state_avg <- final_data |>
  filter(date < as.Date("2020-03-07")) |>
  group_by(State) |>
  summarise(Fallback_Expected = mean(Total_Deaths, na.rm = TRUE), 
            .groups = "drop")

# Merge both
final_data_with_excess <- final_data |>
  left_join(baseline_deaths, by = c("State", "MMWR_Week")) |>
  left_join(state_avg, by = "State") |>
  mutate(Expected_Deaths = ifelse(is.na(Expected_Deaths), 
                                  Fallback_Expected, Expected_Deaths),
         Excess_Deaths = round(Total_Deaths - Expected_Deaths)) |>
  select(date, State, MMWR_Year, MMWR_Week, Total_Deaths, Covid_Deaths, 
         Expected_Deaths, Excess_Deaths)

# Correlation Heatmap
library(reshape2)
cor_matrix <- cor(final_data[, c("Cases", "New_Hosp", "ICU_Hosp", 
                                 "Covid_Death_Rate", "Series_Complete_Pct", 
                                 "Booster", "Bivalent_Booster_Pct")], 
                  use = "complete.obs")
cor_melted <- melt(cor_matrix)
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.x = element_blank(),  # Hide x-axis title
        axis.title.y = element_blank(),  # Hide y-axis title
        axis.ticks = element_blank()) 

# Map of Death Rate by State
library(usmap)
states_map <- map_data("state")
final_data |>
  group_by(State_Full) |>
  summarise(Average_Death_Rate = mean(Covid_Death_Rate, na.rm = TRUE), 
            .groups = "drop") |>
  mutate(region = tolower(State_Full)) |>
  ggplot() +
  geom_map(data = states_map, map = states_map, 
           aes(x = long, y = lat, map_id = region),
           fill = "white", color = "black", linewidth = 0.1) +
  geom_map(aes(map_id = region, fill = Average_Death_Rate),
           map = states_map, color = "black", linewidth = 0.1) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = "grey90") +
  labs(title = "    Average COVID-19 Death Rate by State",
       fill = "Deaths Rate per 100k    ") +
  theme_void()
