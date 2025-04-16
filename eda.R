# Read the data
final_data <- read_csv("/Users/kellyli/Downloads/final_data.csv")

# Divide the pandemic period into waves
# Trend plot 
final_data |>
  pivot_longer(cols = c(Cases, New_Hosp, Covid_Deaths),
               names_to = "Outcome", values_to = "Number") |>
  mutate(rate = (Number/Population)*100000) |>
  ggplot(aes(x = date, y = rate, color = Region)) +
  geom_line(alpha = 0.6) +
  labs(title = "Trend plot for cases, hospitalizations and deaths for each state", 
       x = "Date", y = "Rates per 100,000 people") +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 1)

# Deaths rate by state for each period