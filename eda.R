final_data <- read_csv("/Users/kellyli/Downloads/final_data.csv")

final_data |>
  # use pivot_longer to reshape the data
  pivot_longer(cols = c(Cases, New_Hosp, Covid_Deaths),
               names_to = "Outcome", values_to = "Number") |>
  # calculate the rates per 100,000 people
  mutate(rate = (Number/Population)*100000) |>
  # plot the trend plot and color by region
  ggplot(aes(x = date, y = rate, color = Region)) +
  geom_line(alpha = 0.6) +
  labs(title = "Trend plot for cases, hospitalizations and deaths for each state", x = "Date", y = "Rates per 100,000 people") +
  # place the plots on top of each other
  facet_wrap(~ Outcome, scales = "free_y", ncol = 1)