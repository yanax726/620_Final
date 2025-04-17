library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(randomForest)
library(xgboost)
library(ggplot2)
library(lubridate)
library(patchwork)
data_file    <- "~/Desktop/620_Final/final_data.csv"
plots_folder <- "~/Desktop/620_Final/visualizations"
df <- read.csv(data_file, stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

df <- df %>%
  mutate(
    cases100k  = Cases        / Population * 1e5,
    deaths100k = Covid_Deaths / Population * 1e5,
    hosp100k   = New_Hosp     / Population * 1e5,
    vacc_pct   = replace_na(Series_Complete_Pct,   0),
    boost_pct  = replace_na(Bivalent_Booster_Pct,  0)
  )

# y and x
features <- c("cases100k", "hosp100k", "vacc_pct", "boost_pct", "ICU_Hosp", "Booster")
target   <- "deaths100k"

# Aggregate by month and by week
monthly <- df %>%
  group_by(State, year = MMWR_Year, month = MMWR_Month) %>%
  summarize(across(all_of(c(features, target)), mean, na.rm = TRUE),
            .groups = "drop")

weekly <- df %>%
  group_by(State, year = MMWR_Year, week = MMWR_Week) %>%
  summarize(across(all_of(c(features, target)), mean, na.rm = TRUE),
            .groups = "drop")

# Remove features with no variance or perfect collinearity 
## If not use this function there is error keep popping up
clean_features <- function(data) {
  zeros <- nearZeroVar(data[features], saveMetrics = FALSE)
  keep  <- features[-zeros]
  combos <- findLinearCombos(data[keep])
  if (!is.null(combos$remove)) keep <- keep[-combos$remove]
  keep
}

# Compute performance metrics
compute_metrics <- function(actual, pred) {
  rmse <- sqrt(mean((actual - pred)^2))
  mae  <- mean(abs(actual - pred))
  mape <- mean(abs((actual - pred)/actual), na.rm = TRUE) * 100
  r2   <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  data.frame(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2)
}

# Model functions return predictions on test data
models <- list(
  LM = function(tr, te) {
    cols <- clean_features(tr)
    fit  <- lm(reformulate(cols, target), tr)
    predict(fit, te)
  },
  KNN = function(tr, te) {
    cols <- clean_features(tr)
    fit  <- train(reformulate(cols, target), tr,
                  method     = "knn",
                  preProcess = c("center","scale"),
                  trControl  = trainControl("cv", 5),
                  tuneLength = 7)
    predict(fit, te)
  },
  RF = function(tr, te) {
    cols <- clean_features(tr)
    fit  <- randomForest(reformulate(cols, target), tr, ntree = 50)
    predict(fit, te)
  },
  XGB = function(tr, te) {
    cols   <- clean_features(tr)
    dtr    <- xgb.DMatrix(as.matrix(tr[cols]), label = tr[[target]])
    fit    <- xgboost(data = dtr,
                      objective = "reg:squarederror",
                      nrounds   = 50, verbose = 0)
    predict(fit, as.matrix(te[cols]))
  }
)

# Run models on monthly & weekly, collect metrics
results <- bind_rows(lapply(c("Monthly", "Weekly"), function(freq) {
  data_sum <- if (freq == "Monthly") monthly else weekly
  train_df <- filter(data_sum, year == 2021) %>% na.omit()
  test_df  <- filter(data_sum, year == 2022) %>% na.omit()
  bind_rows(lapply(names(models), function(m) {
    preds <- models[[m]](train_df, test_df)
    mets  <- compute_metrics(test_df[[target]], preds)
    mets$Model       <- m
    mets$Frequency   <- freq
    mets
  }))
}))
print(results)

# Plot R² by model and frequency
r2_plot <- results %>%
  ggplot(aes(Model, R2, fill = Frequency)) +
  geom_col(position = "dodge") +
  labs(title = "R² by Model and Frequency", y = expression(R^2)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave(file.path(plots_folder, "r2_comparison.png"),
       r2_plot, width = 6, height = 4)

# Plot error boxplots
error_data <- bind_rows(lapply(c("Monthly","Weekly"), function(freq) {
  data_sum <- if (freq == "Monthly") monthly else weekly
  train_df <- filter(data_sum, year == 2021) %>% na.omit()
  test_df  <- filter(data_sum, year == 2022) %>% na.omit()
  preds    <- map_dfc(models, ~ .x(train_df, test_df))
  data.frame(Frequency = freq,
             Actual    = test_df[[target]],
             preds) %>%
    pivot_longer(-c(Frequency, Actual),
                 names_to = "Model", values_to = "Pred") %>%
    mutate(Error = abs(Pred - Actual))
}))
error_plot <- error_data %>%
  ggplot(aes(Model, Error, fill = Frequency)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7) +
  facet_wrap(~Frequency) +
  labs(title = "Absolute Error by Model", y = "Error") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave(file.path(plots_folder, "error_boxplots.png"),
       error_plot, width = 7, height = 5)

# Smooth time series for all models
train_m <- filter(monthly, year == 2021) %>% na.omit()
test_m  <- filter(monthly, year == 2022) %>% na.omit()
preds_df <- data.frame(
  Month  = as.Date(paste0(test_m$year, "-", sprintf("%02d", test_m$month), "-01")),
  Actual = test_m[[target]],
  LM     = models$LM(train_m, test_m),
  KNN    = models$KNN(train_m, test_m),
  RF     = models$RF(train_m, test_m),
  XGB    = models$XGB(train_m, test_m)
)
avg_df <- preds_df %>%
  group_by(Month) %>%
  summarize(
    Actual = mean(Actual),
    LM     = mean(LM),
    KNN    = mean(KNN),
    RF     = mean(RF),
    XGB    = mean(XGB),
    .groups = "drop"
  )
smooth_df <- pivot_longer(avg_df, -Month, names_to = "Series", values_to = "Value")
line_plot <- ggplot(smooth_df, aes(Month, Value, color = Series)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(
    "Actual" = "black",
    "LM"     = "steelblue",
    "KNN"    = "darkgreen",
    "RF"     = "purple",
    "XGB"    = "tomato"
  )) +
  labs(title = "Monthly: Actual vs Predictions",
       x = "Month", y = "Deaths per 100k") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(file.path(plots_folder, "timeseries_vs_pred.png"),
       line_plot, width = 8, height = 4)
print(line_plot)

# Smooth time series for best model
best_model <- results %>%
  filter(Frequency == "Monthly") %>%
  arrange(RMSE) %>%
  pull(Model) %>% .[1]
best_df <- avg_df %>% select(Month, Actual, Predicted = all_of(best_model))
best_plot <- ggplot(best_df, aes(Month)) +
  geom_line(aes(y = Actual),    color = "steelblue", size = 1) +
  geom_line(aes(y = Predicted), color = "tomato",     size = 1, linetype = "dashed") +
  labs(title = paste(best_model, "Monthly vs Actual"),
       x = "Month", y = "Deaths per 100k") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(plots_folder, paste0("timeseries_", best_model, ".png")),
       best_plot, width = 8, height = 4)
print(best_plot)

# Scatter and residual density
pm <- preds_df %>%
  select(-Month) %>%
  pivot_longer(-Actual, names_to = "Model", values_to = "Pred") %>%
  mutate(Residual = Pred - Actual)
metrics_lbl <- pm %>%
  group_by(Model) %>%
  summarise(R2   = round(cor(Actual, Pred)^2, 2),
            RMSE = round(sqrt(mean(Residual^2)), 2))
scatter_plot <- ggplot(pm, aes(Actual, Pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed") +
  facet_wrap(~Model) +
  geom_text(data = metrics_lbl,
            aes(x = Inf, y = -Inf, label = paste0("R²=", R2, "\nRMSE=", RMSE)),
            hjust = 1.1, vjust = -0.1, size = 3) +
  labs(title = "Monthly Actual vs Predicted") +
  theme_minimal(base_size = 14)
resid_plot <- ggplot(pm, aes(Residual, fill = Model)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Model, scales = "free") +
  labs(title = "Residual Density", x = "Predicted - Actual") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
combo <- scatter_plot + resid_plot + plot_layout(ncol = 1, heights = c(2, 1))
ggsave(file.path(plots_folder, "scatter_residual.png"), combo, width = 6, height = 8)
print(combo)