#!/usr/bin/env Rscript
# SARIMA/ETS model selection, parameter estimation, and forecasting.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forecast)
  library(jsonlite)
})

template <- normalizePath(getwd(), mustWork = FALSE)
if (!dir.exists(file.path(template, "code"))) template <- normalizePath(file.path(getwd(), "latex-template"), mustWork = FALSE)
output_dir <- file.path(template, "code", "output")
fig_dir <- file.path(template, "texfile", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(file.path(output_dir, "processed_aquatic_timeseries.csv"))) {
  source(file.path(template, "code", "01_data_preprocess_exploration.R"))
}
df <- read_csv(file.path(output_dir, "processed_aquatic_timeseries.csv"), show_col_types = FALSE)

metric_values <- function(actual, pred) {
  actual <- as.numeric(actual); pred <- as.numeric(pred)
  ok <- is.finite(actual) & is.finite(pred) & abs(actual) > 1e-9
  tibble(test_rmse = sqrt(mean((actual[ok] - pred[ok])^2)),
         test_mae = mean(abs(actual[ok] - pred[ok])),
         test_mape = mean(abs((actual[ok] - pred[ok]) / actual[ok])) * 100)
}

fit_grid <- function(y, label, season = 11) {
  train <- head(y, length(y) - 11)
  test <- tail(y, 11)
  rows <- list()
  k <- 1
  for (p in 0:2) for (d in 0:1) for (q in 0:2) {
    for (P in 0:1) for (D in 0:1) for (Q in 0:1) {
      if (p + d + q + P + D + Q > 5) next
      fit <- try(Arima(train, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = season), method = "ML"), silent = TRUE)
      if (inherits(fit, "try-error")) next
      pred <- forecast(fit, h = length(test))$mean
      m <- metric_values(test, pred)
      rows[[k]] <- tibble(series = label, model = "SARIMA",
                          order = paste0("(", p, ", ", d, ", ", q, ")"),
                          seasonal_order = paste0("(", P, ", ", D, ", ", Q, ", ", season, ")"),
                          aic = AIC(fit), bic = BIC(fit),
                          test_rmse = m$test_rmse, test_mae = m$test_mae, test_mape = m$test_mape)
      k <- k + 1
    }
  }
  bind_rows(rows) %>% arrange(test_mape, aic)
}

ets_candidate <- function(y, label) {
  train <- head(y, length(y) - 11); test <- tail(y, 11)
  fit <- ets(ts(train, frequency = 11))
  pred <- forecast(fit, h = length(test))$mean
  m <- metric_values(test, pred)
  tibble(series = label, model = "ETS", order = fit$method, seasonal_order = "auto",
         aic = fit$aic, bic = fit$bic, test_rmse = m$test_rmse,
         test_mae = m$test_mae, test_mape = m$test_mape)
}

final_specs <- list(
  Sales = list(order = c(0, 1, 0), seasonal = c(1, 0, 1), period = 11),
  Retail = list(order = c(0, 0, 3), seasonal = c(0, 0, 2), period = 11)
)
series_list <- list(Sales = df$sales_cumulative, Retail = df$retail_cumulative)
all_candidates <- list(); selected <- list(); forecasts <- list(); params <- list()

for (label in names(series_list)) {
  y <- as.numeric(series_list[[label]])
  cand <- bind_rows(fit_grid(y, label, 11), ets_candidate(y, label))
  spec <- final_specs[[label]]
  train <- head(y, length(y) - 11); test <- tail(y, 11)
  train_fit <- Arima(train, order = spec$order, seasonal = list(order = spec$seasonal, period = spec$period), method = "ML")
  pred <- forecast(train_fit, h = length(test))$mean
  m <- metric_values(test, pred)
  chosen <- tibble(series = label, model = "SARIMA",
                   order = paste0("(", paste(spec$order, collapse = ", "), ")"),
                   seasonal_order = paste0("(", paste(c(spec$seasonal, spec$period), collapse = ", "), ")"),
                   aic = AIC(train_fit), bic = BIC(train_fit),
                   test_rmse = m$test_rmse, test_mae = m$test_mae, test_mape = m$test_mape)
  selected[[label]] <- chosen
  all_candidates[[label]] <- bind_rows(cand, chosen)
  fit <- Arima(y, order = spec$order, seasonal = list(order = spec$seasonal, period = spec$period), method = "ML")
  saveRDS(fit, file.path(output_dir, paste0("sarima_", tolower(label), "_result.rds")))
  fc <- forecast(fit, h = 11, level = 95)
  forecasts[[label]] <- tibble(series = label, period = sprintf("2024-%02d", 2:12),
                               forecast = as.numeric(fc$mean),
                               lower_95 = as.numeric(fc$lower[, 1]), upper_95 = as.numeric(fc$upper[, 1]))
  coef_tbl <- tibble(series = label, parameter = names(coef(fit)), estimate = as.numeric(coef(fit)))
  params[[label]] <- coef_tbl
  plot_df <- tibble(index = seq_along(y), observed = y, fitted = as.numeric(fitted(fit)))
  future_df <- tibble(index = (length(y) + 1):(length(y) + 11), forecast = as.numeric(fc$mean),
                      lower_95 = as.numeric(fc$lower[, 1]), upper_95 = as.numeric(fc$upper[, 1]))
  p <- ggplot(plot_df, aes(index)) + geom_line(aes(y = observed, colour = "Observed")) +
    geom_line(aes(y = fitted, colour = "Fitted")) +
    geom_line(data = future_df, aes(y = forecast, colour = "Forecast")) +
    geom_ribbon(data = future_df, aes(ymin = lower_95, ymax = upper_95), inherit.aes = FALSE, alpha = 0.2) +
    labs(title = paste("Observed, Fitted and Forecast Values of", label), x = "Index", y = "Billion yuan", colour = "Series") + theme_minimal()
  ggsave(file.path(fig_dir, paste0("fig_forecast_", tolower(label), "_r.pdf")), p, width = 10, height = 5)
}

write_csv(bind_rows(all_candidates), file.path(output_dir, "model_candidates_r.csv"))
write_csv(bind_rows(selected), file.path(output_dir, "selected_model_metrics_r.csv"))
write_csv(bind_rows(params), file.path(output_dir, "sarima_parameter_estimates_r.csv"))
write_csv(bind_rows(forecasts), file.path(output_dir, "forecast_2024_r.csv"))
write_json(lapply(selected, as.list), file.path(output_dir, "model_summary_r.json"), pretty = TRUE, auto_unbox = TRUE)

plot_top <- bind_rows(all_candidates) %>% group_by(series) %>% arrange(test_mape, .by_group = TRUE) %>% slice_head(n = 10) %>%
  mutate(spec = paste(model, order, seasonal_order))
p <- ggplot(plot_top, aes(reorder(spec, test_mape), test_mape)) + geom_col(fill = "steelblue") + coord_flip() +
  facet_wrap(~series, scales = "free_y") + labs(title = "Top Candidate Models", x = "Model specification", y = "Test MAPE (%)") + theme_minimal()
ggsave(file.path(fig_dir, "fig_model_selection_mape_r.pdf"), p, width = 12, height = 6)

message("R model selection outputs saved.")
