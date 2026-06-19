#!/usr/bin/env Rscript
# Residual diagnostics for selected SARIMA models.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forecast)
})

template <- normalizePath(getwd(), mustWork = FALSE)
if (!dir.exists(file.path(template, "code"))) template <- normalizePath(file.path(getwd(), "latex-template"), mustWork = FALSE)
output_dir <- file.path(template, "code", "output")
fig_dir <- file.path(template, "texfile", "figures")
if (!file.exists(file.path(output_dir, "sarima_sales_result.rds"))) {
  source(file.path(template, "code", "03_model_selection_forecast.R"))
}
df <- read_csv(file.path(output_dir, "processed_aquatic_timeseries.csv"), show_col_types = FALSE)

ljung_rows <- list()
for (label in c("Sales", "Retail")) {
  fit <- readRDS(file.path(output_dir, paste0("sarima_", tolower(label), "_result.rds")))
  resid <- residuals(fit)
  resid <- resid[(max(2, min(6, floor(length(resid) / 10) + 1))):length(resid)]
  diag_df <- tibble(index = seq_along(resid), residual = as.numeric(resid))
  p1 <- ggplot(diag_df, aes(index, residual)) + geom_line() + geom_point() + geom_hline(yintercept = 0) +
    labs(title = paste("Residual Time Plot of", label), x = "Index", y = "Residual") + theme_minimal()
  p2 <- ggplot(diag_df, aes(sample = residual)) + stat_qq() + stat_qq_line() +
    labs(title = paste("QQ Plot of", label, "Residuals"), x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_minimal()
  ggsave(file.path(fig_dir, paste0("fig_residual_time_", tolower(label), "_r.pdf")), p1, width = 8, height = 4)
  ggsave(file.path(fig_dir, paste0("fig_qq_", tolower(label), "_r.pdf")), p2, width = 6, height = 5)
  pdf(file.path(fig_dir, paste0("fig_residual_acf_", tolower(label), "_r.pdf")), width = 7, height = 4)
  Acf(resid, main = paste("Residual ACF of", label))
  dev.off()
  lags <- 1:min(24, floor(length(resid) / 2))
  lb <- lapply(lags, function(h) {
    bt <- Box.test(resid, lag = h, type = "Ljung-Box", fitdf = length(coef(fit)))
    tibble(series = label, lag = h, lb_stat = unname(bt$statistic), lb_pvalue = bt$p.value)
  }) %>% bind_rows()
  ljung_rows[[label]] <- lb
  p <- ggplot(lb, aes(lag, lb_pvalue)) + geom_line() + geom_point() + geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
    ylim(0, 1) + labs(title = paste("Ljung-Box Test P-values of", label, "Residuals"), x = "Lag", y = "P-value") + theme_minimal()
  ggsave(file.path(fig_dir, paste0("fig_ljung_box_", tolower(label), "_r.pdf")), p, width = 8, height = 4)
}
write_csv(bind_rows(ljung_rows), file.path(output_dir, "ljung_box_tests_r.csv"))

metrics <- read_csv(file.path(output_dir, "selected_model_metrics.csv"), show_col_types = FALSE) %>%
  pivot_longer(c(test_rmse, test_mae, test_mape), names_to = "metric", values_to = "value")
p <- ggplot(metrics, aes(metric, value, fill = series)) + geom_col(position = "dodge") +
  labs(title = "Forecast Error Metrics of Selected Models", x = "Metric", y = "Value", fill = "Series") + theme_minimal()
ggsave(file.path(fig_dir, "fig_error_metrics_r.pdf"), p, width = 8, height = 5)

message("R diagnostics outputs saved.")
