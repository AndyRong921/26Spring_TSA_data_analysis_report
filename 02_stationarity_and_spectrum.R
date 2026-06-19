#!/usr/bin/env Rscript
# Stationarity tests, transformations, ACF/PACF, decomposition, and spectrum plots.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(tseries)
  library(forecast)
})

template <- normalizePath(file.path(getwd()), mustWork = FALSE)
if (!dir.exists(file.path(template, "code"))) template <- normalizePath(file.path(getwd(), "latex-template"), mustWork = FALSE)
output_dir <- file.path(template, "code", "output")
fig_dir <- file.path(template, "texfile", "figures")
if (!file.exists(file.path(output_dir, "processed_aquatic_timeseries.csv"))) {
  source(file.path(template, "code", "01_data_preprocess_exploration.R"))
}
df <- read_csv(file.path(output_dir, "processed_aquatic_timeseries.csv"), show_col_types = FALSE)

stationarity_one <- function(x, name) {
  x <- na.omit(as.numeric(x))
  adf <- adf.test(x)
  kps <- kpss.test(x, null = "Level")
  tibble(series = name,
         adf_stat = unname(adf$statistic), adf_pvalue = adf$p.value,
         kpss_stat = unname(kps$statistic), kpss_pvalue = kps$p.value)
}

tests <- bind_rows(
  stationarity_one(df$sales_cumulative, "Sales cumulative"),
  stationarity_one(df$retail_cumulative, "Retail cumulative"),
  stationarity_one(diff(log(df$sales_cumulative)), "Diff log sales"),
  stationarity_one(diff(log(df$retail_cumulative)), "Diff log retail"),
  stationarity_one(df$sales_monthly, "Sales monthly"),
  stationarity_one(df$retail_monthly, "Retail monthly")
)
write_csv(tests, file.path(output_dir, "stationarity_tests_r.csv"))

plot_decomp <- function(x, label, file) {
  tsx <- ts(as.numeric(x), frequency = 11)
  pdf(file.path(fig_dir, file), width = 9, height = 7)
  plot(stl(tsx, s.window = "periodic"), main = paste("Seasonal Decomposition of", label, "Cumulative Series"))
  dev.off()
}
plot_decomp(df$sales_cumulative, "Sales", "fig_decomposition_sales_r.pdf")
plot_decomp(df$retail_cumulative, "Retail", "fig_decomposition_retail_r.pdf")

plot_acf_pacf <- function(x, label, file) {
  z <- diff(log(as.numeric(x)))
  pdf(file.path(fig_dir, file), width = 10, height = 4)
  par(mfrow = c(1, 2))
  Acf(z, main = paste("ACF of Differenced Log", label))
  Pacf(z, main = paste("PACF of Differenced Log", label))
  dev.off()
}
plot_acf_pacf(df$sales_cumulative, "Sales", "fig_acf_pacf_sales_r.pdf")
plot_acf_pacf(df$retail_cumulative, "Retail", "fig_acf_pacf_retail_r.pdf")

trans <- tibble(index = seq_len(nrow(df)),
                log_sales = log(df$sales_cumulative),
                dlog_sales = c(NA, diff(log(df$sales_cumulative))),
                log_retail = log(df$retail_cumulative),
                dlog_retail = c(NA, diff(log(df$retail_cumulative)))) %>%
  pivot_longer(-index, names_to = "series", values_to = "value")
p <- ggplot(trans, aes(index, value)) + geom_line() + facet_wrap(~series, scales = "free_y", ncol = 2) +
  labs(title = "Log Transformation and First Difference", x = "Index", y = "Value") + theme_minimal()
ggsave(file.path(fig_dir, "fig_transformations_r.pdf"), p, width = 10, height = 6)

plot_spec <- function(x, label) {
  sp <- spec.pgram(diff(log(as.numeric(x))), plot = FALSE)
  tibble(freq = sp$freq, spec = sp$spec, series = label)
}
spec_tbl <- bind_rows(plot_spec(df$sales_cumulative, "Sales"), plot_spec(df$retail_cumulative, "Retail"))
p <- ggplot(spec_tbl, aes(freq, spec)) + geom_line() + geom_point(size = 1) + facet_wrap(~series, scales = "free_y") +
  labs(title = "Periodogram of Log Difference", x = "Frequency", y = "Power") + theme_minimal()
ggsave(file.path(fig_dir, "fig_periodogram_r.pdf"), p, width = 10, height = 4)

message("R stationarity outputs saved.")
