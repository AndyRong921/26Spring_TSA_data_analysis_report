#!/usr/bin/env Rscript
# Data preprocessing and exploratory figures for aquatic product time series.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(readr)
})

template <- normalizePath(getwd(), mustWork = FALSE)
if (!dir.exists(file.path(template, "code"))) template <- normalizePath(file.path(getwd(), "latex-template"), mustWork = FALSE)
root <- normalizePath(file.path(template, ".."), mustWork = FALSE)
data_path <- file.path(root, "data", "青岛水产品销售额零售额.xlsx")
output_dir <- file.path(template, "code", "output")
fig_dir <- file.path(template, "texfile", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)


raw <- read_excel(data_path, col_names = FALSE)
df <- raw[-c(1:3), 1:5]
names(df) <- c("period", "sales_cumulative", "sales_growth", "retail_cumulative", "retail_growth")
df <- df %>%
  mutate(across(everything(), as.numeric),
         period = as.integer(period),
         date = ymd(paste0(period, "01")),
         year = year(date),
         month = month(date)) %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(sales_monthly = sales_cumulative - lag(sales_cumulative),
         retail_monthly = retail_cumulative - lag(retail_cumulative),
         sales_monthly = if_else(row_number() == 1, sales_cumulative, sales_monthly),
         retail_monthly = if_else(row_number() == 1, retail_cumulative, retail_monthly)) %>%
  ungroup()

write_csv(df, file.path(output_dir, "processed_aquatic_timeseries_r.csv"))
summary_tbl <- df %>%
  summarise(across(c(sales_cumulative, retail_cumulative, sales_monthly, retail_monthly,
                     sales_growth, retail_growth),
                   list(count = ~sum(!is.na(.x)), mean = mean, sd = sd, min = min, max = max),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value")
write_csv(summary_tbl, file.path(output_dir, "descriptive_statistics_r.csv"))

yearly <- df %>% group_by(year) %>%
  summarise(sales_cumulative = last(sales_cumulative), retail_cumulative = last(retail_cumulative), .groups = "drop") %>%
  mutate(sales_yoy_pct = 100 * (sales_cumulative / lag(sales_cumulative) - 1),
         retail_yoy_pct = 100 * (retail_cumulative / lag(retail_cumulative) - 1))
write_csv(yearly, file.path(output_dir, "year_end_summary_r.csv"))

p1 <- ggplot(df, aes(date)) +
  geom_line(aes(y = sales_cumulative, colour = "Sales cumulative"), linewidth = 0.8) +
  geom_point(aes(y = sales_cumulative, colour = "Sales cumulative")) +
  geom_line(aes(y = retail_cumulative, colour = "Retail cumulative"), linewidth = 0.8) +
  geom_point(aes(y = retail_cumulative, colour = "Retail cumulative")) +
  labs(title = "Cumulative Aquatic Product Series", x = "Date", y = "Billion yuan", colour = "Series") +
  theme_minimal()
ggsave(file.path(fig_dir, "fig_series_overview_r.pdf"), p1, width = 10, height = 5)

long <- df %>% select(year, month, sales_monthly, retail_monthly) %>%
  pivot_longer(c(sales_monthly, retail_monthly), names_to = "series", values_to = "value") %>%
  mutate(series = recode(series, sales_monthly = "Sales", retail_monthly = "Retail"))
p2 <- ggplot(long, aes(factor(month), value, fill = series)) +
  geom_boxplot(alpha = 0.75) +
  labs(title = "Monthly Contribution by Month", x = "Month", y = "Billion yuan", fill = "Series") +
  theme_minimal()
ggsave(file.path(fig_dir, "fig_box_monthly_r.pdf"), p2, width = 10, height = 5)

p3 <- ggplot(long, aes(series, value, fill = series)) +
  geom_violin(alpha = 0.7) + geom_boxplot(width = 0.12, fill = "white") +
  labs(title = "Distribution of Monthly Contribution", x = "Series", y = "Billion yuan") +
  theme_minimal() + theme(legend.position = "none")
ggsave(file.path(fig_dir, "fig_violin_monthly_r.pdf"), p3, width = 7, height = 5)

message("R preprocessing outputs saved.")
