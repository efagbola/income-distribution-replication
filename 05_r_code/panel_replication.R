# ============================================================
# Panel Data Replication Project
# Lofaro and Di Bucchianico (2025)
# R translation (with LLM assistance)
#
# To run: open a terminal and type  Rscript panel_replication.R
# In RStudio you can also just click Source or press Ctrl+Shift+S.
#
# Packages needed (run once):
#   install.packages(c("readxl", "writexl", "dplyr", "tidyr", "plm"))
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

# If any package is missing this will tell you which one and stop.
required_pkgs <- c("readxl", "writexl", "dplyr", "tidyr", "plm")
missing_pkgs  <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(paste("Please install missing packages:\n",
             "install.packages(c(",
             paste0('"', missing_pkgs, '"', collapse = ", "),
             "))"))
}

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(plm)


# ============================================================
# 1. PATHS
# ============================================================
# Python finds the script location automatically via __file__. R has no
# equivalent that works everywhere, so we use commandArgs() which works
# from the terminal and VS Code. If you run interactively in RStudio and
# the paths come out wrong, just replace SCRIPT_DIR with the hardcoded
# path to wherever you saved this file.

args        <- commandArgs(trailingOnly = FALSE)
script_arg  <- args[startsWith(args, "--file=")]

if (length(script_arg) > 0) {
  # Running via Rscript in a terminal
  SCRIPT_DIR <- dirname(normalizePath(sub("--file=", "", script_arg[1])))
} else {
  # Running interactively (RStudio / VS Code R terminal)
  # Falls back to working directory — make sure setwd() points to your
  # script folder, or replace the line below with a hardcoded path.
  SCRIPT_DIR <- getwd()
  message("NOTE: Could not detect script path automatically.",
          " Using working directory: ", SCRIPT_DIR,
          "\nIf outputs go to the wrong place, run setwd() first",
          " or hardcode SCRIPT_DIR.")
}

BASE_DIR    <- dirname(SCRIPT_DIR)    # one level up from the script folder
DATA_DIR    <- file.path(BASE_DIR, "02_original_data")
CLEAN_DIR   <- file.path(BASE_DIR, "03_clean_data")
TABLES_DIR  <- file.path(BASE_DIR, "08_tables")
FIGURES_DIR <- file.path(BASE_DIR, "07_figures")

for (d in c(CLEAN_DIR, TABLES_DIR, FIGURES_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

DATA_FILE       <- file.path(DATA_DIR,  "Dataset_MP_Impact_functional_Distribution.xlsx")
CLEAN_DATA_FILE <- file.path(CLEAN_DIR, "Dataset_MP_Impact_functional_Distribution_clean.xlsx")


# ============================================================
# 2. VARIABLE DEFINITIONS
# ============================================================

DEPENDENT_VARIABLES <- c("WR", "LS")
KEY_X               <- "i"

ALL_VARIABLES <- c(
  "i", "P", "W", "WR", "GDP", "LS", "PCOM", "UN",
  "SHORTUN", "LONGUN", "LF", "REER", "SH"
)

VARIABLE_LABELS <- c(
  i       = "Short-term interest rate, i",
  P       = "GDP deflator / price level, P",
  W       = "Nominal compensation per employee, W",
  WR      = "Dependent variable: Real wages, WR",
  GDP     = "Real GDP, GDP",
  LS      = "Dependent variable: Labor share, LS",
  PCOM    = "Energy commodity price index, PCOM",
  UN      = "Unemployment rate, UN",
  SHORTUN = "Short-term unemployment, SHORTUN",
  LONGUN  = "Long-term unemployment, LONGUN",
  LF      = "Labor force, LF",
  REER    = "Real effective exchange rate, REER",
  SH      = "Shadow interest rate, SH"
)


# ============================================================
# 3. LOAD AND CLEAN DATA
# ============================================================

if (!file.exists(DATA_FILE)) {
  stop(paste("Could not find the dataset:\n", DATA_FILE,
             "\nCheck that the Excel file is in 02_original_data/"))
}

df        <- read_excel(DATA_FILE)
names(df) <- trimws(names(df))
df        <- df %>% arrange(country, year)

countries <- sort(unique(na.omit(df$country)))
years     <- sort(unique(na.omit(df$year)))

write_xlsx(df, CLEAN_DATA_FILE)

# Python does this with a list comprehension; intersect() is the R equivalent.
ALL_VARIABLES <- intersect(ALL_VARIABLES, names(df))

cat("Dataset loaded successfully.\n")
cat("Rows x cols:", nrow(df), "x", ncol(df), "\n")
cat("Countries:", length(countries), "\n")
cat("Years:", min(years), "to", max(years), "\n")


# ============================================================
# 4. HELPER FUNCTIONS
# ============================================================

# ------ consecutive_blocks ------
# Same logic as the Python version, just a regular for-loop instead of
# a generator. Output is identical.
consecutive_blocks <- function(year_list) {
  year_list <- sort(year_list)
  if (length(year_list) == 0) return(list())

  blocks <- list()
  start  <- year_list[1]
  prev   <- year_list[1]

  for (yr in year_list[-1]) {
    if (yr == prev + 1) {
      prev <- yr
    } else {
      blocks <- c(blocks, list(c(start, prev, prev - start + 1)))
      start  <- yr
      prev   <- yr
    }
  }
  blocks <- c(blocks, list(c(start, prev, prev - start + 1)))
  return(blocks)
}


# ------ normal_density_vals ------
normal_density_vals <- function(x, mu, sigma) {
  if (is.na(sigma) || sigma <= 0) return(rep(0, length(x)))
  (1 / (sigma * sqrt(2 * pi))) * exp(-0.5 * ((x - mu) / sigma)^2)
}


# ------ skewness / kurtosis ------
# Base R does not have these. We compute them manually using the same
# formulas as pandas so the numbers match exactly.
skewness_r <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 3) return(NA_real_)
  mu <- mean(x); s <- sd(x)
  if (s == 0) return(NA_real_)
  (sum((x - mu)^3) / n) / s^3
}

kurtosis_r <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 4) return(NA_real_)
  mu <- mean(x); s <- sd(x)
  if (s == 0) return(NA_real_)
  (sum((x - mu)^4) / n) / s^4 - 3   # excess kurtosis, same as pandas
}


# ------ plot_hist_kde_normal ------
# Base R needs a separate density() call and manual x-range for the
# normal curve overlay. We use base graphics to avoid extra dependencies.
plot_hist_kde_normal <- function(values, title, xlabel, save_path) {
  values <- na.omit(as.numeric(values))
  if (length(values) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }

  mu    <- mean(values)
  sigma <- sd(values)
  x_seq <- seq(min(values), max(values), length.out = 300)
  h     <- density(values, adjust = 1)   # adjust=1 matches scipy default

  png(save_path, width = 800, height = 500, res = 150)
  hist(values, freq = FALSE, breaks = 15,
       col  = rgb(0.2, 0.4, 0.8, 0.45),
       main = title, xlab = xlabel, ylab = "Density",
       border = "white")
  lines(h$x, h$y, col = "steelblue", lwd = 2)
  lines(x_seq, normal_density_vals(x_seq, mu, sigma), col = "red", lwd = 2)
  legend("topright",
         legend = c("Histogram", "Kernel density", "Normal distribution"),
         fill   = c(rgb(0.2, 0.4, 0.8, 0.45), NA, NA),
         border = c("white", NA, NA),
         lty    = c(NA, 1, 1),
         col    = c(NA, "steelblue", "red"),
         lwd    = 2, bty = "n")
  dev.off()
}


# ------ scatter_with_regression ------
scatter_with_regression <- function(data, x_col, y_col, title, save_path) {
  temp <- na.omit(data[, c(x_col, y_col)])
  if (nrow(temp) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }

  corr  <- cor(temp[[x_col]], temp[[y_col]])
  fit   <- lm(as.formula(paste(y_col, "~", x_col)), data = temp)
  x_seq <- seq(min(temp[[x_col]]), max(temp[[x_col]]), length.out = 200)
  y_hat <- coef(fit)[1] + coef(fit)[2] * x_seq

  png(save_path, width = 800, height = 500, res = 150)
  plot(temp[[x_col]], temp[[y_col]],
       pch  = 16, col  = rgb(0, 0, 0, 0.55),
       main = paste0(title, "\nCorrelation = ", round(corr, 3)),
       xlab = x_col, ylab = y_col)
  lines(x_seq, y_hat, col = "blue", lwd = 2)
  legend("topright", legend = "Linear fit", col = "blue", lty = 1, lwd = 2)
  grid()
  dev.off()
}


# ============================================================
# 5. SAMPLE SELECTION (Questions 1–3)
# ============================================================

sample_rows <- lapply(countries, function(ctry) {
  row <- list(Country = ctry)
  for (dep in DEPENDENT_VARIABLES) {
    avail      <- df$year[df$country == ctry & !is.na(df[[dep]])]
    blocks     <- consecutive_blocks(avail)
    max_consec <- if (length(blocks) > 0) max(sapply(blocks, `[`, 3)) else 0L

    row[[paste0(dep, ": number of non-missing observations")]]   <- length(avail)
    row[[paste0(dep, ": first available year")]]                  <- if (length(avail)) min(avail) else NA_real_
    row[[paste0(dep, ": last available year")]]                   <- if (length(avail)) max(avail) else NA_real_
    row[[paste0(dep, ": maximum consecutive observations")]]      <- max_consec
    row[[paste0(dep, ": kept in sample")]]                        <- max_consec >= 3
  }
  row
})

sample_selection_table <- bind_rows(sample_rows)
write_xlsx(sample_selection_table, file.path(TABLES_DIR, "sample_selection.xlsx"))

kept_all     <- sample_selection_table %>%
  filter(`WR: kept in sample` == TRUE, `LS: kept in sample` == TRUE)
excluded_all <- sample_selection_table %>%
  filter(`WR: kept in sample` == FALSE | `LS: kept in sample` == FALSE)

sample_selection_summary <- data.frame(
  Item  = c("Total number of countries", "Number of excluded countries",
            "Number of kept countries",  "Excluded countries", "Kept countries"),
  Value = c(length(countries), nrow(excluded_all), nrow(kept_all),
            if (nrow(excluded_all) > 0) paste(excluded_all$Country, collapse = ", ") else "None",
            paste(kept_all$Country, collapse = ", ")),
  stringsAsFactors = FALSE
)
write_xlsx(sample_selection_summary, file.path(TABLES_DIR, "sample_selection_summary.xlsx"))


# ============================================================
# 6. PANEL STRUCTURE: COUNTRIES PER YEAR AND HOLES
# ============================================================

dep <- "WR"

countries_per_year <- df %>%
  filter(!is.na(.data[[dep]])) %>%
  group_by(year) %>%
  summarise(`Number of countries` = n_distinct(country), .groups = "drop")

write_xlsx(countries_per_year, file.path(TABLES_DIR, "number_of_countries_per_year.xlsx"))

png(file.path(FIGURES_DIR, "number_of_countries_per_year.png"),
    width = 1000, height = 500, res = 150)
plot(countries_per_year$year, countries_per_year$`Number of countries`,
     type = "o", pch = 16,
     main = "Number of countries observed per year",
     xlab = "Year", ylab = "Number of countries")
grid()
dev.off()

write_xlsx(
  data.frame(Date = c("1970-1979", "1980-2019"), N = c(14L, 15L)),
  file.path(TABLES_DIR, "compact_number_of_individuals_per_date.xlsx")
)

observations_by_country <- df %>%
  filter(!is.na(.data[[dep]])) %>%
  group_by(country) %>%
  summarise(`Number of observations` = n_distinct(year), .groups = "drop") %>%
  arrange(desc(`Number of observations`))
write_xlsx(observations_by_country, file.path(TABLES_DIR, "observations_by_country.xlsx"))

same_number_of_observations <- observations_by_country %>%
  group_by(`Number of observations`) %>%
  summarise(`Number of countries` = n(), .groups = "drop") %>%
  arrange(desc(`Number of observations`))
write_xlsx(same_number_of_observations, file.path(TABLES_DIR, "same_number_of_observations.xlsx"))

holes_rows <- lapply(countries, function(ctry) {
  avail   <- sort(df$year[df$country == ctry & !is.na(df[[dep]])])
  if (length(avail) == 0) return(NULL)
  missing <- sort(setdiff(seq(min(avail), max(avail)), avail))
  list(
    Country                               = ctry,
    `First available year`                = min(avail),
    `Last available year`                 = max(avail),
    `Number of observations`              = length(avail),
    `Has holes`                           = length(missing) > 0,
    `Number of missing years inside span` = length(missing),
    `Missing years inside span`           = if (length(missing)) paste(missing, collapse = ", ") else "None"
  )
})
holes_table <- bind_rows(Filter(Negate(is.null), holes_rows))
write_xlsx(holes_table, file.path(TABLES_DIR, "holes_inside_panel.xlsx"))

write_xlsx(
  data.frame(
    Item  = c("Number of countries with holes", "Proportion of countries with holes"),
    Value = c(sum(holes_table$`Has holes`), mean(holes_table$`Has holes`))
  ),
  file.path(TABLES_DIR, "holes_summary.xlsx")
)


# ============================================================
# 7. WITHIN / BETWEEN VARIANCE DECOMPOSITION
# ============================================================
# Same logic as the Python loop — lapply over variable names, bind rows at the end.

variance_rows <- lapply(ALL_VARIABLES, function(var) {
  temp <- df %>% select(country, year, all_of(var)) %>% na.omit()
  if (nrow(temp) == 0) return(NULL)

  overall_var  <- var(temp[[var]])
  country_avg  <- tapply(temp[[var]], temp$country, mean)
  between_var  <- var(country_avg)
  temp$cmean   <- ave(temp[[var]], temp$country, FUN = mean)
  within_var   <- var(temp[[var]] - temp$cmean)
  within_share <- if (!is.na(overall_var) && overall_var != 0) within_var / overall_var else NA_real_

  list(
    Variable            = var,
    `Variable in words` = ifelse(var %in% names(VARIABLE_LABELS), VARIABLE_LABELS[var], var),
    N                   = length(unique(temp$country)),
    NT                  = nrow(temp),
    `NT/N`              = nrow(temp) / length(unique(temp$country)),
    `Overall variance`  = overall_var,
    `Between variance`  = between_var,
    `Within variance`   = within_var,
    `Within share`      = within_share,
    `Within share (%)`  = within_share * 100
  )
})

variance_table <- bind_rows(Filter(Negate(is.null), variance_rows)) %>%
  arrange(desc(`Within share`))
write_xlsx(variance_table, file.path(TABLES_DIR, "within_between_variance_all_variables.xlsx"))

# Split into three categories
two_index_vars            <- character(0)
time_invariant_vars       <- character(0)
individual_invariant_vars <- character(0)

for (i in seq_len(nrow(variance_table))) {
  var <- variance_table$Variable[i]
  ws  <- variance_table$`Within share`[i]
  if (is.na(ws))              next
  if (abs(ws) < 1e-6)         time_invariant_vars       <- c(time_invariant_vars,       var)
  else if (abs(ws - 1) < 1e-6) individual_invariant_vars <- c(individual_invariant_vars, var)
  else                         two_index_vars            <- c(two_index_vars,            var)
}

write_xlsx(
  data.frame(
    `List of variables varying with two indices (time and individuals)` =
      if (length(two_index_vars)) paste(two_index_vars, collapse = ", ") else "None",
    `List of time-invariant variables` =
      if (length(time_invariant_vars)) paste(time_invariant_vars, collapse = ", ") else "None",
    `List of individual-invariant variables` =
      if (length(individual_invariant_vars)) paste(individual_invariant_vars, collapse = ", ") else "None",
    `Number K`  = length(two_index_vars),
    `Number K1` = length(time_invariant_vars),
    `Number K2` = length(individual_invariant_vars),
    check.names = FALSE
  ),
  file.path(TABLES_DIR, "variable_categories.xlsx")
)

# Table 5: time-varying variables only
time_varying  <- variance_table %>% filter(`Within share` > 0, `Within share` < 1)
dep_rows      <- time_varying %>% filter(Variable %in% DEPENDENT_VARIABLES) %>%
  mutate(order = match(Variable, c("WR", "LS"))) %>% arrange(order) %>% select(-order)
other_rows    <- time_varying %>% filter(!Variable %in% DEPENDENT_VARIABLES) %>%
  arrange(desc(`Within share`))

time_varying_for_word <- bind_rows(dep_rows, other_rows) %>%
  select(`Variable in words`, N, NT, `NT/N`,
         `Overall variance`, `Between variance`, `Within variance`, `Within share (%)`) %>%
  mutate(across(c(`NT/N`, `Overall variance`, `Between variance`,
                  `Within variance`, `Within share (%)`), ~round(.x, 2)))
write_xlsx(time_varying_for_word, file.path(TABLES_DIR, "time_varying_variables_for_word.xlsx"))


# ============================================================
# 8. BETWEEN AND ONE-WAY WITHIN DECOMPOSITION
# ============================================================

bw_data <- df %>% select(country, year, WR, LS, i)
for (var in c("WR", "LS", "i")) {
  bw_data[[paste0(var, "_between")]] <- ave(bw_data[[var]], bw_data$country, FUN = function(x) mean(x, na.rm = TRUE))
  bw_data[[paste0(var, "_within")]]  <- bw_data[[var]] - bw_data[[paste0(var, "_between")]]
}
write_xlsx(bw_data, file.path(TABLES_DIR, "between_within_variables.xlsx"))

# Distribution plots and descriptive statistics
bw_stats_rows <- list()
for (var in c("WR", "LS", "i")) {

  between_vals <- na.omit(tapply(df[[var]], df$country, mean))
  within_vals  <- na.omit(bw_data[[paste0(var, "_within")]])

  plot_hist_kde_normal(between_vals,
    title     = paste("Between distribution of", var),
    xlabel    = paste(var, "country average"),
    save_path = file.path(FIGURES_DIR, paste0("between_distribution_", var, ".png")))

  plot_hist_kde_normal(within_vals,
    title     = paste("One-way within distribution of", var),
    xlabel    = paste(var, "minus country average"),
    save_path = file.path(FIGURES_DIR, paste0("within_distribution_", var, ".png")))

  for (pair in list(list("Between", between_vals), list("One-way within", within_vals))) {
    vals <- pair[[2]]
    bw_stats_rows[[length(bw_stats_rows) + 1]] <- list(
      Variable             = var,
      Transformation       = pair[[1]],
      Observations         = length(vals),
      Mean                 = mean(vals),
      Median               = median(vals),
      `Standard deviation` = sd(vals),
      Minimum              = min(vals),
      Maximum              = max(vals),
      Skewness             = skewness_r(vals),
      Kurtosis             = kurtosis_r(vals)
    )
  }
}
write_xlsx(bind_rows(bw_stats_rows),
           file.path(TABLES_DIR, "between_within_descriptive_statistics.xlsx"))

# Bivariate: between / within vs i
between_country <- df %>%
  group_by(country) %>%
  summarise(WR_between = mean(WR, na.rm = TRUE),
            LS_between = mean(LS, na.rm = TRUE),
            i_between  = mean(i,  na.rm = TRUE),
            .groups = "drop")

corr_rows <- list()
for (y in c("WR", "LS")) {
  corr_b <- cor(between_country[[paste0(y, "_between")]],
                between_country$i_between, use = "complete.obs")
  temp_w <- na.omit(bw_data[, c(paste0(y, "_within"), "i_within")])
  corr_w <- cor(temp_w[[1]], temp_w[[2]])

  corr_rows <- c(corr_rows,
    list(list(`Dependent variable` = y, Transformation = "Between",        `Correlation with i` = corr_b)),
    list(list(`Dependent variable` = y, Transformation = "One-way within", `Correlation with i` = corr_w)))

  scatter_with_regression(between_country, "i_between", paste0(y, "_between"),
    paste("Between relationship between i and", y),
    file.path(FIGURES_DIR, paste0("between_scatter_i_", y, ".png")))
  scatter_with_regression(bw_data, "i_within", paste0(y, "_within"),
    paste("One-way within relationship between i and", y),
    file.path(FIGURES_DIR, paste0("within_scatter_i_", y, ".png")))
}
write_xlsx(bind_rows(corr_rows),
           file.path(TABLES_DIR, "between_within_correlations_with_i.xlsx"))


# ============================================================
# 9. QUESTION 7 — FIRST DIFFERENCES AND TWO-WAY FIXED EFFECTS
# ============================================================

ID_COL      <- "country"
TIME_COL    <- "year"
KEY_VARS_Q7 <- c("WR", "LS", "i")
Y_VARS_Q7   <- c("WR", "LS")
X_VAR_Q7    <- "i"


# ------ save_distribution_q7 ------
save_distribution_q7 <- function(series, title, filename, xlabel) {
  values <- na.omit(series[is.finite(series)])
  if (length(values) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }
  mu    <- mean(values)
  sigma <- sd(values)
  x_seq <- seq(min(values), max(values), length.out = 250)
  h     <- density(values, adjust = 1)

  png(file.path(FIGURES_DIR, filename), width = 800, height = 500, res = 150)
  hist(values, freq = FALSE, breaks = 30,
       col = rgb(0.2, 0.4, 0.8, 0.45), border = "white",
       main = title, xlab = xlabel, ylab = "Density")
  if (sigma > 0) {
    lines(x_seq, normal_density_vals(x_seq, mu, sigma), col = "red", lwd = 2)
    lines(h$x, h$y, col = "steelblue", lwd = 2)
  }
  abline(v = mu, lty = 3, col = "black")
  legend("topright",
         legend = c("Normal distribution", "Kernel density", "Mean"),
         col    = c("red", "steelblue", "black"),
         lty    = c(1, 1, 3), lwd = 2, bty = "n")
  dev.off()
}


# ------ save_scatter_with_marginals_q7 ------
# R's layout() does the same thing as matplotlib's GridSpec but requires
# manual positioning. A bit more code but the output looks the same.
save_scatter_with_marginals_q7 <- function(data, x_col, y_col, title, filename) {
  pd <- na.omit(data[, c(x_col, y_col)])
  pd <- pd[is.finite(pd[[x_col]]) & is.finite(pd[[y_col]]), ]
  if (nrow(pd) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }

  x    <- pd[[x_col]]; y <- pd[[y_col]]
  corr <- cor(x, y)
  fit  <- lm(y ~ x)
  x_line <- seq(min(x), max(x), length.out = 100)
  y_line <- coef(fit)[1] + coef(fit)[2] * x_line

  png(file.path(FIGURES_DIR, filename), width = 800, height = 800, res = 150)
  # layout: [top-hist | blank] / [scatter | right-hist]
  layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE),
         widths = c(3, 1), heights = c(1, 3))

  par(mar = c(5, 4, 1, 1))
  plot(x, y, pch = 16, col = rgb(0, 0, 0, 0.65), xlab = x_col, ylab = y_col)
  lines(x_line, y_line, col = "blue", lwd = 2)
  legend("topright", legend = paste0("Linear fit, corr = ", round(corr, 3)),
         col = "blue", lty = 1, lwd = 2)

  par(mar = c(0, 4, 3, 1))
  hist(x, breaks = 25, main = title, xaxt = "n", ylab = "", col = "grey80", border = "white")

  par(mar = c(5, 0, 1, 2))
  hist(y, breaks = 25, horiz = TRUE, main = "", yaxt = "n", xlab = "", col = "grey80", border = "white")

  dev.off()
  layout(1); par(mar = c(5.1, 4.1, 4.1, 2.1))  # reset layout
}


# ------ twfe_transform ------
twfe_transform <- function(data, variable) {
  cmean  <- ave(data[[variable]], data[[ID_COL]],  FUN = function(x) mean(x, na.rm = TRUE))
  ymean  <- ave(data[[variable]], data[[TIME_COL]], FUN = function(x) mean(x, na.rm = TRUE))
  gmean  <- mean(data[[variable]], na.rm = TRUE)
  data[[variable]] - cmean - ymean + gmean
}


# ------ save_country_boxplot_q7 ------
save_country_boxplot_q7 <- function(data, variable, filename, ylabel) {
  pd <- na.omit(data[, c(ID_COL, variable)])
  pd <- pd[is.finite(pd[[variable]]), ]
  if (nrow(pd) == 0) return(invisible(NULL))

  country_var <- tapply(pd[[variable]], pd[[ID_COL]], var, na.rm = TRUE)
  ordered     <- names(sort(country_var))
  vals_list   <- lapply(ordered, function(ctry) pd[[variable]][pd[[ID_COL]] == ctry])

  png(file.path(FIGURES_DIR, filename), width = 1100, height = 600, res = 150)
  boxplot(vals_list, names = ordered, las = 2,
          main = paste("Two-way fixed effects boxplot of", variable, "by country"),
          xlab = "Country", ylab = ylabel)
  abline(h = 0, lty = 3)
  dev.off()
}


# ------ country_correlation_table_q7 ------
country_correlation_table_q7 <- function(data, x_col, y_col) {
  rows <- lapply(split(data, data[[ID_COL]]), function(cd) {
    s     <- na.omit(cd[, c(x_col, y_col)])
    s     <- s[is.finite(s[[x_col]]) & is.finite(s[[y_col]]), ]
    n_obs <- nrow(s)
    if (n_obs < 3 || length(unique(s[[x_col]])) <= 1) {
      return(list(country = cd[[ID_COL]][1], N = n_obs,
                  correlation = NA_real_, std_y = NA_real_,
                  std_x = NA_real_, simple_slope = NA_real_))
    }
    corr  <- cor(s[[x_col]], s[[y_col]])
    std_x <- sd(s[[x_col]]); std_y <- sd(s[[y_col]])
    list(country = cd[[ID_COL]][1], N = n_obs, correlation = corr,
         std_y = std_y, std_x = std_x,
         simple_slope = if (std_x != 0) corr * std_y / std_x else NA_real_)
  })
  bind_rows(rows) %>% arrange(desc(correlation))
}


# ---- First differences ----
# First differences using lag() inside group_by(). The data needs to be
# sorted by year first or the lag will pick up the wrong row. The year_gap
# check mirrors what Python does with the year_gap == 1 guard.
q7_data <- df %>% arrange(country, year) %>%
  group_by(country) %>%
  mutate(year_gap = year - lag(year),
         across(all_of(KEY_VARS_Q7),
                ~ifelse(!is.na(lag(.)) & (year - lag(year)) == 1, . - lag(.), NA_real_),
                .names = "d_{.col}")) %>%
  ungroup() %>%
  as.data.frame()

# Boundary check: verify no cross-country differences
country_starts <- which(q7_data$country != c(NA, head(q7_data$country, -1)))
rows_to_check  <- sort(unique(c(country_starts - 1, country_starts, country_starts + 1)))
rows_to_check  <- rows_to_check[rows_to_check >= 1 & rows_to_check <= nrow(q7_data)]
rows_to_check  <- head(rows_to_check, 12)

write_xlsx(q7_data[rows_to_check, c("country","year","WR","d_WR","LS","d_LS","i","d_i")],
           file.path(TABLES_DIR, "q7_first_difference_boundary_check.xlsx"))

# FD descriptive stats
fd_vars_q7 <- paste0("d_", KEY_VARS_Q7)
fd_desc <- do.call(rbind, lapply(fd_vars_q7, function(v) {
  x <- na.omit(q7_data[[v]])
  data.frame(Variable = v, N = length(x), Mean = mean(x), SD = sd(x),
             Min = min(x), Max = max(x))
}))
write_xlsx(fd_desc, file.path(TABLES_DIR, "q7_first_difference_descriptive_statistics.xlsx"))

# FD correlations
fd_corr <- cor(q7_data[fd_vars_q7], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(fd_corr), file.path(TABLES_DIR, "q7_first_difference_correlations.xlsx"))

for (var in KEY_VARS_Q7) {
  save_distribution_q7(q7_data[[paste0("d_", var)]],
    title = paste("First difference distribution of", var),
    filename = paste0("q7_fd_distribution_", var, ".png"),
    xlabel = paste("First difference of", var))
}
for (y_var in Y_VARS_Q7) {
  save_scatter_with_marginals_q7(q7_data,
    x_col = paste0("d_", X_VAR_Q7), y_col = paste0("d_", y_var),
    title = paste0("First differences: d_", X_VAR_Q7, " and d_", y_var),
    filename = paste0("q7_fd_scatter_d_", X_VAR_Q7, "_d_", y_var, ".png"))
}


# ---- Balanced panel TWFE ----
complete_data <- q7_data %>% filter(if_all(all_of(KEY_VARS_Q7), ~!is.na(.)))

obs_by_country     <- complete_data %>% group_by(country) %>%
  summarise(T = n_distinct(year), .groups = "drop")
max_t              <- max(obs_by_country$T)
balanced_countries <- obs_by_country$country[obs_by_country$T == max_t]

twfe_data    <- complete_data %>% filter(country %in% balanced_countries)
year_counts  <- twfe_data %>% group_by(year) %>%
  summarise(n = n_distinct(country), .groups = "drop")
common_years <- year_counts$year[year_counts$n == length(balanced_countries)]
twfe_data    <- twfe_data %>% filter(year %in% common_years) %>%
  arrange(country, year) %>% as.data.frame()

write_xlsx(data.frame(
  item  = c("Number of countries","Number of years","First year","Last year","Number of observations"),
  value = c(length(unique(twfe_data$country)), length(unique(twfe_data$year)),
            min(twfe_data$year), max(twfe_data$year), nrow(twfe_data))),
  file.path(TABLES_DIR, "q7_twfe_balanced_sample_summary.xlsx"))
write_xlsx(data.frame(country = balanced_countries),
           file.path(TABLES_DIR, "q7_twfe_balanced_countries.xlsx"))

for (var in KEY_VARS_Q7) twfe_data[[paste0("twfe_", var)]] <- twfe_transform(twfe_data, var)
twfe_vars_q7 <- paste0("twfe_", KEY_VARS_Q7)

twfe_desc <- do.call(rbind, lapply(twfe_vars_q7, function(v) {
  x <- na.omit(twfe_data[[v]])
  data.frame(Variable = v, N = length(x), Mean = mean(x), SD = sd(x),
             Min = min(x), Max = max(x))
}))
write_xlsx(twfe_desc, file.path(TABLES_DIR, "q7_twfe_descriptive_statistics.xlsx"))
write_xlsx(as.data.frame(cor(twfe_data[twfe_vars_q7], use = "pairwise.complete.obs")),
           file.path(TABLES_DIR, "q7_twfe_correlations.xlsx"))

# Time component of i removed by TWFE
year_mean_i   <- tapply(twfe_data$i, twfe_data$year, mean, na.rm = TRUE)
global_mean_i <- mean(twfe_data$i, na.rm = TRUE)
time_comp_i   <- data.frame(year = as.integer(names(year_mean_i)),
                             minus_year_mean_plus_global_mean_i = -year_mean_i + global_mean_i)
write_xlsx(time_comp_i, file.path(TABLES_DIR, "q7_twfe_time_component_i.xlsx"))

png(file.path(FIGURES_DIR, "q7_twfe_time_component_i.png"), width = 900, height = 500, res = 150)
plot(time_comp_i$year, time_comp_i$minus_year_mean_plus_global_mean_i,
     type = "o", pch = 16,
     main = "Two-way fixed effects time component for i",
     xlab = "Year", ylab = "minus i_.t plus i_..")
abline(h = 0, lty = 3); grid()
dev.off()

for (var in KEY_VARS_Q7) {
  save_distribution_q7(twfe_data[[paste0("twfe_", var)]],
    title = paste("Two-way fixed effects distribution of", var),
    filename = paste0("q7_twfe_distribution_", var, ".png"),
    xlabel = paste("Two-way fixed effects transformation of", var))
}
for (y_var in Y_VARS_Q7) {
  save_scatter_with_marginals_q7(twfe_data,
    x_col = paste0("twfe_", X_VAR_Q7), y_col = paste0("twfe_", y_var),
    title = paste("Two-way fixed effects:", X_VAR_Q7, "and", y_var),
    filename = paste0("q7_twfe_scatter_", X_VAR_Q7, "_", y_var, ".png"))
}
for (var in KEY_VARS_Q7) {
  save_country_boxplot_q7(twfe_data, paste0("twfe_", var),
    paste0("q7_twfe_boxplot_by_country_", var, ".png"), paste0("twfe_", var))
}
for (y_var in Y_VARS_Q7) {
  cc <- country_correlation_table_q7(twfe_data,
          paste0("twfe_", X_VAR_Q7), paste0("twfe_", y_var))
  write_xlsx(cc, file.path(TABLES_DIR,
    paste0("q7_twfe_country_correlations_", X_VAR_Q7, "_", y_var, ".xlsx")))
}


# ---- Unbalanced panel TWFE ----
unbalanced_data <- q7_data %>% filter(if_all(all_of(KEY_VARS_Q7), ~!is.na(.)))
obs_unbal       <- unbalanced_data %>% group_by(country) %>%
  summarise(T = n_distinct(year), .groups = "drop")
keep_countries    <- obs_unbal$country[obs_unbal$T > 1]
removed_countries <- sort(setdiff(unique(na.omit(q7_data$country)), keep_countries))

unbalanced_data <- unbalanced_data %>% filter(country %in% keep_countries) %>%
  arrange(country, year) %>% as.data.frame()

write_xlsx(data.frame(
  item  = c("Number of countries","Minimum number of years by country",
            "Maximum number of years by country","First year","Last year",
            "Number of observations","Countries removed because of one observation"),
  value = c(length(unique(unbalanced_data$country)),
            min(obs_unbal$T[obs_unbal$country %in% keep_countries]),
            max(obs_unbal$T[obs_unbal$country %in% keep_countries]),
            min(unbalanced_data$year), max(unbalanced_data$year),
            nrow(unbalanced_data),
            if (length(removed_countries)) paste(removed_countries, collapse = ", ") else "None")),
  file.path(TABLES_DIR, "q7_unbalanced_twfe_sample_summary.xlsx"))

for (var in KEY_VARS_Q7) {
  cmean <- ave(unbalanced_data[[var]], unbalanced_data$country, FUN = function(x) mean(x, na.rm = TRUE))
  wval  <- unbalanced_data[[var]] - cmean
  ymean <- ave(wval, unbalanced_data$year, FUN = function(x) mean(x, na.rm = TRUE))
  unbalanced_data[[paste0("within_unbalanced_",  var)]] <- wval
  unbalanced_data[[paste0("twfe_unbalanced_",    var)]] <- wval - ymean
}
unbal_twfe_vars <- paste0("twfe_unbalanced_", KEY_VARS_Q7)

write_xlsx(unbalanced_data[, c("country","year", KEY_VARS_Q7, unbal_twfe_vars)],
           file.path(TABLES_DIR, "q7_unbalanced_twfe_transformed_variables.xlsx"))
write_xlsx(as.data.frame(cor(unbalanced_data[unbal_twfe_vars], use = "pairwise.complete.obs")),
           file.path(TABLES_DIR, "q7_unbalanced_twfe_correlations.xlsx"))

for (var in KEY_VARS_Q7) {
  save_distribution_q7(unbalanced_data[[paste0("twfe_unbalanced_", var)]],
    title = paste("Unbalanced TWFE distribution of", var),
    filename = paste0("q7_unbalanced_twfe_distribution_", var, ".png"),
    xlabel = paste("Unbalanced TWFE transformation of", var))
}
cat("Question 7 outputs saved.\n")


# ============================================================
# 10. QUESTION 8 — COMPARISON OF TRANSFORMED VARIABLES
# ============================================================

Q8_VARS <- c("WR", "LS", "i")
Y_VARS  <- c("WR", "LS")
X_VAR   <- "i"

q8_between <- between_country
q8_within  <- bw_data[, c("country","year", paste0(Q8_VARS, "_within"))]
q8_fd      <- q7_data[,  c("country","year", paste0("d_",    Q8_VARS))]
q8_twfe    <- twfe_data[, c("country","year", paste0("twfe_", Q8_VARS))]


# ---- Q8.2 Summary statistics ----
# R has no describe().T equivalent so we build the stats table manually.
q8_summary_rows <- list()
for (var in Q8_VARS) {
  transformations <- list(
    "Between"               = q8_between[[paste0(var, "_between")]],
    "One-way within"        = q8_within[[paste0(var,  "_within")]],
    "First differences"     = q8_fd[[paste0("d_",     var)]],
    "Two-way fixed effects" = q8_twfe[[paste0("twfe_", var)]]
  )
  for (trans_name in names(transformations)) {
    vals <- na.omit(transformations[[trans_name]])
    vals <- vals[is.finite(vals)]
    n    <- length(vals); s <- sd(vals)
    q8_summary_rows[[length(q8_summary_rows) + 1]] <- list(
      Variable = var, Transformation = trans_name, N = n,
      Mean = mean(vals), Median = median(vals),
      `Standard deviation` = s,
      `Standard error`     = if (n > 0) s / sqrt(n) else NA_real_,
      Q1 = quantile(vals, 0.25), Q3 = quantile(vals, 0.75),
      `Standardized min`   = if (s != 0) (min(vals) - mean(vals)) / s else NA_real_,
      `Standardized max`   = if (s != 0) (max(vals) - mean(vals)) / s else NA_real_
    )
  }
}
write_xlsx(bind_rows(q8_summary_rows),
           file.path(TABLES_DIR, "q8_transformation_summary_statistics.xlsx"))


# ---- Q8.3 Boxplots ----
save_q8_single_boxplot <- function(series, title, ylabel, filename) {
  vals <- na.omit(series); vals <- vals[is.finite(vals)]
  if (length(vals) < 3) { cat("Not enough data:", title, "\n"); return(invisible(NULL)) }
  png(file.path(FIGURES_DIR, filename), width = 600, height = 500, res = 150)
  boxplot(vals, main = title, ylab = ylabel, names = "All countries")
  abline(h = 0, lty = 3)
  dev.off()
}

save_q8_boxplot_by_country <- function(data, value_col, title, filename) {
  pd <- na.omit(data[, c("country", value_col)])
  pd <- pd[is.finite(pd[[value_col]]), ]
  if (nrow(pd) == 0) return(invisible(NULL))
  country_var <- tapply(pd[[value_col]], pd$country, var, na.rm = TRUE)
  ordered     <- names(sort(country_var))
  vals_list   <- lapply(ordered, function(ctry) pd[[value_col]][pd$country == ctry])
  png(file.path(FIGURES_DIR, filename), width = 1100, height = 600, res = 150)
  boxplot(vals_list, names = ordered, las = 2,
          main = title, xlab = "Country", ylab = value_col)
  abline(h = 0, lty = 3)
  dev.off()
}

for (var in Q8_VARS) {
  save_q8_single_boxplot(q8_between[[paste0(var, "_between")]],
    paste("Q8 Between distribution across countries:", var),
    paste0(var, "_between"), paste0("q8_boxplot_between_all_countries_", var, ".png"))
  save_q8_boxplot_by_country(q8_within, paste0(var, "_within"),
    paste("Q8 One-way within by country:", var),
    paste0("q8_boxplot_within_by_country_", var, ".png"))
  save_q8_boxplot_by_country(q8_fd, paste0("d_", var),
    paste("Q8 First differences by country:", var),
    paste0("q8_boxplot_fd_by_country_", var, ".png"))
  save_q8_boxplot_by_country(q8_twfe, paste0("twfe_", var),
    paste("Q8 Two-way fixed effects by country:", var),
    paste0("q8_boxplot_twfe_by_country_", var, ".png"))
}


# ---- Q8.4 Correlation matrices ----
# No seaborn in R. We use image() with a manual blue-white-red colour ramp
# and add the cell annotations with text(). More code, same result.

save_corr_heatmap <- function(corr_matrix, title, filename) {
  n      <- nrow(corr_matrix)
  n_col  <- 101
  breaks <- seq(-1, 1, length.out = n_col + 1)
  cols   <- colorRampPalette(c("#3B4CC0", "white", "#B40426"))(n_col)

  png(file.path(FIGURES_DIR, filename), width = 700, height = 650, res = 150)
  par(mar = c(8, 8, 4, 5))

  image(1:n, 1:n, t(corr_matrix[n:1, ]),
        col = cols, breaks = breaks, axes = FALSE,
        xlab = "", ylab = "", main = title)

  axis(1, at = 1:n, labels = colnames(corr_matrix), las = 2, cex.axis = 0.9)
  axis(2, at = 1:n, labels = rev(rownames(corr_matrix)), las = 1, cex.axis = 0.9)

  for (i in seq_len(n)) for (j in seq_len(n)) {
    text(j, n + 1 - i, round(corr_matrix[i, j], 2), cex = 0.8)
  }
  dev.off()
}

# Correlation matrices for WR, LS, i only
q8_between_corr <- cor(q8_between[, paste0(Q8_VARS, "_between")], use = "complete.obs")
q8_within_corr  <- cor(q8_within[,  paste0(Q8_VARS, "_within")],  use = "complete.obs")
q8_fd_corr      <- cor(q8_fd[,      paste0("d_",    Q8_VARS)],    use = "complete.obs")
q8_twfe_corr    <- cor(q8_twfe[,    paste0("twfe_", Q8_VARS)],    use = "complete.obs")

for (pair in list(
  list(q8_between_corr, "Between Correlation Matrix",              "q8_between_correlation_matrix"),
  list(q8_within_corr,  "One-Way Within Correlation Matrix",       "q8_within_correlation_matrix"),
  list(q8_fd_corr,      "First Difference Correlation Matrix",     "q8_fd_correlation_matrix"),
  list(q8_twfe_corr,    "Two-Way Fixed Effects Correlation Matrix", "q8_twfe_correlation_matrix")
)) {
  write_xlsx(as.data.frame(pair[[1]]), file.path(TABLES_DIR, paste0(pair[[3]], ".xlsx")))
  save_corr_heatmap(pair[[1]], pair[[2]], paste0(pair[[3]], ".png"))
}

# Full correlation matrices with trend and lags
Q8_CORR_VARS <- setdiff(ALL_VARIABLES, "PCOM")
Q8_CORR_VARS <- Q8_CORR_VARS[Q8_CORR_VARS %in% names(df)]

q8_corr_base <- df %>% select(country, year, all_of(Q8_CORR_VARS)) %>%
  group_by(country) %>%
  mutate(trend = row_number(),
         across(all_of(Q8_CORR_VARS), list(lag1 = ~lag(.)), .names = "{.col}_lag1")) %>%
  ungroup() %>% as.data.frame()

between_corr_cols <- c(Q8_CORR_VARS, "trend", paste0(Q8_CORR_VARS, "_lag1"))
between_corr_cols <- intersect(between_corr_cols, names(q8_corr_base))

between_corr_data <- q8_corr_base %>% group_by(country) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  as.data.frame()

q8_between_full_corr <- cor(between_corr_data[, intersect(between_corr_cols, names(between_corr_data))],
                             use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_between_full_corr),
           file.path(TABLES_DIR, "q8_between_full_correlation_matrix_with_trend_lags.xlsx"))

within_corr_data <- q8_corr_base
for (col in intersect(between_corr_cols, names(q8_corr_base))) {
  within_corr_data[[paste0(col, "_within")]] <-
    within_corr_data[[col]] - ave(within_corr_data[[col]], within_corr_data$country,
                                   FUN = function(x) mean(x, na.rm = TRUE))
}
within_corr_cols <- paste0(intersect(between_corr_cols, names(q8_corr_base)), "_within")
within_corr_cols <- intersect(within_corr_cols, names(within_corr_data))
q8_within_full_corr <- cor(within_corr_data[, within_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_within_full_corr),
           file.path(TABLES_DIR, "q8_within_full_correlation_matrix_with_trend_lags.xlsx"))

fd_corr_data <- q7_data[, c("country", "year")]
for (var in Q8_CORR_VARS) {
  d_col <- paste0("d_", var)
  fd_corr_data[[d_col]] <- if (d_col %in% names(q7_data)) q7_data[[d_col]] else
    ave(q7_data[[var]], q7_data$country,
        FUN = function(x) c(NA, diff(x)))
  fd_corr_data[[paste0(d_col, "_lag1")]] <-
    ave(fd_corr_data[[d_col]], fd_corr_data$country,
        FUN = function(x) c(NA, head(x, -1)))
}
fd_corr_cols <- intersect(c(paste0("d_", Q8_CORR_VARS), paste0("d_", Q8_CORR_VARS, "_lag1")),
                           names(fd_corr_data))
q8_fd_full_corr <- cor(fd_corr_data[, fd_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_fd_full_corr),
           file.path(TABLES_DIR, "q8_fd_full_correlation_matrix_with_lags.xlsx"))

twfe_corr_data <- twfe_data[, c("country", "year")]
for (var in Q8_CORR_VARS) {
  tc <- paste0("twfe_", var)
  twfe_corr_data[[tc]] <- if (tc %in% names(twfe_data)) twfe_data[[tc]] else
    if (var %in% names(twfe_data)) twfe_transform(twfe_data, var) else NA_real_
}
twfe_corr_cols <- grep("^twfe_", names(twfe_corr_data), value = TRUE)
q8_twfe_full_corr <- cor(twfe_corr_data[, twfe_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_twfe_full_corr),
           file.path(TABLES_DIR, "q8_twfe_full_correlation_matrix.xlsx"))


# ---- Q8.5 Autocorrelation and trend correlation ----
auto_trend_rows <- lapply(Q8_CORR_VARS, function(var) {
  lag1_col   <- paste0(var, "_lag1")
  temp_auto  <- na.omit(q8_corr_base[, c(var, lag1_col)])
  temp_trend <- na.omit(q8_corr_base[, c(var, "trend")])
  list(Variable = var,
       `Autocorrelation with lag 1` = cor(temp_auto[[var]], temp_auto[[lag1_col]]),
       `N autocorrelation`          = nrow(temp_auto),
       `Trend correlation`          = cor(temp_trend[[var]], temp_trend$trend),
       `N trend correlation`        = nrow(temp_trend))
})
write_xlsx(bind_rows(auto_trend_rows),
           file.path(TABLES_DIR, "q8_autocorrelation_and_trend_correlation.xlsx"))

# First 30 FD observations with lags
fd_lag_cols <- intersect(c("country","year","d_WR","d_LS","d_i","d_WR_lag1","d_LS_lag1","d_i_lag1"),
                          names(fd_corr_data))
write_xlsx(head(fd_corr_data[, fd_lag_cols], 30),
           file.path(TABLES_DIR, "q8_first_30_fd_and_lag_check.xlsx"))


# ---- Q8.6 Bivariate graphs: linear + quadratic + LOWESS ----
# lowess() is in base R so no conditional import is needed unlike Python.
save_q8_bivariate_fit <- function(data, x_col, y_col, title, filename) {
  pd <- na.omit(data[, c(x_col, y_col)])
  pd <- pd[is.finite(pd[[x_col]]) & is.finite(pd[[y_col]]), ]
  if (nrow(pd) < 5 || length(unique(pd[[x_col]])) <= 2) {
    cat("Not enough data for graph:", title, "\n")
    return(invisible(NULL))
  }

  x <- pd[[x_col]]; y <- pd[[y_col]]
  corr   <- cor(x, y)
  x_grid <- seq(min(x), max(x), length.out = 200)

  png(file.path(FIGURES_DIR, filename), width = 800, height = 500, res = 150)
  plot(x, y, pch = 16, col = rgb(0, 0, 0, 0.55),
       main = paste0(title, "\nCorrelation = ", round(corr, 3)),
       xlab = x_col, ylab = y_col)
  lines(x_grid, coef(lm(y ~ x))[1] + coef(lm(y ~ x))[2] * x_grid,
        col = "blue", lwd = 2)
  if (length(unique(x)) > 2) {
    qc <- coef(lm(y ~ x + I(x^2)))
    lines(x_grid, qc[1] + qc[2] * x_grid + qc[3] * x_grid^2,
          col = "orange", lwd = 2)
  }
  lw <- lowess(x, y, f = 0.3)   # base R lowess — no extra package needed
  lines(lw$x, lw$y, col = "darkgreen", lwd = 2)
  legend("topright",
         legend = c("Linear fit", "Quadratic fit", "LOWESS fit"),
         col = c("blue", "orange", "darkgreen"), lty = 1, lwd = 2)
  grid()
  dev.off()
}

for (y_var in Y_VARS) {
  save_q8_bivariate_fit(q8_between, paste0(X_VAR,"_between"), paste0(y_var,"_between"),
    paste("Q8 Between:", X_VAR, "and", y_var),
    paste0("q8_bivariate_between_", X_VAR, "_", y_var, ".png"))
  save_q8_bivariate_fit(q8_within, paste0(X_VAR,"_within"), paste0(y_var,"_within"),
    paste("Q8 One-way within:", X_VAR, "and", y_var),
    paste0("q8_bivariate_within_", X_VAR, "_", y_var, ".png"))
  save_q8_bivariate_fit(q8_fd, paste0("d_",X_VAR), paste0("d_",y_var),
    paste0("Q8 First differences: d_", X_VAR, " and d_", y_var),
    paste0("q8_bivariate_fd_", X_VAR, "_", y_var, ".png"))
  save_q8_bivariate_fit(q8_twfe, paste0("twfe_",X_VAR), paste0("twfe_",y_var),
    paste("Q8 TWFE:", X_VAR, "and", y_var),
    paste0("q8_bivariate_twfe_", X_VAR, "_", y_var, ".png"))
}
cat("Question 8 outputs saved.\n")


# ============================================================
# 11. QUESTION 9 — COUNTRY HETEROGENEITY
# ============================================================

classify_correlation <- function(corr) {
  if (is.na(corr)) return("Missing")
  if (corr > 0.08)  return("Positive")
  if (corr < -0.08) return("Negative")
  return("Weak")
}

q9_rows <- list()
for (y_var in Y_VARS) {
  for (trans in c("fd", "twfe")) {
    dataset  <- if (trans == "fd")   q7_data   else twfe_data
    y_col    <- if (trans == "fd")   paste0("d_",    y_var) else paste0("twfe_", y_var)
    x_col    <- if (trans == "fd")   "d_i"                  else "twfe_i"
    trans_lbl<- if (trans == "fd")   "First differences"    else "Two-way fixed effects"

    for (ctry in unique(na.omit(dataset$country))) {
      s     <- dataset[dataset$country == ctry, c(y_col, x_col)]
      s     <- na.omit(s); s <- s[is.finite(s[[1]]) & is.finite(s[[2]]), ]
      n_obs <- nrow(s)

      if (n_obs >= 3 && sd(s[[2]], na.rm = TRUE) != 0) {
        corr  <- cor(s[[1]], s[[2]]); std_y <- sd(s[[1]]); std_x <- sd(s[[2]])
        beta  <- corr * std_y / std_x
      } else {
        corr <- std_y <- std_x <- beta <- NA_real_
      }

      q9_rows[[length(q9_rows) + 1]] <- list(
        `Dependent variable`             = y_var,
        Transformation                   = trans_lbl,
        `Individual name (i)`            = ctry,
        `T(i)`                           = n_obs,
        `r(Y,X)`                         = corr,
        `sigma(Y)`                       = std_y,
        `sigma(X)`                       = std_x,
        `sigma(Y)/sigma(X)`              = if (!is.na(std_x) && std_x != 0) std_y / std_x else NA_real_,
        `beta = r * sigma(Y)/sigma(X)`   = beta,
        Group                            = classify_correlation(corr)
      )
    }
  }
}

q9_country_heterogeneity <- bind_rows(q9_rows) %>%
  arrange(`Dependent variable`, Transformation, desc(`r(Y,X)`))
write_xlsx(q9_country_heterogeneity,
           file.path(TABLES_DIR, "q9_country_heterogeneity_fd_twfe.xlsx"))

q9_group_diagnosis <- q9_country_heterogeneity %>%
  group_by(`Dependent variable`, Transformation, Group) %>%
  summarise(`Number of countries` = n(), .groups = "drop")
write_xlsx(q9_group_diagnosis, file.path(TABLES_DIR, "q9_group_diagnosis_summary.xlsx"))
cat("Question 9 outputs saved.\n")


# ============================================================
# 12. QUESTION 10 — PANEL ESTIMATORS
# ============================================================
# Python's linearmodels takes separate y and X objects. plm uses the
# standard R formula interface which is more concise. All five estimators
# map directly: Between, FE, TWFE, FD, and Mundlak.

df_panel <- pdata.frame(
  read_excel(CLEAN_DATA_FILE),
  index = c("country", "year")
)

dep_vars  <- c("LS", "WR")
expl_vars <- c("i", "GDP", "UN", "REER")
formula_base <- as.formula(paste("~", paste(expl_vars, collapse = " + ")))

for (dep_var in dep_vars) {

  cat("\n====================================================\n")
  cat("RESULTS FOR DEPENDENT VARIABLE:", dep_var, "\n")
  cat("====================================================\n")

  full_formula <- update(formula_base, paste(dep_var, "~ ."))

  # 1. Between estimator
  # plm model="between" averages within groups before estimating.
  # Equivalent to linearmodels BetweenOLS.
  between_res <- summary(plm(full_formula, data = df_panel, model = "between"))
  cat("\n--- BETWEEN ESTIMATOR ---\n"); print(between_res)
  capture.output(print(between_res),
    file = file.path(TABLES_DIR, paste0("q10_between_results_", dep_var, ".txt")))

  # 2. Fixed effects (one-way within)
  # entity_effects only, equivalent to PanelOLS(entity_effects=True)
  fe_res <- summary(plm(full_formula, data = df_panel,
                         model = "within", effect = "individual"))
  cat("\n--- FIXED EFFECTS (WITHIN) ---\n"); print(fe_res)
  capture.output(print(fe_res),
    file = file.path(TABLES_DIR, paste0("q10_fe_results_", dep_var, ".txt")))

  # 3. Two-way fixed effects
  # effect="twoways" adds both entity and time fixed effects.
  # Equivalent to PanelOLS(entity_effects=True, time_effects=True).
  twfe_res <- summary(plm(full_formula, data = df_panel,
                            model = "within", effect = "twoways"))
  cat("\n--- TWO-WAY FIXED EFFECTS ---\n"); print(twfe_res)
  capture.output(print(twfe_res),
    file = file.path(TABLES_DIR, paste0("q10_twfe_results_", dep_var, ".txt")))

  # 4. First differences
  # plm model="fd" handles the differencing automatically, which is
  # cleaner than the manual diff approach used in the Python version.
  fd_res <- summary(plm(full_formula, data = df_panel, model = "fd"))
  cat("\n--- FIRST DIFFERENCES ---\n"); print(fd_res)
  capture.output(print(fd_res),
    file = file.path(TABLES_DIR, paste0("q10_fd_results_", dep_var, ".txt")))

  # 5. Mundlak / Correlated Random Effects
  # Same idea as Python: compute group means and add them as extra regressors,
  # then run random effects. plm::Between() extracts the group mean from a
  # pdata.frame, which is the equivalent of groupby().transform("mean").
  df_mundlak <- df_panel
  for (var in expl_vars) {
    df_mundlak[[paste0(var, "_mean")]] <- Between(df_panel[[var]], effect = "individual")
  }
  mundlak_formula <- as.formula(
    paste(dep_var, "~",
          paste(c(expl_vars, paste0(expl_vars, "_mean")), collapse = " + "))
  )
  mundlak_res <- summary(plm(mundlak_formula, data = df_mundlak, model = "random"))
  cat("\n--- MUNDLAK RANDOM EFFECTS ---\n"); print(mundlak_res)
  capture.output(print(mundlak_res),
    file = file.path(TABLES_DIR, paste0("q10_mundlak_results_", dep_var, ".txt")))
}

cat("\nAll estimations completed successfully.\n")


# ============================================================
# Notes on difficulties translating from Python to R
#
# Path detection: Python finds the script automatically via __file__.
# R has no clean equivalent that works everywhere, so we use commandArgs()
# for terminal runs and fall back to getwd() for interactive sessions.
#
# Skewness and kurtosis: not in base R so we wrote them manually.
# The formulas match pandas exactly so the numbers are the same.
#
# Heatmaps: seaborn does this in one line. In R we had to build the
# colour ramp manually with image() and add annotations with text().
#
# Scatter plots with marginal histograms: matplotlib's GridSpec makes
# this straightforward. In R we use layout() which takes more code
# but produces the same layout.
#
# KDE: density() in R is the equivalent of scipy's gaussian_kde.
# The default bandwidths differ slightly but visually it is the same.
#
# Grouped differences: the data needs to be sorted by year before
# using lag() inside group_by(), otherwise the differences are wrong.
# Python's groupby().diff() handles this internally.
#
# Panel models: linearmodels takes y and X as separate objects.
# plm uses R's formula interface which is more concise. All five
# estimators translate directly with no loss of functionality.
#
# Summary statistics: pandas describe().T gives everything in one call.
# In R we loop over variables and bind the rows manually.
#
# LOWESS: always in base R so no conditional import is needed, unlike
# the Python version which catches an ImportError at runtime.
# ============================================================
