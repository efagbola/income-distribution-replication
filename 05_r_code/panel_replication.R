# ============================================================
# Panel Data Replication Project
# Lofaro and Di Bucchianico (2025)
# R translation
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plm)

# ============================================================
# Paths
# ============================================================

BASE_DIR     <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
DATA_DIR     <- file.path(BASE_DIR, "02_original_data")
CLEAN_DIR    <- file.path(BASE_DIR, "03_clean_data")
TABLES_DIR   <- file.path(BASE_DIR, "08_tables")
FIGURES_DIR  <- file.path(BASE_DIR, "07_figures")

for (d in c(CLEAN_DIR, TABLES_DIR, FIGURES_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

DATA_FILE       <- file.path(DATA_DIR,  "Dataset_MP_Impact_functional_Distribution.xlsx")
CLEAN_DATA_FILE <- file.path(CLEAN_DIR, "Dataset_MP_Impact_functional_Distribution_clean.xlsx")

# ============================================================
# Variable definitions
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
# Load and clean data
# ============================================================

if (!file.exists(DATA_FILE)) {
  stop(paste("Could not find the dataset here:\n", DATA_FILE,
             "\nCheck that the Excel file is saved in the data folder."))
}

df <- read_excel(DATA_FILE)
names(df) <- trimws(names(df))
df <- df %>% arrange(country, year)

countries <- sort(unique(na.omit(df$country)))
years     <- sort(unique(na.omit(df$year)))

write_xlsx(df, CLEAN_DATA_FILE)

ALL_VARIABLES <- ALL_VARIABLES[ALL_VARIABLES %in% names(df)]

cat("Dataset loaded successfully.\n")
cat("Rows and columns:", nrow(df), "x", ncol(df), "\n")
cat("Countries:", length(countries), "\n")
cat("Years:", min(years), "to", max(years), "\n")


# ============================================================
# Helper functions
# ============================================================

save_text <- function(path, text) {
  writeLines(trimws(text), path)
}

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
  blocks
}

normal_density_vals <- function(x, mu, sigma) {
  if (is.na(sigma) || sigma <= 0) return(rep(0, length(x)))
  (1 / (sigma * sqrt(2 * pi))) * exp(-0.5 * ((x - mu) / sigma)^2)
}

plot_hist_kde_normal <- function(values, title, xlabel, save_path) {
  values <- na.omit(values)
  if (length(values) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }

  mu    <- mean(values)
  sigma <- sd(values)
  x_seq <- seq(min(values), max(values), length.out = 300)
  h     <- density(values, adjust = 1)

  png(save_path, width = 800, height = 500, res = 150)
  hist(values, freq = FALSE, breaks = 15, col = rgb(0.2, 0.4, 0.8, 0.45),
       main = title, xlab = xlabel, ylab = "Density")
  lines(h$x, h$y, col = "steelblue", lwd = 2)
  lines(x_seq, normal_density_vals(x_seq, mu, sigma), col = "red", lwd = 2)
  legend("topright",
         legend = c("Histogram", "Kernel density", "Normal distribution"),
         col    = c(rgb(0.2, 0.4, 0.8, 0.45), "steelblue", "red"),
         lty    = c(NA, 1, 1), pch = c(15, NA, NA), lwd = 2)
  dev.off()
}

scatter_with_regression <- function(data, x_col, y_col, title, save_path) {
  temp <- na.omit(data[, c(x_col, y_col)])
  if (nrow(temp) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }

  corr <- cor(temp[[x_col]], temp[[y_col]])
  fit  <- lm(as.formula(paste(y_col, "~", x_col)), data = temp)
  x_seq <- seq(min(temp[[x_col]]), max(temp[[x_col]]), length.out = 200)
  y_hat <- predict(fit, newdata = setNames(data.frame(x_seq), x_col))

  png(save_path, width = 800, height = 500, res = 150)
  plot(temp[[x_col]], temp[[y_col]], pch = 16, col = rgb(0, 0, 0, 0.55),
       main = paste0(title, "\nCorrelation = ", round(corr, 3)),
       xlab = x_col, ylab = y_col)
  lines(x_seq, y_hat, col = "blue", lwd = 2)
  legend("topright", legend = "Linear fit", col = "blue", lty = 1, lwd = 2)
  dev.off()
}


# ============================================================
# Questions 1–6: Sample selection
# ============================================================

sample_rows <- lapply(countries, function(ctry) {
  row <- list(Country = ctry)

  for (dep in DEPENDENT_VARIABLES) {
    avail <- df$year[df$country == ctry & !is.na(df[[dep]])]
    blocks <- consecutive_blocks(avail)
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

kept_all <- sample_selection_table %>%
  filter(`WR: kept in sample` == TRUE, `LS: kept in sample` == TRUE)

excluded_all <- sample_selection_table %>%
  filter(`WR: kept in sample` == FALSE | `LS: kept in sample` == FALSE)

sample_selection_summary <- data.frame(
  Item = c(
    "Total number of countries",
    "Number of excluded countries",
    "Number of kept countries",
    "Excluded countries",
    "Kept countries"
  ),
  Value = c(
    length(countries),
    nrow(excluded_all),
    nrow(kept_all),
    if (nrow(excluded_all) > 0) paste(excluded_all$Country, collapse = ", ") else "None",
    paste(kept_all$Country, collapse = ", ")
  ),
  stringsAsFactors = FALSE
)
write_xlsx(sample_selection_summary, file.path(TABLES_DIR, "sample_selection_summary.xlsx"))


# ============================================================
# Number of countries per year (WR)
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

compact_number_per_date <- data.frame(
  Date = c("1970-1979", "1980-2019"),
  N    = c(14L, 15L),
  stringsAsFactors = FALSE
)
write_xlsx(compact_number_per_date, file.path(TABLES_DIR, "compact_number_of_individuals_per_date.xlsx"))


# ============================================================
# Observations by country and holes
# ============================================================

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
  avail    <- sort(df$year[df$country == ctry & !is.na(df[[dep]])])
  if (length(avail) == 0) return(NULL)
  expected <- seq(min(avail), max(avail))
  missing  <- sort(setdiff(expected, avail))

  list(
    Country                              = ctry,
    `First available year`               = min(avail),
    `Last available year`                = max(avail),
    `Number of observations`             = length(avail),
    `Has holes`                          = length(missing) > 0,
    `Number of missing years inside span`= length(missing),
    `Missing years inside span`          = if (length(missing)) paste(missing, collapse = ", ") else "None"
  )
})

holes_table <- bind_rows(Filter(Negate(is.null), holes_rows))
write_xlsx(holes_table, file.path(TABLES_DIR, "holes_inside_panel.xlsx"))

holes_summary <- data.frame(
  Item  = c("Number of countries with holes", "Proportion of countries with holes"),
  Value = c(sum(holes_table$`Has holes`), mean(holes_table$`Has holes`)),
  stringsAsFactors = FALSE
)
write_xlsx(holes_summary, file.path(TABLES_DIR, "holes_summary.xlsx"))


# ============================================================
# Within / Between variance decomposition
# ============================================================

variance_rows <- lapply(ALL_VARIABLES, function(var) {
  temp <- df %>% select(country, year, all_of(var)) %>% na.omit()
  if (nrow(temp) == 0) return(NULL)

  overall_var <- var(temp[[var]], na.rm = TRUE)

  country_avg  <- tapply(temp[[var]], temp$country, mean, na.rm = TRUE)
  between_var  <- var(country_avg, na.rm = TRUE)

  temp$cmean   <- ave(temp[[var]], temp$country, FUN = mean)
  within_var   <- var(temp[[var]] - temp$cmean, na.rm = TRUE)

  within_share <- if (!is.na(overall_var) && overall_var != 0) within_var / overall_var else NA_real_

  list(
    Variable          = var,
    `Variable in words` = ifelse(var %in% names(VARIABLE_LABELS), VARIABLE_LABELS[var], var),
    N                 = length(unique(temp$country)),
    NT                = nrow(temp),
    `NT/N`            = nrow(temp) / length(unique(temp$country)),
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


# ============================================================
# Variable categories
# ============================================================

two_index_vars        <- character(0)
time_invariant_vars   <- character(0)
individual_invariant_vars <- character(0)

for (i in seq_len(nrow(variance_table))) {
  var <- variance_table$Variable[i]
  ws  <- variance_table$`Within share`[i]
  if (is.na(ws)) next
  if (abs(ws - 0) < 1e-6)  time_invariant_vars   <- c(time_invariant_vars,   var)
  else if (abs(ws - 1) < 1e-6) individual_invariant_vars <- c(individual_invariant_vars, var)
  else two_index_vars <- c(two_index_vars, var)
}

variable_categories <- data.frame(
  `List of variables varying with two indices (time and individuals)` =
    if (length(two_index_vars)) paste(two_index_vars, collapse = ", ") else "None",
  `List of time-invariant variables` =
    if (length(time_invariant_vars)) paste(time_invariant_vars, collapse = ", ") else "None",
  `List of individual-invariant variables` =
    if (length(individual_invariant_vars)) paste(individual_invariant_vars, collapse = ", ") else "None",
  `Number K`  = length(two_index_vars),
  `Number K1` = length(time_invariant_vars),
  `Number K2` = length(individual_invariant_vars),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write_xlsx(variable_categories, file.path(TABLES_DIR, "variable_categories.xlsx"))


# ============================================================
# Table 5: Time-varying variables
# ============================================================

time_varying <- variance_table %>%
  filter(`Within share` > 0, `Within share` < 1)

dep_rows   <- time_varying %>% filter(Variable %in% DEPENDENT_VARIABLES) %>%
  mutate(order = match(Variable, c("WR", "LS"))) %>%
  arrange(order) %>% select(-order)
other_rows <- time_varying %>% filter(!Variable %in% DEPENDENT_VARIABLES) %>%
  arrange(desc(`Within share`))

time_varying_variables <- bind_rows(dep_rows, other_rows) %>%
  select(`Variable in words`, N, NT, `NT/N`,
         `Overall variance`, `Between variance`, `Within variance`, `Within share (%)`)

time_varying_for_word <- time_varying_variables %>%
  mutate(across(c(`NT/N`, `Overall variance`, `Between variance`,
                  `Within variance`, `Within share (%)`), ~round(.x, 2)))

write_xlsx(time_varying_for_word, file.path(TABLES_DIR, "time_varying_variables_for_word.xlsx"))


# ============================================================
# Between / Within decomposition and distributions
# ============================================================

bw_data <- df %>% select(country, year, WR, LS, i)

for (var in c("WR", "LS", "i")) {
  bw_data[[paste0(var, "_between")]] <- ave(bw_data[[var]], bw_data$country, FUN = function(x) mean(x, na.rm = TRUE))
  bw_data[[paste0(var, "_within")]]  <- bw_data[[var]] - bw_data[[paste0(var, "_between")]]
}

write_xlsx(bw_data, file.path(TABLES_DIR, "between_within_variables.xlsx"))


bw_stats_rows <- list()

for (var in c("WR", "LS", "i")) {
  between_vals <- tapply(df[[var]], df$country, mean, na.rm = TRUE)
  between_vals <- na.omit(between_vals)
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
    trans  <- pair[[1]]
    values <- pair[[2]]
    bw_stats_rows[[length(bw_stats_rows) + 1]] <- list(
      Variable            = var,
      Transformation      = trans,
      Observations        = length(values),
      Mean                = mean(values),
      Median              = median(values),
      `Standard deviation`= sd(values),
      Minimum             = min(values),
      Maximum             = max(values),
      Skewness            = {n <- length(values); mu <- mean(values); s <- sd(values)
                             if (s == 0) NA_real_ else (sum((values - mu)^3) / n) / s^3},
      Kurtosis            = {n <- length(values); mu <- mean(values); s <- sd(values)
                             if (s == 0) NA_real_ else (sum((values - mu)^4) / n) / s^4 - 3}
    )
  }
}

bw_stats <- bind_rows(bw_stats_rows)
write_xlsx(bw_stats, file.path(TABLES_DIR, "between_within_descriptive_statistics.xlsx"))


# ============================================================
# Bivariate: between / within vs i
# ============================================================

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
  corr_w <- cor(temp_w[[paste0(y, "_within")]], temp_w$i_within)

  corr_rows[[length(corr_rows) + 1]] <- list(
    `Dependent variable` = y, Transformation = "Between",       `Correlation with i` = corr_b)
  corr_rows[[length(corr_rows) + 1]] <- list(
    `Dependent variable` = y, Transformation = "One-way within", `Correlation with i` = corr_w)

  scatter_with_regression(
    between_country, paste0("i_between"), paste0(y, "_between"),
    title     = paste("Between relationship between i and", y),
    save_path = file.path(FIGURES_DIR, paste0("between_scatter_i_", y, ".png")))

  scatter_with_regression(
    bw_data, "i_within", paste0(y, "_within"),
    title     = paste("One-way within relationship between i and", y),
    save_path = file.path(FIGURES_DIR, paste0("within_scatter_i_", y, ".png")))
}

bw_correlations <- bind_rows(corr_rows)
write_xlsx(bw_correlations, file.path(TABLES_DIR, "between_within_correlations_with_i.xlsx"))


# ============================================================
# QUESTION 7: First differences and Two-way fixed effects
# ============================================================

ID_COL       <- "country"
TIME_COL     <- "year"
KEY_VARS_Q7  <- c("WR", "LS", "i")
Y_VARS_Q7    <- c("WR", "LS")
X_VAR_Q7     <- "i"


save_distribution_q7 <- function(series, title, filename, xlabel) {
  values <- series[is.finite(series)]
  values <- na.omit(values)
  if (length(values) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }
  mu    <- mean(values)
  sigma <- sd(values)
  x_seq <- seq(min(values), max(values), length.out = 250)
  h     <- density(values, adjust = 1)

  png(file.path(FIGURES_DIR, filename), width = 800, height = 500, res = 150)
  hist(values, freq = FALSE, breaks = 30, col = rgb(0.2, 0.4, 0.8, 0.45),
       main = title, xlab = xlabel, ylab = "Density")
  if (sigma > 0 && length(unique(values)) > 1) {
    lines(x_seq, normal_density_vals(x_seq, mu, sigma), col = "red", lwd = 2)
    lines(h$x, h$y, col = "steelblue", lwd = 2)
  }
  abline(v = mu, lty = 3)
  legend("topright",
         legend = c("Normal distribution", "Kernel density", "Mean"),
         col    = c("red", "steelblue", "black"),
         lty    = c(1, 1, 3), lwd = 2)
  dev.off()
}


save_scatter_with_marginals_q7 <- function(data, x_col, y_col, title, filename) {
  plot_data <- na.omit(data[, c(x_col, y_col)])
  plot_data <- plot_data[is.finite(plot_data[[x_col]]) & is.finite(plot_data[[y_col]]), ]
  if (nrow(plot_data) < 3) {
    cat("Not enough observations to plot:", title, "\n")
    return(invisible(NULL))
  }

  x    <- plot_data[[x_col]]
  y    <- plot_data[[y_col]]
  corr <- cor(x, y)
  fit  <- lm(y ~ x)
  x_line <- seq(min(x), max(x), length.out = 100)
  y_line <- coef(fit)[1] + coef(fit)[2] * x_line

  png(file.path(FIGURES_DIR, filename), width = 800, height = 800, res = 150)
  layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE),
         widths = c(3, 1), heights = c(1, 3))

  # Main scatter
  par(mar = c(5, 4, 1, 1))
  plot(x, y, pch = 16, col = rgb(0, 0, 0, 0.65),
       xlab = x_col, ylab = y_col)
  lines(x_line, y_line, col = "blue", lwd = 2)
  legend("topright",
         legend = paste0("Linear fit, corr = ", round(corr, 3)),
         col = "blue", lty = 1, lwd = 2)

  # Top marginal (x)
  par(mar = c(0, 4, 2, 1))
  hist(x, breaks = 25, main = title, xaxt = "n", ylab = "", col = "grey80")

  # Right marginal (y)
  par(mar = c(5, 0, 1, 2))
  hist(y, breaks = 25, horiz = TRUE, main = "", yaxt = "n", xlab = "", col = "grey80")

  dev.off()
  layout(1)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}


twfe_transform <- function(data, variable) {
  country_mean <- ave(data[[variable]], data[[ID_COL]], FUN = function(x) mean(x, na.rm = TRUE))
  year_mean    <- ave(data[[variable]], data[[TIME_COL]], FUN = function(x) mean(x, na.rm = TRUE))
  global_mean  <- mean(data[[variable]], na.rm = TRUE)
  data[[variable]] - country_mean - year_mean + global_mean
}


save_country_boxplot_q7 <- function(data, variable, filename, ylabel) {
  plot_data <- na.omit(data[, c(ID_COL, variable)])
  plot_data <- plot_data[is.finite(plot_data[[variable]]), ]
  if (nrow(plot_data) == 0) return(invisible(NULL))

  country_var <- tapply(plot_data[[variable]], plot_data[[ID_COL]], var, na.rm = TRUE)
  ordered_countries <- names(sort(country_var))
  vals_list <- lapply(ordered_countries, function(ctry) {
    plot_data[[variable]][plot_data[[ID_COL]] == ctry]
  })

  png(file.path(FIGURES_DIR, filename), width = 1100, height = 600, res = 150)
  boxplot(vals_list, names = ordered_countries, las = 2,
          main = paste("Two-way fixed effects boxplot of", variable, "by country"),
          xlab = "Country", ylab = ylabel)
  abline(h = 0, lty = 3)
  dev.off()
}


country_correlation_table_q7 <- function(data, x_col, y_col) {
  rows <- lapply(split(data, data[[ID_COL]]), function(cd) {
    sample <- na.omit(cd[, c(x_col, y_col)])
    sample <- sample[is.finite(sample[[x_col]]) & is.finite(sample[[y_col]]), ]
    n_obs  <- nrow(sample)

    if (n_obs < 3 || length(unique(sample[[x_col]])) <= 1) {
      return(list(country = cd[[ID_COL]][1], N = n_obs,
                  correlation = NA_real_,
                  std_y = NA_real_, std_x = NA_real_,
                  simple_slope = NA_real_))
    }
    corr  <- cor(sample[[x_col]], sample[[y_col]])
    std_x <- sd(sample[[x_col]])
    std_y <- sd(sample[[y_col]])
    list(country = cd[[ID_COL]][1], N = n_obs,
         correlation  = corr,
         std_y        = std_y,
         std_x        = std_x,
         simple_slope = if (std_x != 0) corr * std_y / std_x else NA_real_)
  })

  bind_rows(rows) %>% arrange(desc(correlation))
}


# --------------------------------------------------------
# First differences
# --------------------------------------------------------

q7_data <- df %>% arrange(country, year)

q7_data <- q7_data %>%
  group_by(country) %>%
  mutate(
    year_gap = year - lag(year),
    across(all_of(KEY_VARS_Q7),
           ~ifelse(!is.na(year_gap) & year_gap == 1, . - lag(.), NA_real_),
           .names = "d_{.col}")
  ) %>%
  ungroup()

# Boundary check
country_starts <- which(q7_data$country != lag(q7_data$country, default = ""))
rows_to_check  <- sort(unique(c(country_starts - 1, country_starts, country_starts + 1)))
rows_to_check  <- rows_to_check[rows_to_check >= 1 & rows_to_check <= nrow(q7_data)]
rows_to_check  <- rows_to_check[seq_len(min(length(rows_to_check), 12))]

fd_boundary_check <- q7_data[rows_to_check,
                              c("country", "year", "WR", "d_WR", "LS", "d_LS", "i", "d_i")]
write_xlsx(fd_boundary_check, file.path(TABLES_DIR, "q7_first_difference_boundary_check.xlsx"))

fd_vars_q7 <- paste0("d_", KEY_VARS_Q7)

fd_desc <- t(sapply(fd_vars_q7, function(v) {
  x <- na.omit(q7_data[[v]])
  c(N = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
}))
write_xlsx(as.data.frame(fd_desc), file.path(TABLES_DIR, "q7_first_difference_descriptive_statistics.xlsx"))

write_xlsx(as.data.frame(cor(q7_data[fd_vars_q7], use = "complete.obs")),
           file.path(TABLES_DIR, "q7_first_difference_correlations.xlsx"))

for (var in KEY_VARS_Q7) {
  save_distribution_q7(
    q7_data[[paste0("d_", var)]],
    title    = paste("First difference distribution of", var),
    filename = paste0("q7_fd_distribution_", var, ".png"),
    xlabel   = paste("First difference of", var)
  )
}

for (y_var in Y_VARS_Q7) {
  save_scatter_with_marginals_q7(
    q7_data,
    x_col    = paste0("d_", X_VAR_Q7),
    y_col    = paste0("d_", y_var),
    title    = paste0("First differences: d_", X_VAR_Q7, " and d_", y_var),
    filename = paste0("q7_fd_scatter_d_", X_VAR_Q7, "_d_", y_var, ".png")
  )
}


# --------------------------------------------------------
# Balanced panel two-way fixed effects
# --------------------------------------------------------

complete_data <- q7_data %>% filter(if_all(all_of(KEY_VARS_Q7), ~!is.na(.)))

obs_by_country <- complete_data %>%
  group_by(country) %>% summarise(T = n_distinct(year), .groups = "drop")

max_t              <- max(obs_by_country$T)
balanced_countries <- obs_by_country$country[obs_by_country$T == max_t]

twfe_data <- complete_data %>% filter(country %in% balanced_countries)

year_counts  <- twfe_data %>% group_by(year) %>% summarise(n = n_distinct(country), .groups = "drop")
common_years <- year_counts$year[year_counts$n == length(balanced_countries)]
twfe_data    <- twfe_data %>% filter(year %in% common_years) %>%
  arrange(country, year) %>% as.data.frame()

balanced_summary <- data.frame(
  item  = c("Number of countries", "Number of years", "First year", "Last year", "Number of observations"),
  value = c(length(unique(twfe_data$country)), length(unique(twfe_data$year)),
            min(twfe_data$year), max(twfe_data$year), nrow(twfe_data)),
  stringsAsFactors = FALSE
)
write_xlsx(balanced_summary, file.path(TABLES_DIR, "q7_twfe_balanced_sample_summary.xlsx"))
write_xlsx(data.frame(country = balanced_countries), file.path(TABLES_DIR, "q7_twfe_balanced_countries.xlsx"))

for (var in KEY_VARS_Q7) {
  twfe_data[[paste0("twfe_", var)]] <- twfe_transform(twfe_data, var)
}

twfe_vars_q7 <- paste0("twfe_", KEY_VARS_Q7)

twfe_desc <- t(sapply(twfe_vars_q7, function(v) {
  x <- na.omit(twfe_data[[v]])
  c(N = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
}))
write_xlsx(as.data.frame(twfe_desc), file.path(TABLES_DIR, "q7_twfe_descriptive_statistics.xlsx"))
write_xlsx(as.data.frame(cor(twfe_data[twfe_vars_q7], use = "complete.obs")),
           file.path(TABLES_DIR, "q7_twfe_correlations.xlsx"))

year_mean_i  <- tapply(twfe_data[[X_VAR_Q7]], twfe_data$year, mean, na.rm = TRUE)
global_mean_i <- mean(twfe_data[[X_VAR_Q7]], na.rm = TRUE)

time_component_i <- data.frame(
  year = as.integer(names(year_mean_i)),
  minus_year_mean_plus_global_mean_i = -year_mean_i + global_mean_i
)
write_xlsx(time_component_i, file.path(TABLES_DIR, "q7_twfe_time_component_i.xlsx"))

png(file.path(FIGURES_DIR, "q7_twfe_time_component_i.png"), width = 900, height = 500, res = 150)
plot(time_component_i$year, time_component_i$minus_year_mean_plus_global_mean_i,
     type = "o", pch = 16,
     main = "Two-way fixed effects time component for i",
     xlab = "Year", ylab = "minus i_.t plus i_..")
abline(h = 0, lty = 3)
dev.off()

for (var in KEY_VARS_Q7) {
  save_distribution_q7(
    twfe_data[[paste0("twfe_", var)]],
    title    = paste("Two-way fixed effects distribution of", var),
    filename = paste0("q7_twfe_distribution_", var, ".png"),
    xlabel   = paste("Two-way fixed effects transformation of", var)
  )
}

for (y_var in Y_VARS_Q7) {
  save_scatter_with_marginals_q7(
    twfe_data,
    x_col    = paste0("twfe_", X_VAR_Q7),
    y_col    = paste0("twfe_", y_var),
    title    = paste("Two-way fixed effects:", X_VAR_Q7, "and", y_var),
    filename = paste0("q7_twfe_scatter_", X_VAR_Q7, "_", y_var, ".png")
  )
}

for (var in KEY_VARS_Q7) {
  save_country_boxplot_q7(
    twfe_data,
    variable = paste0("twfe_", var),
    filename = paste0("q7_twfe_boxplot_by_country_", var, ".png"),
    ylabel   = paste0("twfe_", var)
  )
}

for (y_var in Y_VARS_Q7) {
  country_corr <- country_correlation_table_q7(
    twfe_data,
    x_col = paste0("twfe_", X_VAR_Q7),
    y_col = paste0("twfe_", y_var)
  )
  write_xlsx(country_corr,
             file.path(TABLES_DIR, paste0("q7_twfe_country_correlations_", X_VAR_Q7, "_", y_var, ".xlsx")))
}


# --------------------------------------------------------
# Unbalanced panel two-way fixed effects
# --------------------------------------------------------

unbalanced_data <- q7_data %>% filter(if_all(all_of(KEY_VARS_Q7), ~!is.na(.)))

obs_unbalanced <- unbalanced_data %>%
  group_by(country) %>% summarise(T = n_distinct(year), .groups = "drop")

keep_countries   <- obs_unbalanced$country[obs_unbalanced$T > 1]
removed_countries <- sort(setdiff(unique(na.omit(q7_data$country)), keep_countries))

unbalanced_data <- unbalanced_data %>%
  filter(country %in% keep_countries) %>%
  arrange(country, year) %>% as.data.frame()

unbalanced_summary <- data.frame(
  item  = c("Number of countries", "Minimum number of years by country",
            "Maximum number of years by country", "First year", "Last year",
            "Number of observations", "Countries removed because of one observation"),
  value = c(
    length(unique(unbalanced_data$country)),
    min(obs_unbalanced$T[obs_unbalanced$country %in% keep_countries]),
    max(obs_unbalanced$T[obs_unbalanced$country %in% keep_countries]),
    min(unbalanced_data$year), max(unbalanced_data$year), nrow(unbalanced_data),
    if (length(removed_countries)) paste(removed_countries, collapse = ", ") else "None"
  ),
  stringsAsFactors = FALSE
)
write_xlsx(unbalanced_summary, file.path(TABLES_DIR, "q7_unbalanced_twfe_sample_summary.xlsx"))

for (var in KEY_VARS_Q7) {
  within_col <- paste0("within_unbalanced_", var)
  twfe_col   <- paste0("twfe_unbalanced_", var)
  cmean      <- ave(unbalanced_data[[var]], unbalanced_data$country,
                    FUN = function(x) mean(x, na.rm = TRUE))
  within_val <- unbalanced_data[[var]] - cmean
  ymean      <- ave(within_val, unbalanced_data$year,
                    FUN = function(x) mean(x, na.rm = TRUE))
  unbalanced_data[[within_col]] <- within_val
  unbalanced_data[[twfe_col]]   <- within_val - ymean
}

unbalanced_twfe_vars <- paste0("twfe_unbalanced_", KEY_VARS_Q7)

write_xlsx(
  unbalanced_data[, c("country", "year", KEY_VARS_Q7, unbalanced_twfe_vars)],
  file.path(TABLES_DIR, "q7_unbalanced_twfe_transformed_variables.xlsx")
)

unbal_desc <- t(sapply(unbalanced_twfe_vars, function(v) {
  x <- na.omit(unbalanced_data[[v]])
  c(N = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
}))
write_xlsx(as.data.frame(unbal_desc), file.path(TABLES_DIR, "q7_unbalanced_twfe_descriptive_statistics.xlsx"))
write_xlsx(as.data.frame(cor(unbalanced_data[unbalanced_twfe_vars], use = "complete.obs")),
           file.path(TABLES_DIR, "q7_unbalanced_twfe_correlations.xlsx"))

for (var in KEY_VARS_Q7) {
  save_distribution_q7(
    unbalanced_data[[paste0("twfe_unbalanced_", var)]],
    title    = paste("Unbalanced TWFE distribution of", var),
    filename = paste0("q7_unbalanced_twfe_distribution_", var, ".png"),
    xlabel   = paste("Unbalanced TWFE transformation of", var)
  )
}

cat("Question 7 outputs saved successfully.\n")


# ============================================================
# QUESTION 8: COMPARISON OF TRANSFORMED VARIABLES
# ============================================================

Q8_VARS <- c("WR", "LS", "i")
Y_VARS  <- c("WR", "LS")
X_VAR   <- "i"

q8_between <- between_country
q8_within  <- bw_data[, c("country", "year", paste0(Q8_VARS, "_within"))]
q8_fd      <- q7_data[,  c("country", "year", paste0("d_",     Q8_VARS))]
q8_twfe    <- twfe_data[, c("country", "year", paste0("twfe_",  Q8_VARS))]


# --------------------------------------------------------
# Q8.2 Summary statistics
# --------------------------------------------------------

q8_summary_rows <- list()

for (var in Q8_VARS) {
  transformations <- list(
    "Between"              = q8_between[[paste0(var, "_between")]],
    "One-way within"       = q8_within[[paste0(var,  "_within")]],
    "First differences"    = q8_fd[[paste0("d_",     var)]],
    "Two-way fixed effects"= q8_twfe[[paste0("twfe_", var)]]
  )

  for (trans_name in names(transformations)) {
    values <- na.omit(transformations[[trans_name]])
    values <- values[is.finite(values)]
    n      <- length(values)
    s      <- sd(values)

    q8_summary_rows[[length(q8_summary_rows) + 1]] <- list(
      Variable             = var,
      Transformation       = trans_name,
      N                    = n,
      Mean                 = mean(values),
      Median               = median(values),
      `Standard deviation` = s,
      `Standard error`     = if (n > 0) s / sqrt(n) else NA_real_,
      Q1                   = quantile(values, 0.25),
      Q3                   = quantile(values, 0.75),
      `Standardized min`   = if (s != 0) (min(values) - mean(values)) / s else NA_real_,
      `Standardized max`   = if (s != 0) (max(values) - mean(values)) / s else NA_real_
    )
  }
}

q8_summary <- bind_rows(q8_summary_rows)
write_xlsx(q8_summary, file.path(TABLES_DIR, "q8_transformation_summary_statistics.xlsx"))


# --------------------------------------------------------
# Q8.3 Boxplots
# --------------------------------------------------------

save_q8_single_boxplot <- function(series, title, ylabel, filename) {
  values <- na.omit(series)
  values <- values[is.finite(values)]
  if (length(values) < 3) {
    cat("Not enough data for graph:", title, "\n")
    return(invisible(NULL))
  }
  png(file.path(FIGURES_DIR, filename), width = 600, height = 500, res = 150)
  boxplot(values, main = title, ylab = ylabel, names = "All countries")
  abline(h = 0, lty = 3)
  dev.off()
}

save_q8_boxplot_by_country <- function(data, value_col, title, filename) {
  plot_data <- na.omit(data[, c("country", value_col)])
  plot_data <- plot_data[is.finite(plot_data[[value_col]]), ]
  if (nrow(plot_data) == 0) return(invisible(NULL))

  country_var <- tapply(plot_data[[value_col]], plot_data$country, var, na.rm = TRUE)
  ordered     <- names(sort(country_var))
  vals_list   <- lapply(ordered, function(ctry) plot_data[[value_col]][plot_data$country == ctry])

  png(file.path(FIGURES_DIR, filename), width = 1100, height = 600, res = 150)
  boxplot(vals_list, names = ordered, las = 2, main = title,
          xlab = "Country", ylab = value_col)
  abline(h = 0, lty = 3)
  dev.off()
}

for (var in Q8_VARS) {
  save_q8_single_boxplot(
    q8_between[[paste0(var, "_between")]],
    title    = paste("Q8 Between distribution across countries:", var),
    ylabel   = paste0(var, "_between"),
    filename = paste0("q8_boxplot_between_all_countries_", var, ".png")
  )

  save_q8_boxplot_by_country(q8_within, paste0(var, "_within"),
    paste("Q8 One-way within distribution by country:", var),
    paste0("q8_boxplot_within_by_country_", var, ".png"))

  save_q8_boxplot_by_country(q8_fd, paste0("d_", var),
    paste("Q8 First differences distribution by country:", var),
    paste0("q8_boxplot_fd_by_country_", var, ".png"))

  save_q8_boxplot_by_country(q8_twfe, paste0("twfe_", var),
    paste("Q8 Two-way fixed effects distribution by country:", var),
    paste0("q8_boxplot_twfe_by_country_", var, ".png"))
}


# --------------------------------------------------------
# Q8.4 Correlation matrices with heatmaps
# --------------------------------------------------------

Q8_CORR_VARS <- ALL_VARIABLES[ALL_VARIABLES %in% names(df) & ALL_VARIABLES != "PCOM"]

q8_corr_base <- df[, c("country", "year", Q8_CORR_VARS)]
q8_corr_base <- q8_corr_base %>%
  group_by(country) %>%
  mutate(trend = row_number(),
         across(all_of(Q8_CORR_VARS), ~lag(.), .names = "{.col}_lag1")) %>%
  ungroup() %>%
  as.data.frame()

# Between correlation matrix
between_corr_data <- q8_corr_base %>%
  group_by(country) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")

between_corr_cols <- c(Q8_CORR_VARS, "trend", paste0(Q8_CORR_VARS, "_lag1"))
between_corr_cols <- between_corr_cols[between_corr_cols %in% names(between_corr_data)]

q8_between_full_corr <- cor(between_corr_data[, between_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_between_full_corr),
           file.path(TABLES_DIR, "q8_between_full_correlation_matrix_with_trend_lags.xlsx"))

# One-way within correlation matrix
within_corr_data <- q8_corr_base[, c("country", "year", between_corr_cols)]
for (col in between_corr_cols) {
  if (col %in% names(within_corr_data)) {
    within_corr_data[[paste0(col, "_within")]] <-
      within_corr_data[[col]] - ave(within_corr_data[[col]], within_corr_data$country,
                                     FUN = function(x) mean(x, na.rm = TRUE))
  }
}
within_corr_cols <- paste0(between_corr_cols, "_within")
within_corr_cols <- within_corr_cols[within_corr_cols %in% names(within_corr_data)]
q8_within_full_corr <- cor(within_corr_data[, within_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_within_full_corr),
           file.path(TABLES_DIR, "q8_within_full_correlation_matrix_with_trend_lags.xlsx"))

# FD correlation matrix
fd_corr_data <- q7_data[, "country", drop = FALSE]
fd_corr_data$year <- q7_data$year
for (var in Q8_CORR_VARS) {
  d_col <- paste0("d_", var)
  fd_corr_data[[d_col]] <- if (d_col %in% names(q7_data)) q7_data[[d_col]] else
    ave(q7_data[[var]], q7_data$country, FUN = function(x) c(NA, diff(x)))
  fd_corr_data[[paste0(d_col, "_lag1")]] <-
    ave(fd_corr_data[[d_col]], fd_corr_data$country, FUN = function(x) c(NA, head(x, -1)))
}
fd_corr_cols <- c(paste0("d_", Q8_CORR_VARS), paste0("d_", Q8_CORR_VARS, "_lag1"))
fd_corr_cols <- fd_corr_cols[fd_corr_cols %in% names(fd_corr_data)]
q8_fd_full_corr <- cor(fd_corr_data[, fd_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_fd_full_corr),
           file.path(TABLES_DIR, "q8_fd_full_correlation_matrix_with_lags.xlsx"))

# TWFE correlation matrix
twfe_corr_data <- twfe_data[, c("country", "year")]
for (var in Q8_CORR_VARS) {
  tc <- paste0("twfe_", var)
  twfe_corr_data[[tc]] <- if (tc %in% names(twfe_data)) twfe_data[[tc]] else
    if (var %in% names(twfe_data)) twfe_transform(twfe_data, var) else NA_real_
}
twfe_corr_cols <- paste0("twfe_", Q8_CORR_VARS)
twfe_corr_cols <- twfe_corr_cols[twfe_corr_cols %in% names(twfe_corr_data)]
q8_twfe_full_corr <- cor(twfe_corr_data[, twfe_corr_cols], use = "pairwise.complete.obs")
write_xlsx(as.data.frame(q8_twfe_full_corr),
           file.path(TABLES_DIR, "q8_twfe_full_correlation_matrix.xlsx"))

# Small matrices for the report (WR, LS, i only)
q8_between_corr <- cor(q8_between[, paste0(Q8_VARS, "_between")], use = "complete.obs")
q8_within_corr  <- cor(q8_within[,  paste0(Q8_VARS, "_within")],  use = "complete.obs")
q8_fd_corr      <- cor(q8_fd[,      paste0("d_",    Q8_VARS)],    use = "complete.obs")
q8_twfe_corr    <- cor(q8_twfe[,    paste0("twfe_", Q8_VARS)],    use = "complete.obs")

write_xlsx(as.data.frame(q8_between_corr), file.path(TABLES_DIR, "q8_between_correlation_matrix.xlsx"))
write_xlsx(as.data.frame(q8_within_corr),  file.path(TABLES_DIR, "q8_within_correlation_matrix.xlsx"))
write_xlsx(as.data.frame(q8_fd_corr),      file.path(TABLES_DIR, "q8_fd_correlation_matrix.xlsx"))
write_xlsx(as.data.frame(q8_twfe_corr),    file.path(TABLES_DIR, "q8_twfe_correlation_matrix.xlsx"))


# Heatmap function using base R
save_corr_heatmap <- function(corr_matrix, title, filename) {
  n   <- nrow(corr_matrix)
  png(file.path(FIGURES_DIR, filename), width = 700, height = 650, res = 150)
  par(mar = c(8, 8, 4, 6))

  # Color palette: blue -> white -> red
  n_col  <- 101
  breaks <- seq(-1, 1, length.out = n_col + 1)
  cols   <- colorRampPalette(c("#3B4CC0", "white", "#B40426"))(n_col)

  image(1:n, 1:n, t(corr_matrix[n:1, ]),
        col  = cols, breaks = breaks,
        axes = FALSE, xlab = "", ylab = "", main = title)
  axis(1, at = 1:n, labels = colnames(corr_matrix), las = 2, cex.axis = 0.8)
  axis(2, at = 1:n, labels = rev(rownames(corr_matrix)), las = 1, cex.axis = 0.8)

  # Annotate cells
  for (i in 1:n) for (j in 1:n) {
    text(j, n + 1 - i, round(corr_matrix[i, j], 2), cex = 0.7)
  }

  # Color legend
  legend_y <- seq(1, n, length.out = n_col)
  legend_x <- rep(n + 0.7, n_col)
  usr      <- par("usr")
  rect(usr[2] + 0.1, seq(1, n, length.out = n_col + 1)[-n_col - 1],
       usr[2] + 0.6, seq(1, n, length.out = n_col + 1)[-1],
       col = cols, border = NA, xpd = TRUE)
  axis(4, at = c(1, (n + 1) / 2, n), labels = c(-1, 0, 1), las = 1,
       xpd = TRUE, line = 0.5)

  dev.off()
}

save_corr_heatmap(q8_between_corr, "Between Correlation Matrix",          "q8_between_correlation_matrix.png")
save_corr_heatmap(q8_within_corr,  "One-Way Within Correlation Matrix",    "q8_within_correlation_matrix.png")
save_corr_heatmap(q8_fd_corr,      "First Difference Correlation Matrix",  "q8_fd_correlation_matrix.png")
save_corr_heatmap(q8_twfe_corr,    "Two-Way Fixed Effects Correlation Matrix", "q8_twfe_correlation_matrix.png")


# --------------------------------------------------------
# Q8.5 Autocorrelation and trend correlation
# --------------------------------------------------------

auto_trend_rows <- lapply(Q8_CORR_VARS, function(var) {
  lag1_col  <- paste0(var, "_lag1")
  temp_auto <- na.omit(q8_corr_base[, c(var, lag1_col)])
  temp_trend<- na.omit(q8_corr_base[, c(var, "trend")])

  list(
    Variable                  = var,
    `Autocorrelation with lag 1` = cor(temp_auto[[var]], temp_auto[[lag1_col]]),
    `N autocorrelation`       = nrow(temp_auto),
    `Trend correlation`       = cor(temp_trend[[var]], temp_trend$trend),
    `N trend correlation`     = nrow(temp_trend)
  )
})

q8_auto_trend <- bind_rows(auto_trend_rows)
write_xlsx(q8_auto_trend, file.path(TABLES_DIR, "q8_autocorrelation_and_trend_correlation.xlsx"))

fd_lag_check_cols <- c("country", "year", "d_WR", "d_LS", "d_i", "d_WR_lag1", "d_LS_lag1", "d_i_lag1")
fd_lag_check_cols <- fd_lag_check_cols[fd_lag_check_cols %in% names(fd_corr_data)]
write_xlsx(head(fd_corr_data[, fd_lag_check_cols], 30),
           file.path(TABLES_DIR, "q8_first_30_fd_and_lag_check.xlsx"))


# --------------------------------------------------------
# Q8.6 Bivariate graphs: linear, quadratic, LOWESS
# --------------------------------------------------------

save_q8_bivariate_fit <- function(data, x_col, y_col, title, filename) {
  plot_data <- na.omit(data[, c(x_col, y_col)])
  plot_data <- plot_data[is.finite(plot_data[[x_col]]) & is.finite(plot_data[[y_col]]), ]
  if (nrow(plot_data) < 5 || length(unique(plot_data[[x_col]])) <= 2) {
    cat("Not enough data for graph:", title, "\n")
    return(invisible(NULL))
  }

  x    <- plot_data[[x_col]]
  y    <- plot_data[[y_col]]
  corr <- cor(x, y)
  x_grid <- seq(min(x), max(x), length.out = 200)

  png(file.path(FIGURES_DIR, filename), width = 800, height = 500, res = 150)
  plot(x, y, pch = 16, col = rgb(0, 0, 0, 0.55),
       main = paste0(title, "\nCorrelation = ", round(corr, 3)),
       xlab = x_col, ylab = y_col)

  # Linear fit
  lin_coef <- coef(lm(y ~ x))
  lines(x_grid, lin_coef[1] + lin_coef[2] * x_grid, col = "blue", lwd = 2)

  # Quadratic fit
  if (length(unique(x)) > 2) {
    quad_coef <- coef(lm(y ~ x + I(x^2)))
    lines(x_grid, quad_coef[1] + quad_coef[2] * x_grid + quad_coef[3] * x_grid^2,
          col = "orange", lwd = 2)
  }

  # LOWESS
  lw <- lowess(x, y, f = 0.3)
  lines(lw$x, lw$y, col = "darkgreen", lwd = 2)

  legend("topright",
         legend = c("Linear fit", "Quadratic fit", "LOWESS fit"),
         col    = c("blue", "orange", "darkgreen"),
         lty    = 1, lwd = 2)
  grid()
  dev.off()
}

for (y_var in Y_VARS) {
  save_q8_bivariate_fit(q8_between, paste0(X_VAR, "_between"), paste0(y_var, "_between"),
    paste("Q8 Between:", X_VAR, "and", y_var),
    paste0("q8_bivariate_between_", X_VAR, "_", y_var, ".png"))

  save_q8_bivariate_fit(q8_within, paste0(X_VAR, "_within"), paste0(y_var, "_within"),
    paste("Q8 One-way within:", X_VAR, "and", y_var),
    paste0("q8_bivariate_within_", X_VAR, "_", y_var, ".png"))

  save_q8_bivariate_fit(q8_fd, paste0("d_", X_VAR), paste0("d_", y_var),
    paste0("Q8 First differences: d_", X_VAR, " and d_", y_var),
    paste0("q8_bivariate_fd_", X_VAR, "_", y_var, ".png"))

  save_q8_bivariate_fit(q8_twfe, paste0("twfe_", X_VAR), paste0("twfe_", y_var),
    paste("Q8 TWFE:", X_VAR, "and", y_var),
    paste0("q8_bivariate_twfe_", X_VAR, "_", y_var, ".png"))
}


# ============================================================
# QUESTION 9: COUNTRY HETEROGENEITY
# ============================================================

classify_correlation <- function(corr) {
  if (is.na(corr))    return("Missing")
  if (corr > 0.08)    return("Positive")
  if (corr < -0.08)   return("Negative")
  return("Weak")
}

q9_rows <- list()

for (y_var in Y_VARS) {

  # First differences
  for (ctry in unique(na.omit(q7_data$country))) {
    sample <- q7_data[q7_data$country == ctry,
                      c(paste0("d_", y_var), "d_i")]
    sample <- na.omit(sample)
    sample <- sample[is.finite(sample[[1]]) & is.finite(sample[[2]]), ]
    n_obs  <- nrow(sample)

    if (n_obs >= 3 && sd(sample$d_i, na.rm = TRUE) != 0) {
      corr  <- cor(sample[[1]], sample$d_i)
      std_y <- sd(sample[[1]])
      std_x <- sd(sample$d_i)
      beta  <- corr * std_y / std_x
    } else {
      corr <- std_y <- std_x <- beta <- NA_real_
    }

    q9_rows[[length(q9_rows) + 1]] <- list(
      `Dependent variable` = y_var,
      Transformation       = "First differences",
      `Individual name (i)`= ctry,
      `T(i)`               = n_obs,
      `r(Y,X)`             = corr,
      `sigma(Y)`           = std_y,
      `sigma(X)`           = std_x,
      `sigma(Y)/sigma(X)`  = if (!is.na(std_x) && std_x != 0) std_y / std_x else NA_real_,
      `beta = r * sigma(Y)/sigma(X)` = beta,
      Group                = classify_correlation(corr)
    )
  }

  # TWFE
  for (ctry in unique(na.omit(twfe_data$country))) {
    sample <- twfe_data[twfe_data$country == ctry,
                        c(paste0("twfe_", y_var), "twfe_i")]
    sample <- na.omit(sample)
    sample <- sample[is.finite(sample[[1]]) & is.finite(sample[[2]]), ]
    n_obs  <- nrow(sample)

    if (n_obs >= 3 && sd(sample$twfe_i, na.rm = TRUE) != 0) {
      corr  <- cor(sample[[1]], sample$twfe_i)
      std_y <- sd(sample[[1]])
      std_x <- sd(sample$twfe_i)
      beta  <- corr * std_y / std_x
    } else {
      corr <- std_y <- std_x <- beta <- NA_real_
    }

    q9_rows[[length(q9_rows) + 1]] <- list(
      `Dependent variable` = y_var,
      Transformation       = "Two-way fixed effects",
      `Individual name (i)`= ctry,
      `T(i)`               = n_obs,
      `r(Y,X)`             = corr,
      `sigma(Y)`           = std_y,
      `sigma(X)`           = std_x,
      `sigma(Y)/sigma(X)`  = if (!is.na(std_x) && std_x != 0) std_y / std_x else NA_real_,
      `beta = r * sigma(Y)/sigma(X)` = beta,
      Group                = classify_correlation(corr)
    )
  }
}

q9_country_heterogeneity <- bind_rows(q9_rows) %>%
  arrange(`Dependent variable`, Transformation, desc(`r(Y,X)`))

write_xlsx(q9_country_heterogeneity, file.path(TABLES_DIR, "q9_country_heterogeneity_fd_twfe.xlsx"))

q9_group_diagnosis <- q9_country_heterogeneity %>%
  group_by(`Dependent variable`, Transformation, Group) %>%
  summarise(`Number of countries` = n(), .groups = "drop")

write_xlsx(q9_group_diagnosis, file.path(TABLES_DIR, "q9_group_diagnosis_summary.xlsx"))

cat("Question 8 and 9 outputs saved successfully.\n")


# ============================================================
# QUESTION 10: PANEL ESTIMATORS
# ============================================================

dep_vars    <- c("LS", "WR")
expl_vars   <- c("i", "GDP", "UN", "REER")

df_panel <- pdata.frame(df, index = c("country", "year"))

for (dep_var in dep_vars) {

  cat("\n\n====================================================\n")
  cat("RESULTS FOR DEPENDENT VARIABLE:", dep_var, "\n")
  cat("====================================================\n")

  y_col <- dep_var
  formula_base <- as.formula(paste(y_col, "~", paste(expl_vars, collapse = " + ")))

  # --------------------------------------------------------
  # 1. Between estimator
  # --------------------------------------------------------

  between_model   <- plm(formula_base, data = df_panel, model = "between")
  between_summary <- summary(between_model)

  cat("\n================ BETWEEN ESTIMATOR ================\n")
  print(between_summary)

  capture.output(print(between_summary),
    file = file.path(TABLES_DIR, paste0("q10_between_results_", dep_var, ".txt")))

  # --------------------------------------------------------
  # 2. Fixed effects (one-way within)
  # --------------------------------------------------------

  fe_model   <- plm(formula_base, data = df_panel, model = "within", effect = "individual")
  fe_summary <- summary(fe_model)

  cat("\n================ FIXED EFFECTS ESTIMATOR ================\n")
  print(fe_summary)

  capture.output(print(fe_summary),
    file = file.path(TABLES_DIR, paste0("q10_fe_results_", dep_var, ".txt")))

  # --------------------------------------------------------
  # 3. Two-way fixed effects
  # --------------------------------------------------------

  twfe_model   <- plm(formula_base, data = df_panel, model = "within", effect = "twoways")
  twfe_summary <- summary(twfe_model)

  cat("\n================ TWO WAY FIXED EFFECTS ================\n")
  print(twfe_summary)

  capture.output(print(twfe_summary),
    file = file.path(TABLES_DIR, paste0("q10_twfe_results_", dep_var, ".txt")))

  # --------------------------------------------------------
  # 4. First differences
  # --------------------------------------------------------

  fd_model   <- plm(formula_base, data = df_panel, model = "fd")
  fd_summary <- summary(fd_model)

  cat("\n================ FIRST DIFFERENCES ================\n")
  print(fd_summary)

  capture.output(print(fd_summary),
    file = file.path(TABLES_DIR, paste0("q10_fd_results_", dep_var, ".txt")))

  # --------------------------------------------------------
  # 5. Mundlak / Correlated random effects
  # --------------------------------------------------------

  df_mundlak <- df_panel

  for (var in expl_vars) {
    mean_col <- paste0(var, "_mean")
    df_mundlak[[mean_col]] <- Between(df_panel[[var]], effect = "individual")
  }

  mundlak_formula <- as.formula(
    paste(y_col, "~",
          paste(c(expl_vars, paste0(expl_vars, "_mean")), collapse = " + "))
  )

  mundlak_model   <- plm(mundlak_formula, data = df_mundlak, model = "random")
  mundlak_summary <- summary(mundlak_model)

  cat("\n================ MUNDLAK RANDOM EFFECTS ================\n")
  print(mundlak_summary)

  capture.output(print(mundlak_summary),
    file = file.path(TABLES_DIR, paste0("q10_mundlak_results_", dep_var, ".txt")))
}

cat("\n\nAll estimations completed successfully.\n")
