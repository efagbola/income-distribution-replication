from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde


# Q7. First differences and two way fixed effects
# This script uses the clean dataset produced in Q1 to Q6.
# The purpose is descriptive: we transform the key panel variables
# and then save the tables and figures needed to comment Q7.

BASE_DIR = Path(__file__).resolve().parents[1]

DATA_FILE = BASE_DIR / "03_clean_data" / "Dataset_MP_Impact_functional_Distribution_clean.xlsx"
TABLES_DIR = BASE_DIR / "08_tables"
FIGURES_DIR = BASE_DIR / "07_figures"

TABLES_DIR.mkdir(parents=True, exist_ok=True)
FIGURES_DIR.mkdir(parents=True, exist_ok=True)

ID_COL = "country"
TIME_COL = "year"

KEY_VARS = ["LS", "WR", "i"]
Y_VARS = ["LS", "WR"]
X_VAR = "i"


def normal_density(x_values, mean, std):
    """Normal density using the sample mean and sample standard deviation."""
    return (1 / (std * np.sqrt(2 * np.pi))) * np.exp(
        -0.5 * ((x_values - mean) / std) ** 2
    )


def save_distribution(series, title, filename, xlabel):
    """
    Save a univariate distribution plot.

    The histogram shows the transformed data.
    The normal density is only a benchmark.
    The kernel density gives a smoother view of the empirical distribution.
    """
    values = series.replace([np.inf, -np.inf], np.nan).dropna()

    if values.empty:
        return

    mean = values.mean()
    std = values.std()

    plt.figure(figsize=(8, 5))
    plt.hist(values, bins=30, density=True, alpha=0.45, label="Histogram")

    if std > 0:
        x_values = np.linspace(values.min(), values.max(), 200)

        plt.plot(
            x_values,
            normal_density(x_values, mean, std),
            label="Normal density"
        )

        if values.nunique() > 1:
            kde = gaussian_kde(values)
            plt.plot(
                x_values,
                kde(x_values),
                label="Kernel density estimate"
            )

    plt.axvline(mean, linestyle=":", label="Mean")
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel("Density")
    plt.legend()
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


def save_scatter_with_marginals(data, x_col, y_col, title, filename):
    """
    Save a bivariate graph with marginal histograms.

    The central panel is the scatter plot.
    The top and right panels show the marginal distributions.
    The fitted line and the correlation help compare the transformed variables.
    """
    plot_data = data[[x_col, y_col]].replace([np.inf, -np.inf], np.nan).dropna()

    if plot_data.empty:
        return

    x = plot_data[x_col]
    y = plot_data[y_col]

    fig = plt.figure(figsize=(8, 8))
    grid = fig.add_gridspec(4, 4, hspace=0.05, wspace=0.05)

    ax_scatter = fig.add_subplot(grid[1:, :3])
    ax_hist_x = fig.add_subplot(grid[0, :3], sharex=ax_scatter)
    ax_hist_y = fig.add_subplot(grid[1:, 3], sharey=ax_scatter)

    ax_scatter.scatter(x, y, alpha=0.65)

    if x.nunique() > 1 and y.nunique() > 1:
        slope, intercept = np.polyfit(x, y, 1)
        x_line = np.linspace(x.min(), x.max(), 100)
        y_line = intercept + slope * x_line
        corr = x.corr(y)

        ax_scatter.plot(
            x_line,
            y_line,
            label=f"Linear fit, corr = {corr:.3f}"
        )
        ax_scatter.legend()

    ax_hist_x.hist(x, bins=25)
    ax_hist_y.hist(y, bins=25, orientation="horizontal")

    ax_hist_x.tick_params(labelbottom=False)
    ax_hist_y.tick_params(labelleft=False)

    ax_scatter.set_xlabel(x_col)
    ax_scatter.set_ylabel(y_col)
    fig.suptitle(title)

    fig.subplots_adjust(top=0.92, hspace=0.05, wspace=0.05)
    fig.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close(fig)


def twfe_transform(data, variable):
    """
    Compute the two way fixed effects transformation.

    Formula:
    x_it minus x_i. minus x_.t plus x_..

    This removes country averages and year averages.
    The remaining part is the variation left after both effects are removed.
    """
    country_mean = data.groupby(ID_COL)[variable].transform("mean")
    year_mean = data.groupby(TIME_COL)[variable].transform("mean")
    global_mean = data[variable].mean()

    return data[variable] - country_mean - year_mean + global_mean


if not DATA_FILE.exists():
    raise FileNotFoundError(
        f"Clean dataset not found: {DATA_FILE}. Run replication_main.py first."
    )


df = pd.read_excel(DATA_FILE)
df.columns = df.columns.str.strip()
df = df.sort_values([ID_COL, TIME_COL]).reset_index(drop=True)


# First differences
# We compute changes within the same country.
# When years are not consecutive, the difference is kept missing.
# This avoids subtracting observations that are not true yearly neighbors.

df["year_gap"] = df.groupby(ID_COL)[TIME_COL].diff()
consecutive_year = df["year_gap"].eq(1)

for var in KEY_VARS:
    country_difference = df.groupby(ID_COL)[var].diff()
    df[f"d_{var}"] = country_difference.where(consecutive_year)


# Boundary check
# This table verifies that the first observation of each new country
# does not receive a first difference from the previous country.

country_start_rows = df.index[df[ID_COL].ne(df[ID_COL].shift())].tolist()[:4]
rows_to_check = []

for row in country_start_rows:
    rows_to_check.extend([row - 1, row, row + 1])

rows_to_check = sorted(
    set(row for row in rows_to_check if 0 <= row < len(df))
)

boundary_check = df.loc[
    rows_to_check,
    [ID_COL, TIME_COL, "LS", "d_LS", "WR", "d_WR", "i", "d_i"]
]

boundary_check.to_excel(
    TABLES_DIR / "q7_first_difference_boundary_check.xlsx",
    index=False
)


# Descriptive statistics and correlations for first differences

fd_vars = [f"d_{var}" for var in KEY_VARS]

df[fd_vars].describe().T.to_excel(
    TABLES_DIR / "q7_first_difference_descriptive_statistics.xlsx"
)

df[fd_vars].corr().to_excel(
    TABLES_DIR / "q7_first_difference_correlations.xlsx"
)


# First difference distributions

for var in KEY_VARS:
    save_distribution(
        df[f"d_{var}"],
        title=f"First difference distribution of {var}",
        filename=f"q7_fd_distribution_{var}.png",
        xlabel=f"First difference of {var}"
    )


# First difference bivariate graphs
# We compare the monetary policy variable with both outcomes.

for y_var in Y_VARS:
    save_scatter_with_marginals(
        df,
        x_col=f"d_{X_VAR}",
        y_col=f"d_{y_var}",
        title=f"First differences: d_{X_VAR} and d_{y_var}",
        filename=f"q7_fd_scatter_d_{X_VAR}_d_{y_var}.png"
    )


# Balanced sample for two way fixed effects
# We keep countries with the longest available time span.
# Then we keep only years that are common to those countries.

complete_data = df.dropna(subset=KEY_VARS).copy()

obs_by_country = complete_data.groupby(ID_COL)[TIME_COL].nunique()
max_t = obs_by_country.max()

balanced_countries = obs_by_country[
    obs_by_country == max_t
].index.tolist()

twfe_data = complete_data[
    complete_data[ID_COL].isin(balanced_countries)
].copy()

year_counts = twfe_data.groupby(TIME_COL)[ID_COL].nunique()
common_years = year_counts[
    year_counts == len(balanced_countries)
].index.tolist()

twfe_data = twfe_data[
    twfe_data[TIME_COL].isin(common_years)
].copy()

twfe_data = twfe_data.sort_values([ID_COL, TIME_COL]).reset_index(drop=True)


# Save the sample used for the two way fixed effects part.

balanced_summary = pd.DataFrame({
    "item": [
        "Number of countries",
        "Number of years",
        "First year",
        "Last year",
        "Number of observations"
    ],
    "value": [
        twfe_data[ID_COL].nunique(),
        twfe_data[TIME_COL].nunique(),
        twfe_data[TIME_COL].min(),
        twfe_data[TIME_COL].max(),
        len(twfe_data)
    ]
})

balanced_summary.to_excel(
    TABLES_DIR / "q7_twfe_balanced_sample_summary.xlsx",
    index=False
)

pd.DataFrame({ID_COL: balanced_countries}).to_excel(
    TABLES_DIR / "q7_twfe_balanced_countries.xlsx",
    index=False
)


# Two way fixed effects transformations

for var in KEY_VARS:
    twfe_data[f"twfe_{var}"] = twfe_transform(twfe_data, var)

twfe_vars = [f"twfe_{var}" for var in KEY_VARS]


# Descriptive statistics and correlations after two way fixed effects

twfe_data[twfe_vars].describe().T.to_excel(
    TABLES_DIR / "q7_twfe_descriptive_statistics.xlsx"
)

twfe_data[twfe_vars].corr().to_excel(
    TABLES_DIR / "q7_twfe_correlations.xlsx"
)


# Time component removed by two way fixed effects
# For the monetary policy variable, this is minus i_.t plus i_..
# It helps show the common time pattern that TWFE removes.

year_mean_i = twfe_data.groupby(TIME_COL)[X_VAR].mean()
global_mean_i = twfe_data[X_VAR].mean()

time_component_i = pd.DataFrame({
    TIME_COL: year_mean_i.index,
    "minus_year_mean_plus_global_mean_i": -year_mean_i.values + global_mean_i
})

time_component_i.to_excel(
    TABLES_DIR / "q7_twfe_time_component_i.xlsx",
    index=False
)

plt.figure(figsize=(9, 5))
plt.plot(
    time_component_i[TIME_COL],
    time_component_i["minus_year_mean_plus_global_mean_i"],
    marker="o"
)
plt.axhline(0, linestyle=":")
plt.title("Two way fixed effects time component for i")
plt.xlabel("Year")
plt.ylabel("minus i_.t plus i_..")
plt.tight_layout()
plt.savefig(FIGURES_DIR / "q7_twfe_time_component_i.png", dpi=300)
plt.close()


# Two way fixed effects distributions

for var in KEY_VARS:
    save_distribution(
        twfe_data[f"twfe_{var}"],
        title=f"Two way fixed effects distribution of {var}",
        filename=f"q7_twfe_distribution_{var}.png",
        xlabel=f"Two way fixed effects transformation of {var}"
    )


# Two way fixed effects bivariate graphs

for y_var in Y_VARS:
    save_scatter_with_marginals(
        twfe_data,
        x_col=f"twfe_{X_VAR}",
        y_col=f"twfe_{y_var}",
        title=f"Two way fixed effects: {X_VAR} and {y_var}",
        filename=f"q7_twfe_scatter_{X_VAR}_{y_var}.png"
    )
# Country heterogeneity after two way fixed effects
# The previous graphs looked at the full transformed sample.
# Here we check whether the transformed variables behave similarly across countries.
# This is useful before assuming one common slope for all countries.

def save_country_boxplot(data, variable, filename, ylabel):
    """
    Save a country boxplot ordered by within country variance.

    Countries with larger transformed variance appear first.
    This makes it easier to see whether some countries drive the dispersion.
    """
    boxplot_data = data[[ID_COL, variable]].replace([np.inf, -np.inf], np.nan).dropna()

    if boxplot_data.empty:
        return

    country_variance = (
        boxplot_data
        .groupby(ID_COL)[variable]
        .var()
        .sort_values(ascending=True)
    )

    ordered_countries = country_variance.index.tolist()

    values_by_country = [
        boxplot_data.loc[boxplot_data[ID_COL] == country, variable]
        for country in ordered_countries
    ]

    plt.figure(figsize=(11, 6))
    plt.boxplot(values_by_country, labels=ordered_countries, showfliers=True)
    plt.xticks(rotation=45, ha="right")
    plt.axhline(0, linestyle=":")
    plt.title(f"Two way fixed effects boxplot of {variable} by country")
    plt.xlabel("Country")
    plt.ylabel(ylabel)
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


def country_correlation_table(data, x_col, y_col):
    """
    Compute country by country simple correlations and slopes.

    The slope is descriptive only. It comes from a simple bivariate
    regression inside each country.

    Formula:
    simple slope = correlation * std(Y) / std(X)
    """
    rows = []

    for country, country_data in data.groupby(ID_COL):
        sample = (
            country_data[[x_col, y_col]]
            .replace([np.inf, -np.inf], np.nan)
            .dropna()
        )

        n_obs = len(sample)

        if n_obs < 3 or sample[x_col].nunique() <= 1:
            rows.append({
                ID_COL: country,
                "N": n_obs,
                "correlation": np.nan,
                f"std_{y_col}": np.nan,
                f"std_{x_col}": np.nan,
                "simple_slope": np.nan,
            })
            continue

        x = sample[x_col]
        y = sample[y_col]

        correlation = x.corr(y)
        std_x = x.std()
        std_y = y.std()

        if std_x == 0:
            simple_slope = np.nan
        else:
            simple_slope = correlation * std_y / std_x

        rows.append({
            ID_COL: country,
            "N": n_obs,
            "correlation": correlation,
            f"std_{y_col}": std_y,
            f"std_{x_col}": std_x,
            "simple_slope": simple_slope,
        })

    return (
        pd.DataFrame(rows)
        .sort_values("correlation", ascending=False)
        .reset_index(drop=True)
    )

# Boxplots by country for the transformed variables used in the TWFE part.

for var in KEY_VARS:
    save_country_boxplot(
        twfe_data,
        variable=f"twfe_{var}",
        filename=f"q7_twfe_boxplot_by_country_{var}.png",
        ylabel=f"twfe_{var}"
    )


# Country by country correlations between monetary policy and the two outcomes.
# These tables show whether the bivariate relationship has the same sign everywhere.

for y_var in Y_VARS:
    country_corr = country_correlation_table(
        twfe_data,
        x_col=f"twfe_{X_VAR}",
        y_col=f"twfe_{y_var}"
    )

    country_corr.to_excel(
        TABLES_DIR / f"q7_twfe_country_correlations_{X_VAR}_{y_var}.xlsx",
        index=False
    )
    # Unbalanced two way fixed effects transformation
# This keeps the available unbalanced sample instead of dropping Japan.
# The idea follows the instruction in the template:
# first compute one way within variables, then remove year effects
# by subtracting the yearly mean of each within transformed variable.

unbalanced_data = df.dropna(subset=KEY_VARS).copy()

obs_by_country_unbalanced = unbalanced_data.groupby(ID_COL)[TIME_COL].nunique()

countries_to_keep = obs_by_country_unbalanced[
    obs_by_country_unbalanced > 1
].index.tolist()

unbalanced_data = unbalanced_data[
    unbalanced_data[ID_COL].isin(countries_to_keep)
].copy()

unbalanced_data = (
    unbalanced_data
    .sort_values([ID_COL, TIME_COL])
    .reset_index(drop=True)
)

removed_countries = sorted(
    set(df[ID_COL].dropna().unique()) - set(countries_to_keep)
)

unbalanced_summary = pd.DataFrame({
    "item": [
        "Number of countries",
        "Minimum number of years by country",
        "Maximum number of years by country",
        "First year",
        "Last year",
        "Number of observations",
        "Countries removed because of one observation"
    ],
    "value": [
        unbalanced_data[ID_COL].nunique(),
        obs_by_country_unbalanced.loc[countries_to_keep].min(),
        obs_by_country_unbalanced.loc[countries_to_keep].max(),
        unbalanced_data[TIME_COL].min(),
        unbalanced_data[TIME_COL].max(),
        len(unbalanced_data),
        ", ".join(removed_countries) if removed_countries else "None"
    ]
})

unbalanced_summary.to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_sample_summary.xlsx",
    index=False
)


for var in KEY_VARS:
    within_col = f"within_unbalanced_{var}"
    twfe_col = f"twfe_unbalanced_{var}"

    # Step 1: remove country averages.
    country_mean = unbalanced_data.groupby(ID_COL)[var].transform("mean")
    unbalanced_data[within_col] = unbalanced_data[var] - country_mean

    # Step 2: remove year effects from the within transformed variable.
    # This is equivalent to regressing the within variable on year dummies
    # and collecting the residuals.
    year_mean = unbalanced_data.groupby(TIME_COL)[within_col].transform("mean")
    unbalanced_data[twfe_col] = unbalanced_data[within_col] - year_mean


unbalanced_twfe_vars = [
    f"twfe_unbalanced_{var}" for var in KEY_VARS
]

unbalanced_data[
    [ID_COL, TIME_COL] + KEY_VARS + unbalanced_twfe_vars
].to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_transformed_variables.xlsx",
    index=False
)

unbalanced_data[unbalanced_twfe_vars].describe().T.to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_descriptive_statistics.xlsx"
)

unbalanced_data[unbalanced_twfe_vars].corr().to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_correlations.xlsx"
)


# Distribution plots for the unbalanced TWFE transformed variables.
# These will be used in the next part of the question.

for var in KEY_VARS:
    save_distribution(
        unbalanced_data[f"twfe_unbalanced_{var}"],
        title=f"Unbalanced TWFE distribution of {var}",
        filename=f"q7_unbalanced_twfe_distribution_{var}.png",
        xlabel=f"Unbalanced TWFE transformation of {var}"
    )