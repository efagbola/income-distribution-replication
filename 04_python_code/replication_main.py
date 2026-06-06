# ============================================================
# Panel Data Replication Project
# Lofaro and Di Bucchianico (2025)
# ============================================================

from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde, norm

BASE_DIR = Path(__file__).resolve().parents[1]
DATA_DIR = BASE_DIR / "02_original_data"
CLEAN_DATA_DIR = BASE_DIR / "03_clean_data"
TABLES_DIR = BASE_DIR / "08_tables"
FIGURES_DIR = BASE_DIR / "07_figures"

CLEAN_DATA_DIR.mkdir(parents=True, exist_ok=True)
TABLES_DIR.mkdir(parents=True, exist_ok=True)
FIGURES_DIR.mkdir(parents=True, exist_ok=True)

DATA_FILE = DATA_DIR / "Dataset_MP_Impact_functional_Distribution.xlsx"
CLEAN_DATA_FILE = CLEAN_DATA_DIR / "Dataset_MP_Impact_functional_Distribution_clean.xlsx"

# Main variables for the partial replication.
DEPENDENT_VARIABLES = ["WR", "LS"]
KEY_X = "i"

# Variables available in the paper dataset.
ALL_VARIABLES = [
    "i", "P", "W", "WR", "GDP", "LS", "PCOM", "UN",
    "SHORTUN", "LONGUN", "LF", "REER", "SH"
]

VARIABLE_LABELS = {
    "i": "Short-term interest rate, i",
    "P": "GDP deflator / price level, P",
    "W": "Nominal compensation per employee, W",
    "WR": "Dependent variable: Real wages, WR",
    "GDP": "Real GDP, GDP",
    "LS": "Dependent variable: Labor share, LS",
    "PCOM": "Energy commodity price index, PCOM",
    "UN": "Unemployment rate, UN",
    "SHORTUN": "Short-term unemployment, SHORTUN",
    "LONGUN": "Long-term unemployment, LONGUN",
    "LF": "Labor force, LF",
    "REER": "Real effective exchange rate, REER",
    "SH": "Shadow interest rate, SH"
}


if not DATA_FILE.exists():
    raise FileNotFoundError(
        f"Could not find the dataset here:\n{DATA_FILE}\n\n"
        "Check that the Excel file is saved in the data folder."
    )

df = pd.read_excel(DATA_FILE)
df.columns = df.columns.str.strip()
df = df.sort_values(["country", "year"]).reset_index(drop=True)

countries = sorted(df["country"].dropna().unique())
years = sorted(df["year"].dropna().unique())

df.to_excel(CLEAN_DATA_FILE, index=False)

# Keep only variables that exist in the file.
ALL_VARIABLES = [var for var in ALL_VARIABLES if var in df.columns]

print("Dataset loaded successfully.")
print("Rows and columns:", df.shape)
print("Countries:", len(countries))
print("Years:", min(years), "to", max(years))


# Helper functions

def save_text(path, text):
    """Save a text comment that can be copied into the Word document."""
    with open(path, "w", encoding="utf-8") as f:
        f.write(text.strip() + "\n")


def consecutive_blocks(year_list):
    """Find consecutive year blocks for one country."""
    year_list = sorted(year_list)
    if len(year_list) == 0:
        return []

    blocks = []
    start = year_list[0]
    previous = year_list[0]

    for year in year_list[1:]:
        if year == previous + 1:
            previous = year
        else:
            blocks.append((start, previous, previous - start + 1))
            start = year
            previous = year

    blocks.append((start, previous, previous - start + 1))
    return blocks


def normal_density(x_values, mean_value, std_value):
    """Normal density with the same mean and standard deviation as the data."""
    if std_value <= 0 or pd.isna(std_value):
        return np.zeros_like(x_values)
    return (1 / (std_value * np.sqrt(2 * np.pi))) * np.exp(
        -0.5 * ((x_values - mean_value) / std_value) ** 2
    )


def plot_hist_kde_normal(values, title, xlabel, save_path):
    """
    Plot one distribution graph with:
    - histogram
    - KDE line if scipy is available
    - normal law with same mean and standard deviation
    """
    values = pd.Series(values).dropna()

    if len(values) < 3:
        print(f"Not enough observations to plot: {title}")
        return

    mean_value = values.mean()
    std_value = values.std(ddof=1)

    x_values = np.linspace(values.min(), values.max(), 300)

    plt.figure(figsize=(8, 5))
    plt.hist(values, bins=15, density=True, alpha=0.45, label="Histogram")

    if values.nunique() > 1:
        kde = gaussian_kde(values)
        plt.plot(x_values, kde(x_values), label="Kernel density estimate")

    plt.plot(
        x_values,
        normal_density(x_values, mean_value, std_value),
        label="Normal distribution"
    )

    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel("Density")
    plt.grid(True)
    plt.legend()
    plt.tight_layout()
    plt.savefig(save_path, dpi=300)
    plt.close()


def scatter_with_regression(data, x_col, y_col, title, save_path):
    """Simple scatterplot with linear fit and correlation in the title."""
    temp = data[[x_col, y_col]].dropna()

    if len(temp) < 3:
        print(f"Not enough observations to plot: {title}")
        return

    corr = temp[[x_col, y_col]].corr().iloc[0, 1]

    plt.figure(figsize=(8, 5))
    plt.scatter(temp[x_col], temp[y_col], alpha=0.55)

    if temp[x_col].nunique() > 1:
        slope, intercept = np.polyfit(temp[x_col], temp[y_col], 1)
        x_values = np.linspace(temp[x_col].min(), temp[x_col].max(), 200)
        plt.plot(x_values, intercept + slope * x_values, label="Linear fit")
        plt.legend()

    plt.title(f"{title}\nCorrelation = {corr:.3f}")
    plt.xlabel(x_col)
    plt.ylabel(y_col)
    plt.grid(True)
    plt.tight_layout()
    plt.savefig(save_path, dpi=300)
    plt.close()



# At least 3 consecutive observations for each dependent variable
sample_rows = []

for country in countries:
    row = {"Country": country}

    for dep in DEPENDENT_VARIABLES:
        available_years = df.loc[
            (df["country"] == country) & (df[dep].notna()),
            "year"
        ].tolist()

        blocks = consecutive_blocks(available_years)
        max_consecutive = max([b[2] for b in blocks]) if blocks else 0

        row[f"{dep}: number of non-missing observations"] = len(available_years)
        row[f"{dep}: first available year"] = min(available_years) if available_years else np.nan
        row[f"{dep}: last available year"] = max(available_years) if available_years else np.nan
        row[f"{dep}: maximum consecutive observations"] = max_consecutive
        row[f"{dep}: kept in sample"] = max_consecutive >= 3

    sample_rows.append(row)

sample_selection_table = pd.DataFrame(sample_rows)

sample_selection_table.to_excel(
    TABLES_DIR / "sample_selection.xlsx",
    index=False
)


# Compact summary table for the Word template
kept_all = sample_selection_table[
    (sample_selection_table["WR: kept in sample"] == True) &
    (sample_selection_table["LS: kept in sample"] == True)
]

excluded_all = sample_selection_table[
    (sample_selection_table["WR: kept in sample"] == False) |
    (sample_selection_table["LS: kept in sample"] == False)
]

sample_selection_summary = pd.DataFrame({
    "Item": [
        "Total number of countries",
        "Number of excluded countries",
        "Number of kept countries",
        "Excluded countries",
        "Kept countries"
    ],
    "Value": [
        len(countries),
        len(excluded_all),
        len(kept_all),
        ", ".join(excluded_all["Country"].tolist()) if len(excluded_all) > 0 else "None",
        ", ".join(kept_all["Country"].tolist())
    ]
})

sample_selection_summary.to_excel(
    TABLES_DIR / "sample_selection_summary.xlsx",
    index=False
)



# Sample selection within an unbalanced panel
# Number of individuals per date, observations by country, and holes
# For WR and LS, the availability pattern is the same so use WR for the graph and compact tables to avoid duplicated outputs.


dep = "WR"

# Number of countries per year
countries_per_year = (
    df[df[dep].notna()]
    .groupby("year")["country"]
    .nunique()
    .reset_index(name="Number of countries")
)

countries_per_year.to_excel(
    TABLES_DIR / "number_of_countries_per_year.xlsx",
    index=False
)

plt.figure(figsize=(10, 5))
plt.plot(
    countries_per_year["year"],
    countries_per_year["Number of countries"],
    marker="o"
)
plt.xlabel("Year")
plt.ylabel("Number of countries")
plt.title("Number of countries observed per year")
plt.grid(True)
plt.tight_layout()
plt.savefig(
    FIGURES_DIR / "number_of_countries_per_year.png",
    dpi=300
)
plt.close()


# Compact table for the Word template
compact_number_per_date = pd.DataFrame({
    "Date": ["1970-1979", "1980-2019"],
    "N": [14, 15]
})

compact_number_per_date.to_excel(
    TABLES_DIR / "compact_number_of_individuals_per_date.xlsx",
    index=False
)


# Number of observations by country
observations_by_country = (
    df[df[dep].notna()]
    .groupby("country")["year"]
    .nunique()
    .reset_index(name="Number of observations")
    .sort_values("Number of observations", ascending=False)
)

observations_by_country.to_excel(
    TABLES_DIR / "observations_by_country.xlsx",
    index=False
)


# Number of countries with the same number of observations
same_number_of_observations = (
    observations_by_country
    .groupby("Number of observations")["country"]
    .nunique()
    .reset_index(name="Number of countries")
    .sort_values("Number of observations", ascending=False)
)

same_number_of_observations.to_excel(
    TABLES_DIR / "same_number_of_observations.xlsx",
    index=False
)


# Holes / discontinuities inside country-specific time spans
holes_rows = []

for country in countries:
    temp = df[(df["country"] == country) & (df[dep].notna())]
    available_years = sorted(temp["year"].tolist())

    if not available_years:
        continue

    expected_years = list(range(min(available_years), max(available_years) + 1))
    missing_inside = sorted(list(set(expected_years) - set(available_years)))

    holes_rows.append({
        "Country": country,
        "First available year": min(available_years),
        "Last available year": max(available_years),
        "Number of observations": len(available_years),
        "Has holes": len(missing_inside) > 0,
        "Number of missing years inside span": len(missing_inside),
        "Missing years inside span": ", ".join(map(str, missing_inside)) if missing_inside else "None"
    })

holes_table = pd.DataFrame(holes_rows)

holes_table.to_excel(
    TABLES_DIR / "holes_inside_panel.xlsx",
    index=False
)


# Compact holes summary for the Word answer
holes_summary = pd.DataFrame({
    "Item": [
        "Number of countries with holes",
        "Proportion of countries with holes"
    ],
    "Value": [
        int(holes_table["Has holes"].sum()),
        holes_table["Has holes"].mean()
    ]
})

holes_summary.to_excel(
    TABLES_DIR / "holes_summary.xlsx",
    index=False
)



# Within/between variance and 3 variable categories

variance_rows = []

for var in ALL_VARIABLES:
    temp = df[["country", "year", var]].dropna().copy()

    if temp.empty:
        continue

    # Overall variance
    overall_variance = temp[var].var(ddof=1)

    # Between variance: variance of country averages
    country_average = temp.groupby("country")[var].mean()
    between_variance = country_average.var(ddof=1)

    # Within variance: variance of x_it minus country average
    temp["country_average"] = temp.groupby("country")[var].transform("mean")
    temp["within_value"] = temp[var] - temp["country_average"]
    within_variance = temp["within_value"].var(ddof=1)

    within_share = within_variance / overall_variance if overall_variance != 0 else np.nan

    variance_rows.append({
        "Variable": var,
        "Variable in words": VARIABLE_LABELS.get(var, var),
        "N": temp["country"].nunique(),
        "NT": len(temp),
        "NT/N": len(temp) / temp["country"].nunique(),
        "Overall variance": overall_variance,
        "Between variance": between_variance,
        "Within variance": within_variance,
        "Within share": within_share,
        "Within share (%)": within_share * 100
    })

variance_table = pd.DataFrame(variance_rows)
variance_table = variance_table.sort_values("Within share", ascending=False)

# Save full variance table as evidence
variance_table.to_excel(
    TABLES_DIR / "within_between_variance_all_variables.xlsx",
    index=False
)



# Split variables into the 3 categories 
two_index_vars = []
time_invariant_vars = []
individual_invariant_vars = []

for _, row in variance_table.iterrows():
    var = row["Variable"]
    within_share = row["Within share"]

    if pd.isna(within_share):
        continue

    # 0% within variation: time-invariant
    if np.isclose(within_share, 0, atol=1e-6):
        time_invariant_vars.append(var)

    # 100% within variation: common time series
    elif np.isclose(within_share, 1, atol=1e-6):
        individual_invariant_vars.append(var)

    # Between 0 and 100%: varies by country and time
    else:
        two_index_vars.append(var)


# Table 4 in the Word template
variable_categories = pd.DataFrame({
    "List of variables varying with two indices (time and individuals)": [
        ", ".join(two_index_vars) if two_index_vars else "None"
    ],
    "List of time-invariant variables": [
        ", ".join(time_invariant_vars) if time_invariant_vars else "None"
    ],
    "List of individual-invariant variables": [
        ", ".join(individual_invariant_vars) if individual_invariant_vars else "None"
    ],
    "Number K": [len(two_index_vars)],
    "Number K1": [len(time_invariant_vars)],
    "Number K2": [len(individual_invariant_vars)]
})

variable_categories.to_excel(
    TABLES_DIR / "variable_categories.xlsx",
    index=False
)



# Table 5: time-varying variables only
time_varying = variance_table[
    (variance_table["Within share"] > 0) &
    (variance_table["Within share"] < 1)
].copy()

# Put dependent variables first: WR then LS
dependent_rows = time_varying[time_varying["Variable"].isin(DEPENDENT_VARIABLES)].copy()
dependent_rows["order"] = dependent_rows["Variable"].map({"WR": 1, "LS": 2})
dependent_rows = dependent_rows.sort_values("order").drop(columns=["order"])

# Then put the other variables sorted by within share
other_rows = time_varying[~time_varying["Variable"].isin(DEPENDENT_VARIABLES)].copy()
other_rows = other_rows.sort_values("Within share", ascending=False)

time_varying_variables = pd.concat(
    [dependent_rows, other_rows],
    ignore_index=True
)

time_varying_variables = time_varying_variables[[
    "Variable in words",
    "N",
    "NT",
    "NT/N",
    "Overall variance",
    "Between variance",
    "Within variance",
    "Within share (%)"
]]

# Rounded table for Word
time_varying_variables_for_word = time_varying_variables.copy()

for col in ["NT/N", "Overall variance", "Between variance", "Within variance", "Within share (%)"]:
    time_varying_variables_for_word[col] = time_varying_variables_for_word[col].round(2)

time_varying_variables_for_word.to_excel(
    TABLES_DIR / "time_varying_variables_for_word.xlsx",
    index=False
)


# Between and one-way within decomposition

# Create between and within values for WR, LS, and i.
between_within_data = df[["country", "year", "WR", "LS", "i"]].copy()

for var in ["WR", "LS", "i"]:
    between_within_data[f"{var}_between"] = (
        between_within_data.groupby("country")[var].transform("mean")
    )
    between_within_data[f"{var}_within"] = (
        between_within_data[var] - between_within_data[f"{var}_between"]
    )

# Save the transformed dataset used for this section
between_within_data.to_excel(
    TABLES_DIR / "between_within_variables.xlsx",
    index=False
)



# Distribution graphs and descriptive statistics
between_within_stats_rows = []

for var in ["WR", "LS", "i"]:

    # Between transformation: one country average per country
    between_values = df.groupby("country")[var].mean().dropna()

    # Within transformation: x_it minus country average
    within_values = between_within_data[f"{var}_within"].dropna()

    # Between distribution graph
    plot_hist_kde_normal(
        between_values,
        title=f"Between distribution of {var}",
        xlabel=f"{var} country average",
        save_path=FIGURES_DIR / f"between_distribution_{var}.png"
    )

    # Within distribution graph
    plot_hist_kde_normal(
        within_values,
        title=f"One-way within distribution of {var}",
        xlabel=f"{var} minus country average",
        save_path=FIGURES_DIR / f"within_distribution_{var}.png"
    )

    # Descriptive statistics for between and within values
    for transformation, values in [
        ("Between", between_values),
        ("One-way within", within_values)
    ]:
        between_within_stats_rows.append({
            "Variable": var,
            "Transformation": transformation,
            "Observations": len(values),
            "Mean": values.mean(),
            "Median": values.median(),
            "Standard deviation": values.std(ddof=1),
            "Minimum": values.min(),
            "Maximum": values.max(),
            "Skewness": values.skew(),
            "Kurtosis": values.kurtosis()
        })

between_within_stats = pd.DataFrame(between_within_stats_rows)

between_within_stats.to_excel(
    TABLES_DIR / "between_within_descriptive_statistics.xlsx",
    index=False
)


# Bivariate comparison with the key explanatory variable i


# Between comparison: one observation per country
between_country = (
    df.groupby("country")[["WR", "LS", "i"]]
    .mean()
    .reset_index()
    .rename(columns={
        "WR": "WR_between",
        "LS": "LS_between",
        "i": "i_between"
    })
)

correlation_rows = []

for y in ["WR", "LS"]:

    # Correlation between country averages
    corr_between = between_country[[f"{y}_between", "i_between"]].corr().iloc[0, 1]

    # Correlation between within-transformed variables
    corr_within = (
        between_within_data[[f"{y}_within", "i_within"]]
        .dropna()
        .corr()
        .iloc[0, 1]
    )

    correlation_rows.append({
        "Dependent variable": y,
        "Transformation": "Between",
        "Correlation with i": corr_between
    })

    correlation_rows.append({
        "Dependent variable": y,
        "Transformation": "One-way within",
        "Correlation with i": corr_within
    })

    # Scatterplot for between relationship
    scatter_with_regression(
        between_country,
        "i_between",
        f"{y}_between",
        title=f"Between relationship between i and {y}",
        save_path=FIGURES_DIR / f"between_scatter_i_{y}.png"
    )

    # Scatterplot for within relationship
    scatter_with_regression(
        between_within_data,
        "i_within",
        f"{y}_within",
        title=f"One-way within relationship between i and {y}",
        save_path=FIGURES_DIR / f"within_scatter_i_{y}.png"
    )

between_within_correlations = pd.DataFrame(correlation_rows)

between_within_correlations.to_excel(
    TABLES_DIR / "between_within_correlations_with_i.xlsx",
    index=False
)



# ============================================================
# QUESTION 7. FIRST DIFFERENCES AND TWO-WAY FIXED EFFECTS
# ============================================================

# This section continues from Questions 1 to 6 and uses the same cleaned
# panel dataset already loaded above. The aim is to create the first-difference
# and two-way fixed effects transformed variables needed for Question 7.

ID_COL = "country"
TIME_COL = "year"
KEY_VARS_Q7 = ["WR", "LS", "i"]
Y_VARS_Q7 = ["WR", "LS"]
X_VAR_Q7 = "i"


def save_distribution_q7(series, title, filename, xlabel):
    """
    Save a distribution plot with a histogram, a normal density,
    and a kernel density estimate.
    """
    values = series.replace([np.inf, -np.inf], np.nan).dropna()

    if len(values) < 3:
        print(f"Not enough observations to plot: {title}")
        return

    mean_value = values.mean()
    std_value = values.std(ddof=1)

    plt.figure(figsize=(8, 5))
    plt.hist(values, bins=30, density=True, alpha=0.45, label="Histogram")

    if std_value > 0 and values.nunique() > 1:
        x_values = np.linspace(values.min(), values.max(), 250)
        plt.plot(
            x_values,
            normal_density(x_values, mean_value, std_value),
            label="Normal distribution"
        )
        kde = gaussian_kde(values)
        plt.plot(x_values, kde(x_values), label="Kernel density estimate")

    plt.axvline(mean_value, linestyle=":", label="Mean")
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel("Density")
    plt.legend()
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


def save_scatter_with_marginals_q7(data, x_col, y_col, title, filename):
    """
    Save a scatterplot with marginal histograms and a linear fit.
    """
    plot_data = data[[x_col, y_col]].replace([np.inf, -np.inf], np.nan).dropna()

    if len(plot_data) < 3:
        print(f"Not enough observations to plot: {title}")
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
        ax_scatter.plot(x_line, y_line, label=f"Linear fit, corr = {corr:.3f}")
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
    Compute the two-way fixed effects transformation:
    x_it - x_i. - x_.t + x_..
    """
    country_mean = data.groupby(ID_COL)[variable].transform("mean")
    year_mean = data.groupby(TIME_COL)[variable].transform("mean")
    global_mean = data[variable].mean()
    return data[variable] - country_mean - year_mean + global_mean


def save_country_boxplot_q7(data, variable, filename, ylabel):
    """
    Save a country boxplot ordered by within-country variance.
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
    plt.title(f"Two-way fixed effects boxplot of {variable} by country")
    plt.xlabel("Country")
    plt.ylabel(ylabel)
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


def country_correlation_table_q7(data, x_col, y_col):
    """
    Compute country-by-country correlations and simple slopes.
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
        std_x = x.std(ddof=1)
        std_y = y.std(ddof=1)
        simple_slope = correlation * std_y / std_x if std_x != 0 else np.nan

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


# ------------------------------------------------------------
# First differences
# ------------------------------------------------------------

# First differences are computed only within the same country. If the years are
# not consecutive, the difference is kept missing.

q7_data = df.copy()
q7_data["year_gap"] = q7_data.groupby(ID_COL)[TIME_COL].diff()
consecutive_year = q7_data["year_gap"].eq(1)

for var in KEY_VARS_Q7:
    country_difference = q7_data.groupby(ID_COL)[var].diff()
    q7_data[f"d_{var}"] = country_difference.where(consecutive_year)

# Check that the first observation of a new country does not use the last
# observation of the previous country.
country_start_rows = q7_data.index[q7_data[ID_COL].ne(q7_data[ID_COL].shift())].tolist()[:4]
rows_to_check = []

for row in country_start_rows:
    rows_to_check.extend([row - 1, row, row + 1])

rows_to_check = sorted(set(row for row in rows_to_check if 0 <= row < len(q7_data)))

first_difference_boundary_check = q7_data.loc[
    rows_to_check,
    [ID_COL, TIME_COL, "WR", "d_WR", "LS", "d_LS", "i", "d_i"]
]

first_difference_boundary_check.to_excel(
    TABLES_DIR / "q7_first_difference_boundary_check.xlsx",
    index=False
)

fd_vars_q7 = [f"d_{var}" for var in KEY_VARS_Q7]

q7_data[fd_vars_q7].describe().T.to_excel(
    TABLES_DIR / "q7_first_difference_descriptive_statistics.xlsx"
)

q7_data[fd_vars_q7].corr().to_excel(
    TABLES_DIR / "q7_first_difference_correlations.xlsx"
)

for var in KEY_VARS_Q7:
    save_distribution_q7(
        q7_data[f"d_{var}"],
        title=f"First difference distribution of {var}",
        filename=f"q7_fd_distribution_{var}.png",
        xlabel=f"First difference of {var}"
    )

for y_var in Y_VARS_Q7:
    save_scatter_with_marginals_q7(
        q7_data,
        x_col=f"d_{X_VAR_Q7}",
        y_col=f"d_{y_var}",
        title=f"First differences: d_{X_VAR_Q7} and d_{y_var}",
        filename=f"q7_fd_scatter_d_{X_VAR_Q7}_d_{y_var}.png"
    )


# ------------------------------------------------------------
# Balanced panel two-way fixed effects transformation
# ------------------------------------------------------------

complete_data = q7_data.dropna(subset=KEY_VARS_Q7).copy()

obs_by_country = complete_data.groupby(ID_COL)[TIME_COL].nunique()
max_t = obs_by_country.max()

balanced_countries = obs_by_country[obs_by_country == max_t].index.tolist()

twfe_data = complete_data[complete_data[ID_COL].isin(balanced_countries)].copy()

year_counts = twfe_data.groupby(TIME_COL)[ID_COL].nunique()
common_years = year_counts[year_counts == len(balanced_countries)].index.tolist()

twfe_data = twfe_data[twfe_data[TIME_COL].isin(common_years)].copy()
twfe_data = twfe_data.sort_values([ID_COL, TIME_COL]).reset_index(drop=True)

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

for var in KEY_VARS_Q7:
    twfe_data[f"twfe_{var}"] = twfe_transform(twfe_data, var)

twfe_vars_q7 = [f"twfe_{var}" for var in KEY_VARS_Q7]

twfe_data[twfe_vars_q7].describe().T.to_excel(
    TABLES_DIR / "q7_twfe_descriptive_statistics.xlsx"
)

twfe_data[twfe_vars_q7].corr().to_excel(
    TABLES_DIR / "q7_twfe_correlations.xlsx"
)

# Time component removed by TWFE for the monetary policy variable.
year_mean_i = twfe_data.groupby(TIME_COL)[X_VAR_Q7].mean()
global_mean_i = twfe_data[X_VAR_Q7].mean()

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
plt.title("Two-way fixed effects time component for i")
plt.xlabel("Year")
plt.ylabel("minus i_.t plus i_..")
plt.tight_layout()
plt.savefig(FIGURES_DIR / "q7_twfe_time_component_i.png", dpi=300)
plt.close()

for var in KEY_VARS_Q7:
    save_distribution_q7(
        twfe_data[f"twfe_{var}"],
        title=f"Two-way fixed effects distribution of {var}",
        filename=f"q7_twfe_distribution_{var}.png",
        xlabel=f"Two-way fixed effects transformation of {var}"
    )

for y_var in Y_VARS_Q7:
    save_scatter_with_marginals_q7(
        twfe_data,
        x_col=f"twfe_{X_VAR_Q7}",
        y_col=f"twfe_{y_var}",
        title=f"Two-way fixed effects: {X_VAR_Q7} and {y_var}",
        filename=f"q7_twfe_scatter_{X_VAR_Q7}_{y_var}.png"
    )

for var in KEY_VARS_Q7:
    save_country_boxplot_q7(
        twfe_data,
        variable=f"twfe_{var}",
        filename=f"q7_twfe_boxplot_by_country_{var}.png",
        ylabel=f"twfe_{var}"
    )

for y_var in Y_VARS_Q7:
    country_corr = country_correlation_table_q7(
        twfe_data,
        x_col=f"twfe_{X_VAR_Q7}",
        y_col=f"twfe_{y_var}"
    )

    country_corr.to_excel(
        TABLES_DIR / f"q7_twfe_country_correlations_{X_VAR_Q7}_{y_var}.xlsx",
        index=False
    )


# ------------------------------------------------------------
# Unbalanced panel two-way fixed effects transformation
# ------------------------------------------------------------

# This keeps the available unbalanced sample instead of dropping Japan.
# First remove country averages, then remove year effects from those within
# variables. This follows the instruction in the homework template for the
# unbalanced TWFE transformation.

unbalanced_data = q7_data.dropna(subset=KEY_VARS_Q7).copy()

obs_by_country_unbalanced = unbalanced_data.groupby(ID_COL)[TIME_COL].nunique()
countries_to_keep = obs_by_country_unbalanced[obs_by_country_unbalanced > 1].index.tolist()

unbalanced_data = unbalanced_data[unbalanced_data[ID_COL].isin(countries_to_keep)].copy()
unbalanced_data = unbalanced_data.sort_values([ID_COL, TIME_COL]).reset_index(drop=True)

removed_countries = sorted(set(q7_data[ID_COL].dropna().unique()) - set(countries_to_keep))

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

for var in KEY_VARS_Q7:
    within_col = f"within_unbalanced_{var}"
    twfe_col = f"twfe_unbalanced_{var}"

    country_mean = unbalanced_data.groupby(ID_COL)[var].transform("mean")
    unbalanced_data[within_col] = unbalanced_data[var] - country_mean

    year_mean = unbalanced_data.groupby(TIME_COL)[within_col].transform("mean")
    unbalanced_data[twfe_col] = unbalanced_data[within_col] - year_mean

unbalanced_twfe_vars_q7 = [f"twfe_unbalanced_{var}" for var in KEY_VARS_Q7]

unbalanced_data[[ID_COL, TIME_COL] + KEY_VARS_Q7 + unbalanced_twfe_vars_q7].to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_transformed_variables.xlsx",
    index=False
)

unbalanced_data[unbalanced_twfe_vars_q7].describe().T.to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_descriptive_statistics.xlsx"
)

unbalanced_data[unbalanced_twfe_vars_q7].corr().to_excel(
    TABLES_DIR / "q7_unbalanced_twfe_correlations.xlsx"
)

for var in KEY_VARS_Q7:
    save_distribution_q7(
        unbalanced_data[f"twfe_unbalanced_{var}"],
        title=f"Unbalanced TWFE distribution of {var}",
        filename=f"q7_unbalanced_twfe_distribution_{var}.png",
        xlabel=f"Unbalanced TWFE transformation of {var}"
    )

print("Question 7 outputs saved successfully.")

# ============================================
# 8. COMPARISON OF TRANSFORMED VARIABLES
# ============================================

Q8_VARS = ["WR", "LS", "i"]
Y_VARS = ["WR", "LS"]
X_VAR = "i"

# ------------------------------------------------------------
# Q8.1 Build comparison datasets
# ------------------------------------------------------------

# Between: one observation per country
q8_between = between_country[["country"] + [f"{v}_between" for v in Q8_VARS]].copy()

# One-way within: country-year observations
q8_within = between_within_data[
    ["country", "year"] + [f"{v}_within" for v in Q8_VARS]
].copy()

# First differences: country-year observations
q8_fd = q7_data[
    ["country", "year"] + [f"d_{v}" for v in Q8_VARS]
].copy()

# Balanced TWFE: country-year observations
q8_twfe = twfe_data[
    ["country", "year"] + [f"twfe_{v}" for v in Q8_VARS]
].copy()


# ------------------------------------------------------------
# Q8.2 Summary statistics
# ------------------------------------------------------------

q8_summary_rows = []

for var in Q8_VARS:
    transformations = {
        "Between": q8_between[f"{var}_between"],
        "One-way within": q8_within[f"{var}_within"],
        "First differences": q8_fd[f"d_{var}"],
        "Two-way fixed effects": q8_twfe[f"twfe_{var}"],
    }

    for trans_name, series in transformations.items():
        values = series.replace([np.inf, -np.inf], np.nan).dropna()
        std = values.std(ddof=1)

        q8_summary_rows.append({
            "Variable": var,
            "Transformation": trans_name,
            "N": len(values),
            "Mean": values.mean(),
            "Median": values.median(),
            "Standard deviation": std,
            "Standard error": std / np.sqrt(len(values)) if len(values) > 0 else np.nan,
            "Q1": values.quantile(0.25),
            "Q3": values.quantile(0.75),
            "Standardized min": (values.min() - values.mean()) / std if std != 0 else np.nan,
            "Standardized max": (values.max() - values.mean()) / std if std != 0 else np.nan,
        })

q8_summary = pd.DataFrame(q8_summary_rows)
q8_summary.to_excel(TABLES_DIR / "q8_transformation_summary_statistics.xlsx", index=False)


# ------------------------------------------------------------
# Q8.3 Boxplots: Between overall, other transformations by country
# ------------------------------------------------------------

def save_q8_single_boxplot(series, title, ylabel, filename):
    values = pd.Series(series).replace([np.inf, -np.inf], np.nan).dropna()

    if len(values) < 3:
        print(f"Not enough data for graph: {title}")
        return

    plt.figure(figsize=(6, 5))
    plt.boxplot(values, showfliers=True)
    plt.axhline(0, linestyle=":")
    plt.title(title)
    plt.ylabel(ylabel)
    plt.xticks([1], ["All countries"])
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


def save_q8_boxplot_by_country(data, value_col, title, filename):
    plot_data = data[["country", value_col]].replace([np.inf, -np.inf], np.nan).dropna()

    if plot_data.empty:
        return

    country_variance = (
        plot_data.groupby("country")[value_col]
        .var()
        .sort_values(ascending=True)
    )

    ordered_countries = country_variance.index.tolist()

    values_by_country = [
        plot_data.loc[plot_data["country"] == country, value_col]
        for country in ordered_countries
    ]

    plt.figure(figsize=(11, 6))
    plt.boxplot(values_by_country, labels=ordered_countries, showfliers=True)
    plt.xticks(rotation=45, ha="right")
    plt.axhline(0, linestyle=":")
    plt.title(title)
    plt.xlabel("Country")
    plt.ylabel(value_col)
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


for var in Q8_VARS:
    # Between distribution: one country average per country, so one overall boxplot
    save_q8_single_boxplot(
        q8_between[f"{var}_between"],
        title=f"Q8 Between distribution across countries: {var}",
        ylabel=f"{var}_between",
        filename=f"q8_boxplot_between_all_countries_{var}.png"
    )

    # Within, FD and TWFE distributions by country
    save_q8_boxplot_by_country(
        q8_within,
        value_col=f"{var}_within",
        title=f"Q8 One-way within distribution by country: {var}",
        filename=f"q8_boxplot_within_by_country_{var}.png"
    )

    save_q8_boxplot_by_country(
        q8_fd,
        value_col=f"d_{var}",
        title=f"Q8 First differences distribution by country: {var}",
        filename=f"q8_boxplot_fd_by_country_{var}.png"
    )

    save_q8_boxplot_by_country(
        q8_twfe,
        value_col=f"twfe_{var}",
        title=f"Q8 Two-way fixed effects distribution by country: {var}",
        filename=f"q8_boxplot_twfe_by_country_{var}.png"
    )


# ------------------------------------------------------------
# Q8.4 Correlation matrices with trend and lags
# ------------------------------------------------------------

Q8_CORR_VARS = [v for v in ALL_VARIABLES if v in df.columns and v != "PCOM"]

q8_corr_base = df[["country", "year"] + Q8_CORR_VARS].copy()
q8_corr_base["trend"] = q8_corr_base.groupby("country").cumcount() + 1

for var in Q8_CORR_VARS:
    q8_corr_base[f"{var}_lag1"] = q8_corr_base.groupby("country")[var].shift(1)

# Between matrix: country averages + trend average + variable lags
between_corr_data = (
    q8_corr_base
    .groupby("country")
    .mean(numeric_only=True)
    .reset_index()
)

between_corr_cols = (
    Q8_CORR_VARS
    + ["trend"]
    + [f"{v}_lag1" for v in Q8_CORR_VARS]
)

q8_between_full_corr = between_corr_data[between_corr_cols].corr()
q8_between_full_corr.to_excel(TABLES_DIR / "q8_between_full_correlation_matrix_with_trend_lags.xlsx")


# One-way within matrix: remove country means
within_corr_data = q8_corr_base[["country", "year"] + between_corr_cols].copy()

for col in between_corr_cols:
    within_corr_data[f"{col}_within"] = (
        within_corr_data[col] - within_corr_data.groupby("country")[col].transform("mean")
    )

within_corr_cols = [f"{col}_within" for col in between_corr_cols]
q8_within_full_corr = within_corr_data[within_corr_cols].corr()
q8_within_full_corr.to_excel(TABLES_DIR / "q8_within_full_correlation_matrix_with_trend_lags.xlsx")


# FD matrix: include FD variables and lags of FD variables
fd_corr_data = q7_data[["country", "year"]].copy()

for var in Q8_CORR_VARS:
    if f"d_{var}" in q7_data.columns:
        fd_corr_data[f"d_{var}"] = q7_data[f"d_{var}"]
    else:
        fd_corr_data[f"d_{var}"] = q7_data.groupby("country")[var].diff()

    fd_corr_data[f"d_{var}_lag1"] = fd_corr_data.groupby("country")[f"d_{var}"].shift(1)

fd_corr_cols = [f"d_{v}" for v in Q8_CORR_VARS] + [f"d_{v}_lag1" for v in Q8_CORR_VARS]
q8_fd_full_corr = fd_corr_data[fd_corr_cols].corr()
q8_fd_full_corr.to_excel(TABLES_DIR / "q8_fd_full_correlation_matrix_with_lags.xlsx")


# TWFE matrix: balanced TWFE for available variables
twfe_corr_data = twfe_data[["country", "year"]].copy()

for var in Q8_CORR_VARS:
    if f"twfe_{var}" in twfe_data.columns:
        twfe_corr_data[f"twfe_{var}"] = twfe_data[f"twfe_{var}"]
    elif var in twfe_data.columns:
        twfe_corr_data[f"twfe_{var}"] = twfe_transform(twfe_data, var)

twfe_corr_cols = [col for col in twfe_corr_data.columns if col.startswith("twfe_")]
q8_twfe_full_corr = twfe_corr_data[twfe_corr_cols].corr()
q8_twfe_full_corr.to_excel(TABLES_DIR / "q8_twfe_full_correlation_matrix.xlsx")


# Smaller matrices for the report: only WR, LS and i
q8_between_corr = q8_between[[f"{v}_between" for v in Q8_VARS]].corr()
q8_between_corr.to_excel(TABLES_DIR / "q8_between_correlation_matrix.xlsx")

q8_within_corr = q8_within[[f"{v}_within" for v in Q8_VARS]].corr()
q8_within_corr.to_excel(TABLES_DIR / "q8_within_correlation_matrix.xlsx")

q8_fd_corr = q8_fd[[f"d_{v}" for v in Q8_VARS]].corr()
q8_fd_corr.to_excel(TABLES_DIR / "q8_fd_correlation_matrix.xlsx")

q8_twfe_corr = q8_twfe[[f"twfe_{v}" for v in Q8_VARS]].corr()
q8_twfe_corr.to_excel(TABLES_DIR / "q8_twfe_correlation_matrix.xlsx")


# ------------------------------------------------------------
# Q8.5 Autocorrelation, trend correlation, and first 30 FD check
# ------------------------------------------------------------

auto_trend_rows = []

for var in Q8_CORR_VARS:
    temp_auto = q8_corr_base[[var, f"{var}_lag1"]].dropna()
    temp_trend = q8_corr_base[[var, "trend"]].dropna()

    auto_trend_rows.append({
        "Variable": var,
        "Autocorrelation with lag 1": temp_auto[var].corr(temp_auto[f"{var}_lag1"]),
        "N autocorrelation": len(temp_auto),
        "Trend correlation": temp_trend[var].corr(temp_trend["trend"]),
        "N trend correlation": len(temp_trend),
    })

q8_auto_trend = pd.DataFrame(auto_trend_rows)
q8_auto_trend.to_excel(TABLES_DIR / "q8_autocorrelation_and_trend_correlation.xlsx", index=False)


# First 30 observations with FD and lag of FD
q8_fd_lag_check = fd_corr_data[["country", "year", "d_WR", "d_LS", "d_i", "d_WR_lag1", "d_LS_lag1", "d_i_lag1"]].copy()

q8_fd_lag_check.head(30).to_excel(
    TABLES_DIR / "q8_first_30_fd_and_lag_check.xlsx",
    index=False
)
# ------------------------------------------------------------
# Q8.6 Bivariate graphs: linear, quadratic and LOWESS fit
# ------------------------------------------------------------

try:
    from statsmodels.nonparametric.smoothers_lowess import lowess
    HAS_LOWESS = True
except ImportError:
    HAS_LOWESS = False


def save_q8_bivariate_fit(data, x_col, y_col, title, filename):
    plot_data = data[[x_col, y_col]].replace([np.inf, -np.inf], np.nan).dropna()

    if len(plot_data) < 5 or plot_data[x_col].nunique() <= 2:
        print(f"Not enough data for graph: {title}")
        return

    x = plot_data[x_col]
    y = plot_data[y_col]
    corr = x.corr(y)

    x_grid = np.linspace(x.min(), x.max(), 200)

    plt.figure(figsize=(8, 5))
    plt.scatter(x, y, alpha=0.55, label="Observations")

    # Linear fit
    linear_coef = np.polyfit(x, y, 1)
    plt.plot(x_grid, np.polyval(linear_coef, x_grid), label="Linear fit")

    # Quadratic fit
    if x.nunique() > 2:
        quadratic_coef = np.polyfit(x, y, 2)
        plt.plot(x_grid, np.polyval(quadratic_coef, x_grid), label="Quadratic fit")

    # LOWESS fit
    if HAS_LOWESS:
        lowess_result = lowess(y, x, frac=0.3, return_sorted=True)
        plt.plot(lowess_result[:, 0], lowess_result[:, 1], label="LOWESS fit")

    plt.title(f"{title}\nCorrelation = {corr:.3f}")
    plt.xlabel(x_col)
    plt.ylabel(y_col)
    plt.grid(True)
    plt.legend()
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300)
    plt.close()


for y_var in Y_VARS:
    save_q8_bivariate_fit(
        q8_between,
        x_col=f"{X_VAR}_between",
        y_col=f"{y_var}_between",
        title=f"Q8 Between: {X_VAR} and {y_var}",
        filename=f"q8_bivariate_between_{X_VAR}_{y_var}.png"
    )

    save_q8_bivariate_fit(
        q8_within,
        x_col=f"{X_VAR}_within",
        y_col=f"{y_var}_within",
        title=f"Q8 One-way within: {X_VAR} and {y_var}",
        filename=f"q8_bivariate_within_{X_VAR}_{y_var}.png"
    )

    save_q8_bivariate_fit(
        q8_fd,
        x_col=f"d_{X_VAR}",
        y_col=f"d_{y_var}",
        title=f"Q8 First differences: d_{X_VAR} and d_{y_var}",
        filename=f"q8_bivariate_fd_{X_VAR}_{y_var}.png"
    )

    save_q8_bivariate_fit(
        q8_twfe,
        x_col=f"twfe_{X_VAR}",
        y_col=f"twfe_{y_var}",
        title=f"Q8 TWFE: {X_VAR} and {y_var}",
        filename=f"q8_bivariate_twfe_{X_VAR}_{y_var}.png"
    )


# ============================================================
# 9. COUNTRY HETEROGENEITY
# ============================================================

def classify_correlation(corr):
    if pd.isna(corr):
        return "Missing"
    elif corr > 0.08:
        return "Positive"
    elif corr < -0.08:
        return "Negative"
    else:
        return "Weak"


q9_rows = []

for y_var in Y_VARS:

    # First-difference heterogeneity
    for country, country_data in q7_data.groupby("country"):
        sample = country_data[[f"d_{y_var}", "d_i"]].replace([np.inf, -np.inf], np.nan).dropna()

        if len(sample) >= 3 and sample["d_i"].std(ddof=1) != 0:
            corr = sample[f"d_{y_var}"].corr(sample["d_i"])
            std_y = sample[f"d_{y_var}"].std(ddof=1)
            std_x = sample["d_i"].std(ddof=1)
            ratio = std_y / std_x if std_x != 0 else np.nan
            beta = corr * ratio if std_x != 0 else np.nan
        else:
            corr, std_y, std_x, ratio, beta = np.nan, np.nan, np.nan, np.nan, np.nan

        q9_rows.append({
            "Dependent variable": y_var,
            "Transformation": "First differences",
            "Individual name (i)": country,
            "T(i)": len(sample),
            "r(Y,X)": corr,
            "sigma(Y)": std_y,
            "sigma(X)": std_x,
            "sigma(Y)/sigma(X)": ratio,
            "beta = r * sigma(Y)/sigma(X)": beta,
            "Group": classify_correlation(corr),
        })

    # TWFE heterogeneity
    for country, country_data in twfe_data.groupby("country"):
        sample = country_data[[f"twfe_{y_var}", "twfe_i"]].replace([np.inf, -np.inf], np.nan).dropna()

        if len(sample) >= 3 and sample["twfe_i"].std(ddof=1) != 0:
            corr = sample[f"twfe_{y_var}"].corr(sample["twfe_i"])
            std_y = sample[f"twfe_{y_var}"].std(ddof=1)
            std_x = sample["twfe_i"].std(ddof=1)
            ratio = std_y / std_x if std_x != 0 else np.nan
            beta = corr * ratio if std_x != 0 else np.nan
        else:
            corr, std_y, std_x, ratio, beta = np.nan, np.nan, np.nan, np.nan, np.nan

        q9_rows.append({
            "Dependent variable": y_var,
            "Transformation": "Two-way fixed effects",
            "Individual name (i)": country,
            "T(i)": len(sample),
            "r(Y,X)": corr,
            "sigma(Y)": std_y,
            "sigma(X)": std_x,
            "sigma(Y)/sigma(X)": ratio,
            "beta = r * sigma(Y)/sigma(X)": beta,
            "Group": classify_correlation(corr),
        })


q9_country_heterogeneity = pd.DataFrame(q9_rows)

q9_country_heterogeneity = q9_country_heterogeneity.sort_values(
    ["Dependent variable", "Transformation", "r(Y,X)"],
    ascending=[True, True, False]
)

q9_country_heterogeneity.to_excel(
    TABLES_DIR / "q9_country_heterogeneity_fd_twfe.xlsx",
    index=False
)

q9_group_diagnosis = (
    q9_country_heterogeneity
    .groupby(["Dependent variable", "Transformation", "Group"])
    .size()
    .reset_index(name="Number of countries")
)

q9_group_diagnosis.to_excel(
    TABLES_DIR / "q9_group_diagnosis_summary.xlsx",
    index=False
)

print("Question 8 and 9 outputs saved successfully.")