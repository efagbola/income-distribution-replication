# ============================================================
# Panel Data Replication Project
# Lofaro and Di Bucchianico (2025)
# ============================================================

from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

try:
    from scipy.stats import gaussian_kde
    SCIPY_AVAILABLE = True
except ImportError:
    SCIPY_AVAILABLE = False
    print("Warning: scipy is not installed, so KDE lines will not be plotted.")
    print("Install it with: pip install scipy")


BASE_DIR = Path(r"C:\Users\Evelyn1\OneDrive - Quant Decisions S.L\Documents\Perso\Econometrics")
DATA_DIR = BASE_DIR / "data"
OUTPUT_DIR = BASE_DIR / "outputs"

TABLES_DIR = OUTPUT_DIR / "tables"
FIGURES_DIR = OUTPUT_DIR / "figures"

TABLES_DIR.mkdir(parents=True, exist_ok=True)
FIGURES_DIR.mkdir(parents=True, exist_ok=True)

DATA_FILE = DATA_DIR / "Dataset_MP_Impact_functional_Distribution.xlsx"

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

df.to_excel(
    DATA_DIR / "Dataset_MP_Impact_functional_Distribution_clean.xlsx",
    index=False
)

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

    if SCIPY_AVAILABLE and values.nunique() > 1:
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


print("\nFinished successfully.")
print("Tables saved in:", TABLES_DIR)
print("Figures saved in:", FIGURES_DIR)