"""
Starter replication script for:
Lofaro & Di Bucchianico (2025)
"Impact of monetary policy on functional income distribution"

What this script does:
1. Loads the Excel dataset.
2. Checks that country-year observations are unique.
3. Creates summary statistics.
4. Plots country-average variables over time.
5. Estimates simple two-way fixed-effects models.

These fixed-effects regressions are NOT the full Panel VAR from the paper.
They are a first-step partial replication to understand the data before moving
to impulse response functions.

Required packages:
    pandas
    numpy
    matplotlib
    statsmodels
    openpyxl

Optional package:
    linearmodels

Install with:
    pip install pandas numpy matplotlib statsmodels openpyxl linearmodels
"""

from pathlib import Path
import warnings

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf


# ---------------------------------------------------------------------
# 1. USER SETTINGS
# ---------------------------------------------------------------------

# Put this Python file in the same folder as the Excel file, or edit DATA_FILE.
DATA_FILE = Path("Dataset_MP_Impact_functional_Distribution.xlsx")

# Output folder
OUTPUT_DIR = Path("python_replication_outputs")
OUTPUT_DIR.mkdir(exist_ok=True)

# Variables for summary statistics
SUMMARY_VARS = [
    "i", "P", "W", "WR", "GDP", "LS", "PCOM", "UN",
    "SHORTUN", "LONGUN", "LF", "REER", "SH"
]

# Variables to plot as cross-country averages over time
PLOT_VARS = ["i", "WR", "LS", "GDP", "UN", "REER"]


# ---------------------------------------------------------------------
# 2. LOAD DATA
# ---------------------------------------------------------------------

if not DATA_FILE.exists():
    raise FileNotFoundError(
        f"Could not find {DATA_FILE}. Put this script in the same folder "
        "as the Excel dataset, or edit DATA_FILE at the top of the script."
    )

df = pd.read_excel(DATA_FILE)

# Clean column names just in case there are spaces
df.columns = [str(c).strip() for c in df.columns]

print("\nData loaded successfully.")
print(f"Rows: {df.shape[0]}")
print(f"Columns: {df.shape[1]}")
print("\nColumns:")
print(df.columns.tolist())

# Basic expected columns check
required_cols = {"year", "country", "i", "WR", "GDP", "LS", "PCOM", "UN", "REER"}
missing = required_cols.difference(df.columns)
if missing:
    raise ValueError(f"Missing required columns: {sorted(missing)}")

# Ensure correct types
df["year"] = pd.to_numeric(df["year"], errors="coerce")
df = df.dropna(subset=["year"]).copy()
df["year"] = df["year"].astype(int)
df["country"] = df["country"].astype(str)

for col in SUMMARY_VARS:
    if col in df.columns:
        df[col] = pd.to_numeric(df[col], errors="coerce")


# ---------------------------------------------------------------------
# 3. PANEL CHECKS
# ---------------------------------------------------------------------

print("\nPanel structure:")
print("Unit variable: country")
print("Time variable: year")

duplicates = df.duplicated(subset=["country", "year"]).sum()
print(f"\nDuplicate country-year observations: {duplicates}")

if duplicates > 0:
    print("\nWarning: duplicated country-year rows found:")
    print(df[df.duplicated(subset=["country", "year"], keep=False)].sort_values(["country", "year"]))

countries = sorted(df["country"].dropna().unique())
print(f"\nNumber of countries: {len(countries)}")
print(countries)

print("\nYears:")
print(f"Min year: {df['year'].min()}")
print(f"Max year: {df['year'].max()}")

panel_counts = df.groupby("country")["year"].agg(["min", "max", "count"])
panel_counts.to_csv(OUTPUT_DIR / "panel_counts_by_country.csv")
print(f"\nSaved panel counts to: {OUTPUT_DIR / 'panel_counts_by_country.csv'}")


# ---------------------------------------------------------------------
# 4. SUMMARY STATISTICS
# ---------------------------------------------------------------------

available_summary_vars = [v for v in SUMMARY_VARS if v in df.columns]

summary_stats = (
    df[available_summary_vars]
    .describe()
    .T[["count", "mean", "std", "min", "25%", "50%", "75%", "max"]]
)

summary_stats.to_csv(OUTPUT_DIR / "summary_statistics.csv")
summary_stats.to_excel(OUTPUT_DIR / "summary_statistics.xlsx")

print("\nSummary statistics:")
print(summary_stats.round(3))
print(f"\nSaved summary stats to: {OUTPUT_DIR / 'summary_statistics.csv'}")
print(f"Saved summary stats to: {OUTPUT_DIR / 'summary_statistics.xlsx'}")


# ---------------------------------------------------------------------
# 5. COUNTRY-AVERAGE PLOTS OVER TIME
# ---------------------------------------------------------------------

avg_by_year = df.groupby("year", as_index=False)[PLOT_VARS].mean(numeric_only=True)
avg_by_year.to_csv(OUTPUT_DIR / "average_by_year.csv", index=False)

print(f"\nSaved yearly averages to: {OUTPUT_DIR / 'average_by_year.csv'}")

for var in PLOT_VARS:
    if var not in avg_by_year.columns:
        continue

    plt.figure(figsize=(8, 5))
    plt.plot(avg_by_year["year"], avg_by_year[var], marker="o", linewidth=1.5)
    plt.title(f"Cross-country average of {var} over time")
    plt.xlabel("Year")
    plt.ylabel(var)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()

    out_path = OUTPUT_DIR / f"average_{var}_over_time.png"
    plt.savefig(out_path, dpi=300)
    plt.close()

    print(f"Saved plot: {out_path}")


# ---------------------------------------------------------------------
# 6. CREATE TRANSFORMED VARIABLES
# ---------------------------------------------------------------------

# Log transformations where values are positive.
# These are common in macro/panel work, but always check the paper's exact definitions.
for col in ["WR", "GDP", "PCOM", "REER", "LF"]:
    if col in df.columns:
        positive = df[col] > 0
        df[f"ln_{col}"] = np.where(positive, np.log(df[col]), np.nan)

# Create one-period lags by country.
# Useful because monetary policy may affect wages/labor share with a delay.
df = df.sort_values(["country", "year"]).copy()

lag_vars = ["i", "ln_GDP", "UN", "PCOM", "ln_PCOM", "REER", "ln_REER", "SH"]
for var in lag_vars:
    if var in df.columns:
        df[f"L1_{var}"] = df.groupby("country")[var].shift(1)


# ---------------------------------------------------------------------
# 7. FIXED-EFFECTS REGRESSIONS
# ---------------------------------------------------------------------

"""
We estimate two-way fixed effects:

    LS_it    = beta * i_it + controls_it + country FE + year FE + error_it
    ln_WR_it = beta * i_it + controls_it + country FE + year FE + error_it

In statsmodels, this is done using:
    C(country) + C(year)

Standard errors are clustered by country.

Interpretation:
- coefficient on i: association between short-term interest rate and outcome,
  conditional on controls, country FE, and year FE.
- This is NOT a causal monetary policy shock estimate and NOT a Panel VAR.
"""

reg_df = df.copy()

# Model formulas.
# Use GDP in logs because it is a scale variable.
formulas = {
    "LS_current": "LS ~ i + ln_GDP + UN + PCOM + REER + C(country) + C(year)",
    "ln_WR_current": "ln_WR ~ i + ln_GDP + UN + PCOM + REER + C(country) + C(year)",
    "LS_lagged": "LS ~ L1_i + L1_ln_GDP + L1_UN + L1_PCOM + L1_REER + C(country) + C(year)",
    "ln_WR_lagged": "ln_WR ~ L1_i + L1_ln_GDP + L1_UN + L1_PCOM + L1_REER + C(country) + C(year)",
}

regression_results = {}

for name, formula in formulas.items():
    print("\n" + "=" * 80)
    print(f"Estimating model: {name}")
    print(formula)

    needed_vars = []
    for token in formula.replace("~", "+").replace("C(country)", "").replace("C(year)", "").split("+"):
        token = token.strip()
        if token and token not in ["country", "year"]:
            needed_vars.append(token)

    model_df = reg_df[["country", "year"] + needed_vars].dropna().copy()
    model_df["year"] = model_df["year"].astype(int)
    model_df["country"] = model_df["country"].astype(str)

    print(f"Observations used: {len(model_df)}")

    if model_df.empty:
        print("Skipping because no observations are available after dropping missing values.")
        continue

    try:
        model = smf.ols(formula=formula, data=model_df)
        result = model.fit(cov_type="cluster", cov_kwds={"groups": model_df["country"]})
        regression_results[name] = result

        print(result.summary())

        # Save full text output
        with open(OUTPUT_DIR / f"{name}_regression_output.txt", "w", encoding="utf-8") as f:
            f.write(result.summary().as_text())

        # Save compact coefficient table
        coef_table = pd.DataFrame({
            "coef": result.params,
            "std_err": result.bse,
            "t": result.tvalues,
            "p_value": result.pvalues,
        })
        coef_table.to_csv(OUTPUT_DIR / f"{name}_coefficients.csv")

        print(f"Saved regression output: {OUTPUT_DIR / f'{name}_regression_output.txt'}")
        print(f"Saved coefficient table: {OUTPUT_DIR / f'{name}_coefficients.csv'}")

    except Exception as e:
        warnings.warn(f"Could not estimate {name}: {e}")


# ---------------------------------------------------------------------
# 8. OPTIONAL: LINEARMODELS PANELOLS VERSION
# ---------------------------------------------------------------------

"""
If linearmodels is installed, this estimates the same type of two-way FE
models using PanelOLS.

This part is optional. The statsmodels version above is enough for a class
starter replication.
"""

try:
    from linearmodels.panel import PanelOLS

    print("\n" + "=" * 80)
    print("Optional PanelOLS estimates using linearmodels")

    panel_df = df.set_index(["country", "year"]).sort_index()

    def run_panelols(dep_var, x_vars, name):
        model_data = panel_df[[dep_var] + x_vars].dropna()
        y = model_data[dep_var]
        X = model_data[x_vars]

        # Add constant manually. Entity and time effects are specified separately.
        X = X.assign(constant=1)

        model = PanelOLS(
            y,
            X,
            entity_effects=True,
            time_effects=True,
            drop_absorbed=True,
        )

        result = model.fit(cov_type="clustered", cluster_entity=True)
        print("\n" + "-" * 80)
        print(name)
        print(result.summary)

        with open(OUTPUT_DIR / f"{name}_PanelOLS_output.txt", "w", encoding="utf-8") as f:
            f.write(str(result.summary))

        return result

    run_panelols(
        dep_var="LS",
        x_vars=["i", "ln_GDP", "UN", "PCOM", "REER"],
        name="LS_current"
    )

    run_panelols(
        dep_var="ln_WR",
        x_vars=["i", "ln_GDP", "UN", "PCOM", "REER"],
        name="ln_WR_current"
    )

except ImportError:
    print("\nlinearmodels is not installed. Skipping optional PanelOLS section.")
    print("To install it, run: pip install linearmodels")

except Exception as e:
    warnings.warn(f"PanelOLS section failed: {e}")


# ---------------------------------------------------------------------
# 9. SAVE CLEANED DATA
# ---------------------------------------------------------------------

df.to_csv(OUTPUT_DIR / "cleaned_dataset_with_logs_and_lags.csv", index=False)

print("\nDone.")
print(f"All outputs saved in folder: {OUTPUT_DIR.resolve()}")


# -------------------------------------------------------------
# Create clean regression table manually from PanelOLS results
# -------------------------------------------------------------

clean_table = pd.DataFrame({
    "Variable": [
        "Interest rate i",
        "",
        "log GDP",
        "",
        "Unemployment UN",
        "",
        "REER",
        "",
        "Country fixed effects",
        "Year fixed effects",
        "Clustered SE",
        "Observations"
    ],
    "Labor share LS": [
        "0.0029",
        "(0.1424)",
        "-1.3048",
        "(4.1835)",
        "-0.1703*",
        "(0.0942)",
        "-0.0245",
        "(0.0211)",
        "Yes",
        "Yes",
        "Country",
        "731"
    ],
    "Log real wage ln_WR": [
        "0.0158*",
        "(0.0088)",
        "-0.1760",
        "(0.6612)",
        "-0.0081",
        "(0.0122)",
        "-0.0048",
        "(0.0032)",
        "Yes",
        "Yes",
        "Country",
        "731"
    ]
})

clean_table.to_excel(OUTPUT_DIR / "clean_fixed_effects_table.xlsx", index=False)
clean_table.to_csv(OUTPUT_DIR / "clean_fixed_effects_table.csv", index=False)

print("\nClean fixed-effects table:")
print(clean_table)