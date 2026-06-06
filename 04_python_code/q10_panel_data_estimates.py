from pathlib import Path
import pandas as pd

from linearmodels.panel import PanelOLS, BetweenOLS

# ============================================================
# Paths
# ============================================================

BASE_DIR = Path(__file__).resolve().parents[1]

DATA_FILE = (
    BASE_DIR
    / "03_clean_data"
    / "Dataset_MP_Impact_functional_Distribution_clean.xlsx"
)

OUTPUT_DIR = BASE_DIR / "08_tables"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# ============================================================
# Load data
# ============================================================

df = pd.read_excel(DATA_FILE)

# Panel structure
df = df.set_index(["country", "year"])

# ============================================================
# Variables
# ============================================================

dependent_variables = ["LS", "WR"]
explanatory_variables = ["i", "GDP", "UN", "REER"]

# ============================================================
# Loop over dependent variables
# ============================================================

for dep_var in dependent_variables:

    print("\n\n====================================================")
    print(f"RESULTS FOR DEPENDENT VARIABLE: {dep_var}")
    print("====================================================")

    y = df[dep_var]
    X = df[explanatory_variables]

    # ============================================================
    # 1. BETWEEN ESTIMATOR
    # ============================================================

    between_model = BetweenOLS(y, X)
    between_results = between_model.fit()

    print("\n================ BETWEEN ESTIMATOR ================\n")
    print(between_results.summary)

    with open(OUTPUT_DIR / f"q10_between_results_{dep_var}.txt", "w") as f:
        f.write(str(between_results.summary))

    # ============================================================
    # 2. FIXED EFFECTS (WITHIN)
    # ============================================================

    fe_model = PanelOLS(
        y,
        X,
        entity_effects=True
    )

    fe_results = fe_model.fit()

    print("\n================ FIXED EFFECTS ESTIMATOR ================\n")
    print(fe_results.summary)

    with open(OUTPUT_DIR / f"q10_fe_results_{dep_var}.txt", "w") as f:
        f.write(str(fe_results.summary))

    # ============================================================
    # 3. TWO-WAY FIXED EFFECTS
    # ============================================================

    twfe_model = PanelOLS(
        y,
        X,
        entity_effects=True,
        time_effects=True
    )

    twfe_results = twfe_model.fit()

    print("\n================ TWO WAY FIXED EFFECTS ================\n")
    print(twfe_results.summary)

    with open(OUTPUT_DIR / f"q10_twfe_results_{dep_var}.txt", "w") as f:
        f.write(str(twfe_results.summary))

    with open(OUTPUT_DIR / f"q10_twfe_results_{dep_var}.txt", "w") as f:
        f.write(str(twfe_results.summary))

    # ============================================================
    # 4. FIRST DIFFERENCES
    # ============================================================

    fd_df = df[[dep_var] + explanatory_variables].copy()

    fd_df = fd_df.groupby(level=0).diff()

    y_fd = fd_df[dep_var]
    X_fd = fd_df[explanatory_variables]

    fd_model = PanelOLS(
        y_fd,
        X_fd
    )

    fd_results = fd_model.fit()

    print("\n================ FIRST DIFFERENCES ================\n")
    print(fd_results.summary)

    with open(OUTPUT_DIR / f"q10_fd_results_{dep_var}.txt", "w") as f:
        f.write(str(fd_results.summary))

print("\n\nAll estimations completed successfully.")