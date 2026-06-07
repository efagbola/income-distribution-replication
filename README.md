# Income Distribution Replication

Partial replication of Lofaro & Di Bucchianico (2025), *Impact of monetary policy on functional income distribution: A panel vector autoregressive analysis*.

This repository contains the data, code, figures, tables, and final report for a panel data econometrics replication project using a country-year panel of 15 advanced economies from 1970 to 2019.

## Repository Structure

```text
income-distribution-replication/
│
├── 01_original_paper/       # Original paper PDF
├── 02_original_data/        # Original dataset
├── 03_clean_data/           # Cleaned dataset
├── 04_python_code/          # Main Python replication code
├── 05_r_code/               # R version of the code
├── 06_stata_code/           # Stata version of the code
├── 07_figures/              # Generated figures
├── 08_tables/               # Generated tables
├── 09_final_report/         # Final written report
├── 10_llm_prompts_log/      # LLM prompt and response log
├── README.md
└── requirements.txt
```

## Main Script

```text
04_python_code/replication_main.py
```

The script produces:

- panel structure checks
- sample selection tables
- within/between variance tables
- first-difference transformations
- two-way fixed-effects transformations
- country heterogeneity tables
- panel estimator results
- pre/post-2008 split-sample estimates
- figures and tables used in the final report

## How to Run

```bash
python3 -m pip install -r requirements.txt
python3 04_python_code/replication_main.py
```

Outputs are saved in:

```text
03_clean_data/
07_figures/
08_tables/
```

## Main Variables

| Variable | Description |
|---|---|
| `i` | Short-term interest rate |
| `WR` | Real wages per employee |
| `LS` | Adjusted labor share |
| `GDP` | Real GDP |
| `UN` | Unemployment rate |
| `REER` | Real effective exchange rate |

## Reference

Lofaro, A., & Di Bucchianico, S. (2025). *Impact of monetary policy on functional income distribution: A panel vector autoregressive analysis*. Economic Modelling, 151, 107227. https://doi.org/10.1016/j.econmod.2025.107227
