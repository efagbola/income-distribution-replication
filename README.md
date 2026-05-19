# Income Distribution Replication

Partial replication of Lofaro & Di Bucchianico (2025), *Impact of monetary policy on functional income distribution: A panel vector autoregressive analysis*.

This project uses the authors’ provided dataset to produce summary statistics, descriptive plots, and preliminary fixed-effects regressions for labor share and real wages.

## Project Overview

The original paper studies how monetary policy affects functional income distribution using a Panel VAR model. This repository does not fully reproduce the Panel VAR or impulse response functions. Instead, it provides a first-stage replication in Python focused on:

- checking the country-year panel structure
- creating summary statistics
- plotting cross-country averages over time
- estimating preliminary two-way fixed-effects regressions

## Repository Structure

```text
income-distribution-replication/
│
├── README.md
├── requirements.txt
├── lofarodibucchianico_replication_analysis.py
├── Dataset_MP_Impact_functional_Distribution.xlsx
├── replication_log.txt
│
├── outputs/
│   ├── summary_statistics.xlsx
│   ├── summary_statistics.csv
│   ├── clean_fixed_effects_table.xlsx
│   ├── clean_fixed_effects_table.csv
│   ├── average_i_over_time.png
│   ├── average_LS_over_time.png
│   ├── average_WR_over_time.png
│   └── average_UN_over_time.png
```

## Data

The dataset is a country-year panel of 15 advanced economies from 1970 to 2019.

## Main Variables

| Variable | Description |
|---|---|
| `i` | Short-term interest rate |
| `SH` | Shadow interest rate |
| `WR` | Real wage / real compensation measure |
| `LS` | Labor share |
| `GDP` | Real GDP |
| `UN` | Unemployment rate |
| `PCOM` | Commodity price index |
| `REER` | Real effective exchange rate |

## Method

The main fixed-effects specifications are:

```text
LS_it = country FE + year FE + β i_it + controls_it + error_it
```

```text
ln(WR_it) = country FE + year FE + β i_it + controls_it + error_it
```

Controls include `ln_GDP`, `UN`, `PCOM`, and `REER`. Standard errors are clustered by country.

## How to Run

Install dependencies:

```bash
python3 -m pip install -r requirements.txt
```

Run the analysis:

```bash
python3 lofarodibucchianico_replication_analysis.py
```

Outputs are saved in the `outputs/` folder.

## Outputs

Key outputs include:

| File | Description |
|---|---|
| `summary_statistics.xlsx` | Summary statistics for the main variables |
| `clean_fixed_effects_table.xlsx` | Clean regression table |
| `average_i_over_time.png` | Average interest rate over time |
| `average_LS_over_time.png` | Average labor share over time |
| `average_WR_over_time.png` | Average real wage over time |
| `average_UN_over_time.png` | Average unemployment rate over time |

## Status

Completed:

- Data loading
- Panel structure check
- Summary statistics
- Descriptive plots
- Fixed-effects regressions
- Clean regression table

Not yet completed:

- Full Panel VAR replication
- Impulse response functions
- Monte Carlo standard errors

## Limitations

This is a partial replication. The fixed-effects regressions are preliminary and should not be interpreted as causal estimates of monetary policy shocks. The original paper uses a Panel VAR framework, which is not fully reproduced here.

## Reference

Lofaro, A., & Di Bucchianico, S. (2025). *Impact of monetary policy on functional income distribution: A panel vector autoregressive analysis*. Economic Modelling.
