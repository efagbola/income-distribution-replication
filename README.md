# Monetary Policy and Income Distribution Replication

This repository contains a partial replication of Lofaro & Di Bucchianico (2025), *"Impact of monetary policy on functional income distribution: A panel vector autoregressive analysis."*

The project uses the authors' provided dataset to produce summary statistics, descriptive plots, and preliminary two-way fixed-effects regressions for labor share and real wages.

## Project Overview

The original paper studies how monetary policy affects functional income distribution using a Panel VAR framework. This project does not fully reproduce the Panel VAR analysis. Instead, it provides a first-stage partial replication using Python.

The replication focuses on:

- checking the structure of the panel dataset,
- producing descriptive statistics,
- plotting cross-country averages over time,
- estimating preliminary fixed-effects regressions.

## Data

The dataset is a country-year panel covering 15 advanced economies from 1970 to 2019.

Panel structure:

- Unit: `country`
- Time variable: `year`
- Countries: 15
- Years: 1970–2019
- Total observations: 750

Main variables:

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

## Repository Structure

```text
Econometrics_Project/
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
│
