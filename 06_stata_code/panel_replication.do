* ============================================================
* Panel Data Replication Project
* Lofaro and Di Bucchianico (2025)
* Stata translation (with LLM assistance)
*
* KNOWN DIFFICULTIES / DIFFERENCES FROM PYTHON:
* 1. Stata works with one dataset in memory at a time. The Python
*    script builds several dataframes simultaneously (between_country,
*    q7_data, twfe_data, etc.). In Stata we must save, clear, reload,
*    and merge repeatedly. Every section below notes when this happens.
* 2. Stata has no native KDE histogram overlay as clean as matplotlib.
*    We use -kdensity- and -twoway- to approximate it.
* 3. Scatter plots with marginal histograms (save_scatter_with_marginals_q7)
*    cannot be replicated natively; we produce plain scatterplots instead.
* 4. The LOWESS bivariate graphs use -lowess- which is available in
*    Stata but the three-fit overlay (linear + quadratic + LOWESS) must
*    be built with -twoway- combining multiple plot types.
* 5. Correlation heatmaps do not exist natively in Stata. We export the
*    correlation matrices to Excel and note that visualisation requires
*    an external tool or the user-written command -heatplot- (SSC).
* 6. The Mundlak/CRE estimator uses -xtreg, re- with added group means,
*    not a dedicated command. This is equivalent to the Python approach.
* 7. Path handling: Stata uses macros instead of Path objects. Adjust
*    the BASE_DIR macro below to match your folder layout before running.
* 8. Excel export uses -export excel-, which requires Stata 12+.
* 9. -xtset- requires numeric IDs; we encode the string country variable.
* ============================================================


* ============================================================
* 0. USER SETTINGS — adjust before running
* ============================================================

* Set your project root. All other paths are built from this.
* Example: global BASE_DIR "C:/Users/ann/Documents/panel_project"
global BASE_DIR "REPLACE_WITH_YOUR_PROJECT_ROOT"

global DATA_DIR    "$BASE_DIR/02_original_data"
global CLEAN_DIR   "$BASE_DIR/03_clean_data"
global TABLES_DIR  "$BASE_DIR/08_tables"
global FIGURES_DIR "$BASE_DIR/07_figures"

* Create output folders (shell commands — works on Windows and Mac/Linux)
cap mkdir "$CLEAN_DIR"
cap mkdir "$TABLES_DIR"
cap mkdir "$FIGURES_DIR"

global DATA_FILE       "$DATA_DIR/Dataset_MP_Impact_functional_Distribution.xlsx"
global CLEAN_DATA_FILE "$CLEAN_DIR/Dataset_MP_Impact_functional_Distribution_clean.dta"

* Variable lists
global DEPENDENT_VARS  WR LS
global KEY_X           i
global ALL_VARIABLES   i P W WR GDP LS PCOM UN SHORTUN LONGUN LF REER SH
global EXPL_VARS       i GDP UN REER


* ============================================================
* 1. LOAD AND CLEAN DATA
* ============================================================

import excel using "$DATA_FILE", sheet("Sheet1") firstrow clear

* Trim variable names (Stata does not store spaces in varnames,
* but labels might have leading/trailing spaces)
* Sort panel
sort country year

* Save clean version
save "$CLEAN_DATA_FILE", replace

* Keep only variables that exist (drop those not in the sheet)
* DIFFICULTY: Python does this dynamically. In Stata we check manually
* or use a loop with -cap confirm variable-.
foreach var of global ALL_VARIABLES {
    cap confirm variable `var'
    if _rc != 0 {
        di "WARNING: variable `var' not found in dataset — skipping"
    }
}

* Encode string country to numeric ID required by -xtset-
encode country, gen(country_id)
xtset country_id year

di "Dataset loaded."
di "Observations: `=_N'"


* ============================================================
* HELPER: consecutive_blocks equivalent
* We use this logic inline where needed (max run of non-missing obs).
* DIFFICULTY: Python's consecutive_blocks() function has no direct Stata
* equivalent. We approximate it by checking year gaps within country.
* ============================================================


* ============================================================
* QUESTIONS 1–6: SAMPLE SELECTION
* ============================================================

* --- Max consecutive non-missing observations per country × dep var ---
* DIFFICULTY: This requires a within-group run-length calculation.
* We implement it with a loop generating a run counter.

tempfile main_data
save `main_data'

foreach dep in $DEPENDENT_VARS {

    use `main_data', clear
    keep country country_id year `dep'
    sort country_id year

    * Mark non-missing years
    gen nonmiss = !missing(`dep')

    * Run-length counter within country (reset on missing or new country)
    by country_id: gen run = 1 if nonmiss == 1 & (_n == 1 | nonmiss[_n-1] == 0)
    by country_id: replace run = run[_n-1] + 1 if nonmiss == 1 & _n > 1 & nonmiss[_n-1] == 1
    replace run = 0 if missing(run)

    * Max consecutive per country
    by country_id: egen max_consec_`dep' = max(run)
    * Total non-missing
    by country_id: egen n_nonmiss_`dep' = total(nonmiss)
    * First and last available year
    by country_id: egen first_yr_`dep' = min(cond(!missing(`dep'), year, .))
    by country_id: egen last_yr_`dep'  = max(cond(!missing(`dep'), year, .))

    gen kept_`dep' = (max_consec_`dep' >= 3)

    * Keep one row per country
    by country_id: keep if _n == 1
    keep country country_id max_consec_`dep' n_nonmiss_`dep' ///
         first_yr_`dep' last_yr_`dep' kept_`dep'

    tempfile sel_`dep'
    save `sel_`dep''
}

* Merge selection tables
use `sel_WR', clear
merge 1:1 country_id using `sel_LS', nogen

export excel using "$TABLES_DIR/sample_selection.xlsx", ///
    sheet("sample_selection") firstrow(variables) replace

* Summary counts
count
local total_countries = r(N)
count if kept_WR == 0 | kept_LS == 0
local n_excluded = r(N)
count if kept_WR == 1 & kept_LS == 1
local n_kept = r(N)

di "Total countries: `total_countries'"
di "Excluded: `n_excluded'"
di "Kept: `n_kept'"


* ============================================================
* Number of countries per year (WR)
* ============================================================

use `main_data', clear

gen nonmiss_WR = !missing(WR)
preserve
    keep if nonmiss_WR == 1
    collapse (count) n_countries=country_id, by(year)
    export excel using "$TABLES_DIR/number_of_countries_per_year.xlsx", ///
        sheet("countries_per_year") firstrow(variables) replace
    twoway connected n_countries year, ///
        title("Number of countries observed per year") ///
        xtitle("Year") ytitle("Number of countries") ///
        msymbol(circle)
    graph export "$FIGURES_DIR/number_of_countries_per_year.png", replace width(1000)
restore


* ============================================================
* Observations by country and holes
* ============================================================

preserve
    keep if !missing(WR)
    bysort country_id: gen n_obs = _N
    bysort country_id: gen first_yr = year[1]
    bysort country_id: gen last_yr  = year[_N]

    * Detect holes: expected span minus actual obs
    gen span = last_yr - first_yr + 1
    gen n_missing_inside = span - n_obs
    gen has_holes = (n_missing_inside > 0)

    by country_id: keep if _n == 1
    keep country n_obs first_yr last_yr has_holes n_missing_inside

    export excel using "$TABLES_DIR/holes_inside_panel.xlsx", ///
        sheet("holes") firstrow(variables) replace

    * Holes summary
    sum has_holes
    local n_holes    = r(sum)
    local prop_holes = r(mean)
    di "Countries with holes: `n_holes', proportion: `prop_holes'"
restore


* ============================================================
* WITHIN / BETWEEN VARIANCE DECOMPOSITION
* ============================================================
* DIFFICULTY: Stata's -xtsum- gives within/between/overall std dev
* automatically. We use it for all variables and reconstruct variances.

use `main_data', clear

* Build variance table for each variable
tempfile var_table
cap erase `var_table'

foreach var in $ALL_VARIABLES {
    cap confirm variable `var'
    if _rc != 0 continue

    qui xtsum `var'
    local overall_var = r(sd_overall)^2
    local between_var = r(sd_b)^2
    local within_var  = r(sd_w)^2
    local within_share = `within_var' / `overall_var'

    * Append one row
    clear
    set obs 1
    gen variable      = "`var'"
    gen overall_var   = `overall_var'
    gen between_var   = `between_var'
    gen within_var    = `within_var'
    gen within_share  = `within_share'
    gen within_pct    = `within_share' * 100

    cap append using `var_table'
    save `var_table', replace
}

use `var_table', clear
sort within_share
export excel using "$TABLES_DIR/within_between_variance_all_variables.xlsx", ///
    sheet("variance") firstrow(variables) replace


* ============================================================
* Variable categories (time-invariant, individual-invariant, two-index)
* ============================================================
* DIFFICULTY: In Python this is a loop over a DataFrame.
* In Stata we use -xtsum- output directly.

use `main_data', clear

foreach var in $ALL_VARIABLES {
    cap confirm variable `var'
    if _rc != 0 continue
    qui xtsum `var'
    local ws = (r(sd_w)^2) / (r(sd_overall)^2)
    if abs(`ws') < 1e-6 {
        di "`var' -> TIME INVARIANT"
    }
    else if abs(`ws' - 1) < 1e-6 {
        di "`var' -> INDIVIDUAL INVARIANT (common time series)"
    }
    else {
        di "`var' -> TWO-INDEX (varies across countries and time)"
    }
}


* ============================================================
* BETWEEN AND ONE-WAY WITHIN DECOMPOSITION
* ============================================================

use `main_data', clear

foreach var in WR LS i {
    by country_id: egen `var'_between = mean(`var')
    gen `var'_within = `var' - `var'_between
}

save "$TABLES_DIR/between_within_variables.dta", replace


* Distribution plots (histogram + kdensity + normal overlay)
* DIFFICULTY: matplotlib produces these in one call. In Stata we use
* -twoway (histogram) (kdensity) (function)- for the overlay.

foreach var in WR LS i {

    * Between distribution
    preserve
        by country_id: keep if _n == 1
        keep country `var'_between
        drop if missing(`var'_between)

        qui sum `var'_between
        local mu  = r(mean)
        local sig = r(sd)

        twoway ///
            (histogram `var'_between, freq density color(blue%40) lcolor(blue%40)) ///
            (kdensity  `var'_between, lcolor(blue) lwidth(medthick)) ///
            (function  y = normalden(x, `mu', `sig'), ///
                range(`var'_between) lcolor(red) lwidth(medthick)), ///
            title("Between distribution of `var'") ///
            xtitle("`var' country average") ytitle("Density") ///
            legend(order(1 "Histogram" 2 "KDE" 3 "Normal"))
        graph export "$FIGURES_DIR/between_distribution_`var'.png", replace width(800)
    restore

    * Within distribution
    preserve
        keep if !missing(`var'_within)
        qui sum `var'_within
        local mu  = r(mean)
        local sig = r(sd)

        twoway ///
            (histogram `var'_within, freq density color(blue%40) lcolor(blue%40)) ///
            (kdensity  `var'_within, lcolor(blue) lwidth(medthick)) ///
            (function  y = normalden(x, `mu', `sig'), ///
                range(`var'_within) lcolor(red) lwidth(medthick)), ///
            title("One-way within distribution of `var'") ///
            xtitle("`var' minus country average") ytitle("Density") ///
            legend(order(1 "Histogram" 2 "KDE" 3 "Normal"))
        graph export "$FIGURES_DIR/within_distribution_`var'.png", replace width(800)
    restore
}


* Descriptive statistics for between and within
preserve
    * Between: one obs per country
    by country_id: keep if _n == 1
    foreach var in WR LS i {
        qui sum `var'_between, detail
        di "Between `var': mean=`r(mean)', sd=`r(sd)', min=`r(min)', max=`r(max)'"
    }
restore

foreach var in WR LS i {
    qui sum `var'_within, detail
    di "Within `var': mean=`r(mean)', sd=`r(sd)', min=`r(min)', max=`r(max)'"
}


* Bivariate scatterplots (between and within vs i)
preserve
    by country_id: keep if _n == 1
    foreach y in WR LS {
        qui corr `y'_between i_between
        local rho = r(rho)
        twoway (scatter `y'_between i_between) ///
               (lfit   `y'_between i_between), ///
            title("Between relationship between i and `y'" ///
                  "Correlation = `: display %5.3f `rho''") ///
            xtitle("i_between") ytitle("`y'_between") ///
            legend(order(2 "Linear fit"))
        graph export "$FIGURES_DIR/between_scatter_i_`y'.png", replace width(800)
    }
restore

foreach y in WR LS {
    qui corr `y'_within i_within
    local rho = r(rho)
    twoway (scatter `y'_within i_within) ///
           (lfit   `y'_within i_within), ///
        title("One-way within relationship between i and `y'" ///
              "Correlation = `: display %5.3f `rho''") ///
        xtitle("i_within") ytitle("`y'_within") ///
        legend(order(2 "Linear fit"))
    graph export "$FIGURES_DIR/within_scatter_i_`y'.png", replace width(800)
}


* ============================================================
* QUESTION 7: FIRST DIFFERENCES AND TWO-WAY FIXED EFFECTS
* ============================================================

use `main_data', clear
xtset country_id year

* ---- First differences ----
* DIFFICULTY: Python checks year_gap == 1 to avoid cross-country differences.
* Stata's -D.- operator on a panel already respects xtset gaps,
* returning missing for non-consecutive years — same behaviour.

foreach var in WR LS i {
    gen d_`var' = D.`var'
}

* Boundary check: show rows around country transitions
sort country_id year
list country year WR d_WR LS d_LS i d_i in 1/15, sepby(country_id)

export excel country year WR d_WR LS d_LS i d_i using ///
    "$TABLES_DIR/q7_first_difference_boundary_check.xlsx", ///
    sheet("boundary") firstrow(variables) replace

* FD descriptive statistics
foreach var in WR LS i {
    qui sum d_`var', detail
    di "d_`var': N=`r(N)', mean=`r(mean)', sd=`r(sd)', min=`r(min)', max=`r(max)'"
}

* FD distribution plots
foreach var in WR LS i {
    qui sum d_`var', detail
    local mu  = r(mean)
    local sig = r(sd)
    twoway ///
        (histogram d_`var', density color(blue%40) bins(30)) ///
        (kdensity  d_`var') ///
        (function  y = normalden(x, `mu', `sig'), range(d_`var')), ///
        title("First difference distribution of `var'") ///
        xtitle("First difference of `var'") ytitle("Density") ///
        legend(order(1 "Histogram" 2 "KDE" 3 "Normal"))
    graph export "$FIGURES_DIR/q7_fd_distribution_`var'.png", replace width(800)
}

* FD scatterplots
* DIFFICULTY: Scatter with marginal histograms not available natively.
* We produce plain scatterplots with a linear fit.
foreach y in WR LS {
    qui corr d_`y' d_i
    local rho = r(rho)
    twoway (scatter d_`y' d_i) (lfit d_`y' d_i), ///
        title("First differences: d_i and d_`y'" ///
              "Correlation = `: display %5.3f `rho''") ///
        xtitle("d_i") ytitle("d_`y'") legend(order(2 "Linear fit"))
    graph export "$FIGURES_DIR/q7_fd_scatter_d_i_d_`y'.png", replace width(800)
}

tempfile q7_data
save `q7_data'


* ---- Balanced panel TWFE ----
* DIFFICULTY: Python finds the balanced subsample algorithmically.
* In Stata we use -xtbalance- (SSC) or do it manually with -fillin-.
* We implement it manually below.

use `q7_data', clear
drop if missing(WR) | missing(LS) | missing(i)

* Count T per country
by country_id: gen T_i = _N
qui sum T_i
local max_t = r(max)

* Keep only countries with the maximum number of observations
keep if T_i == `max_t'

* Keep only years present for ALL remaining countries
by year: gen n_ctry_yr = _N
qui sum n_ctry_yr
local n_balanced = r(max)   // after filtering, should equal n countries
keep if n_ctry_yr == `n_balanced'

sort country_id year

* TWFE transformation: x_it - x_i. - x_.t + x_..
foreach var in WR LS i {
    by country_id: egen cmean_`var' = mean(`var')
    by year:        egen ymean_`var' = mean(`var')
    qui sum `var'
    local gmean_`var' = r(mean)
    gen twfe_`var' = `var' - cmean_`var' - ymean_`var' + `gmean_`var''
}

* TWFE summary
foreach var in WR LS i {
    qui sum twfe_`var', detail
    di "twfe_`var': mean=`r(mean)', sd=`r(sd)'"
}

* Time component of i
preserve
    by year: keep if _n == 1
    gen time_comp_i = -ymean_i + `gmean_i'
    twoway connected time_comp_i year, ///
        yline(0) title("Two-way fixed effects time component for i") ///
        xtitle("Year") ytitle("minus i_.t plus i_..")
    graph export "$FIGURES_DIR/q7_twfe_time_component_i.png", replace width(900)
restore

* TWFE distributions
foreach var in WR LS i {
    qui sum twfe_`var', detail
    local mu  = r(mean)
    local sig = r(sd)
    twoway ///
        (histogram twfe_`var', density color(blue%40) bins(30)) ///
        (kdensity  twfe_`var') ///
        (function  y = normalden(x, `mu', `sig'), range(twfe_`var')), ///
        title("TWFE distribution of `var'") ///
        xtitle("TWFE transformation of `var'") ytitle("Density") ///
        legend(order(1 "Histogram" 2 "KDE" 3 "Normal"))
    graph export "$FIGURES_DIR/q7_twfe_distribution_`var'.png", replace width(800)
}

* TWFE scatterplots
foreach y in WR LS {
    qui corr twfe_`y' twfe_i
    local rho = r(rho)
    twoway (scatter twfe_`y' twfe_i) (lfit twfe_`y' twfe_i), ///
        title("TWFE: i and `y'" "Correlation = `: display %5.3f `rho''") ///
        xtitle("twfe_i") ytitle("twfe_`y'") legend(order(2 "Linear fit"))
    graph export "$FIGURES_DIR/q7_twfe_scatter_i_`y'.png", replace width(800)
}

* Country boxplots (TWFE)
* DIFFICULTY: Python orders by within-country variance. In Stata we
* produce a single -graph box- command; ordering by variance requires
* pre-sorting and is more involved.
foreach var in WR LS i {
    graph box twfe_`var', over(country, sort(1)) ///
        title("TWFE boxplot of `var' by country") yline(0)
    graph export "$FIGURES_DIR/q7_twfe_boxplot_by_country_`var'.png", replace width(1100)
}

* Country-level correlations (TWFE)
foreach y in WR LS {
    tempfile corr_`y'
    cap erase `corr_`y''
    levelsof country, local(ctry_list)
    foreach ctry of local ctry_list {
        qui corr twfe_`y' twfe_i if country == "`ctry'"
        local rho_ctry = r(rho)
        clear
        set obs 1
        gen country_name = "`ctry'"
        gen correlation  = `rho_ctry'
        cap append using `corr_`y''
        save `corr_`y'', replace
    }
    use `corr_`y'', clear
    sort correlation
    export excel using "$TABLES_DIR/q7_twfe_country_correlations_i_`y'.xlsx", ///
        sheet("corr") firstrow(variables) replace
}

tempfile twfe_data
save `twfe_data'


* ---- Unbalanced panel TWFE ----

use `q7_data', clear
drop if missing(WR) | missing(LS) | missing(i)

* Remove countries with only one observation
by country_id: gen T_unbal = _N
drop if T_unbal <= 1

foreach var in WR LS i {
    by country_id: egen cmean_unbal_`var' = mean(`var')
    gen within_unbal_`var' = `var' - cmean_unbal_`var'
    by year: egen ymean_unbal_`var' = mean(within_unbal_`var')
    gen twfe_unbal_`var' = within_unbal_`var' - ymean_unbal_`var'
}

* Save
export excel country year WR LS i twfe_unbal_WR twfe_unbal_LS twfe_unbal_i ///
    using "$TABLES_DIR/q7_unbalanced_twfe_transformed_variables.xlsx", ///
    sheet("twfe_unbal") firstrow(variables) replace

* Distribution plots
foreach var in WR LS i {
    qui sum twfe_unbal_`var', detail
    local mu  = r(mean)
    local sig = r(sd)
    twoway ///
        (histogram twfe_unbal_`var', density color(blue%40) bins(30)) ///
        (kdensity  twfe_unbal_`var') ///
        (function  y = normalden(x, `mu', `sig'), range(twfe_unbal_`var')), ///
        title("Unbalanced TWFE distribution of `var'") ///
        xtitle("Unbalanced TWFE: `var'") ytitle("Density") ///
        legend(order(1 "Histogram" 2 "KDE" 3 "Normal"))
    graph export "$FIGURES_DIR/q7_unbalanced_twfe_distribution_`var'.png", replace width(800)
}

di "Question 7 outputs saved."


* ============================================================
* QUESTION 8: COMPARISON OF TRANSFORMED VARIABLES
* ============================================================
* DIFFICULTY: Python builds four separate dataframes and loops over them.
* In Stata we must switch datasets in memory for each transformation.
* We use -preserve/restore- and tempfiles throughout.

use `main_data', clear

* Rebuild between and within for Q8
foreach var in WR LS i {
    by country_id: egen `var'_between = mean(`var')
    gen `var'_within = `var' - `var'_between
}

* --- Q8.2 Summary statistics ---
foreach var in WR LS i {
    foreach trans in between within {
        qui sum `var'_`trans', detail
        di "`trans' `var': N=`r(N)', mean=`r(mean)', sd=`r(sd)'"
    }
}

* FD summary
use `q7_data', clear
foreach var in WR LS i {
    qui sum d_`var', detail
    di "FD d_`var': N=`r(N)', mean=`r(mean)', sd=`r(sd)'"
}

* TWFE summary
use `twfe_data', clear
foreach var in WR LS i {
    qui sum twfe_`var', detail
    di "TWFE twfe_`var': N=`r(N)', mean=`r(mean)', sd=`r(sd)'"
}


* --- Q8.3 Boxplots ---

* Between: one value per country — single overall boxplot
use `main_data', clear
foreach var in WR LS i {
    by country_id: egen `var'_between = mean(`var')
}
preserve
    by country_id: keep if _n == 1
    foreach var in WR LS i {
        graph box `var'_between, ///
            title("Q8 Between distribution: `var'") yline(0)
        graph export "$FIGURES_DIR/q8_boxplot_between_all_countries_`var'.png", replace width(600)
    }
restore

* Within by country
foreach var in WR LS i {
    by country_id: egen `var'_within = mean(`var') * 0   // placeholder
    replace `var'_within = `var' - `var'_between
    graph box `var'_within, over(country) ///
        title("Q8 One-way within by country: `var'") yline(0)
    graph export "$FIGURES_DIR/q8_boxplot_within_by_country_`var'.png", replace width(1100)
}

* FD by country
use `q7_data', clear
foreach var in WR LS i {
    graph box d_`var', over(country) ///
        title("Q8 FD by country: `var'") yline(0)
    graph export "$FIGURES_DIR/q8_boxplot_fd_by_country_`var'.png", replace width(1100)
}

* TWFE by country
use `twfe_data', clear
foreach var in WR LS i {
    graph box twfe_`var', over(country) ///
        title("Q8 TWFE by country: `var'") yline(0)
    graph export "$FIGURES_DIR/q8_boxplot_twfe_by_country_`var'.png", replace width(1100)
}


* --- Q8.4 Correlation matrices ---
* DIFFICULTY: Heatmaps are not native in Stata.
* Install -heatplot- from SSC if available: -ssc install heatplot-
* Otherwise export to Excel and visualise there.
* We export correlation matrices and note the limitation.

* Between correlation matrix (WR, LS, i)
use `main_data', clear
foreach var in WR LS i {
    by country_id: egen `var'_between = mean(`var')
}
preserve
    by country_id: keep if _n == 1
    pwcorr WR_between LS_between i_between, star(.05)
    * DIFFICULTY: -pwcorr- output cannot be directly exported.
    * Export via matrix:
    correlate WR_between LS_between i_between
    matrix C = r(C)
    putexcel set "$TABLES_DIR/q8_between_correlation_matrix.xlsx", replace
    putexcel A1 = matrix(C), names
restore

* Within correlation matrix
use `main_data', clear
foreach var in WR LS i {
    by country_id: egen `var'_between = mean(`var')
    gen `var'_within = `var' - `var'_between
}
correlate WR_within LS_within i_within
matrix C = r(C)
putexcel set "$TABLES_DIR/q8_within_correlation_matrix.xlsx", replace
putexcel A1 = matrix(C), names

* FD correlation matrix
use `q7_data', clear
correlate d_WR d_LS d_i
matrix C = r(C)
putexcel set "$TABLES_DIR/q8_fd_correlation_matrix.xlsx", replace
putexcel A1 = matrix(C), names

* TWFE correlation matrix
use `twfe_data', clear
correlate twfe_WR twfe_LS twfe_i
matrix C = r(C)
putexcel set "$TABLES_DIR/q8_twfe_correlation_matrix.xlsx", replace
putexcel A1 = matrix(C), names

* Heatmap note:
di "NOTE: Heatmap visualisation requires -heatplot- (ssc install heatplot)."
di "      Run: heatplot twfe_WR twfe_LS twfe_i after loading each dataset."


* --- Q8.5 Autocorrelation and trend correlation ---

use `main_data', clear
xtset country_id year

* Build trend within country
by country_id: gen trend = _n

foreach var in WR LS i {
    * Lag 1
    gen `var'_lag1 = L.`var'
    * Autocorrelation
    qui corr `var' `var'_lag1
    di "Autocorrelation `var': r = `r(rho)'"
    * Trend correlation
    qui corr `var' trend
    di "Trend correlation `var': r = `r(rho)'"
}

* First 30 FD observations with lags
use `q7_data', clear
foreach var in WR LS i {
    gen d_`var'_lag1 = L.d_`var'
}
list country year d_WR d_LS d_i d_WR_lag1 d_LS_lag1 d_i_lag1 in 1/30
export excel country year d_WR d_LS d_i d_WR_lag1 d_LS_lag1 d_i_lag1 ///
    using "$TABLES_DIR/q8_first_30_fd_and_lag_check.xlsx", ///
    sheet("fd_lag") firstrow(variables) replace


* --- Q8.6 Bivariate graphs: linear + quadratic + LOWESS ---
* DIFFICULTY: Python combines three fits in one figure using matplotlib.
* Stata uses -twoway- with (lfit), (qfit), and (lowess) combined.

* Between
use `main_data', clear
foreach var in WR LS i {
    by country_id: egen `var'_between = mean(`var')
}
preserve
    by country_id: keep if _n == 1
    foreach y in WR LS {
        qui corr `y'_between i_between
        local rho = r(rho)
        twoway ///
            (scatter `y'_between i_between, mcolor(black%55)) ///
            (lfit    `y'_between i_between, lcolor(blue))      ///
            (qfit    `y'_between i_between, lcolor(orange))    ///
            (lowess  `y'_between i_between, lcolor(green) bwidth(.3)), ///
            title("Q8 Between: i and `y'" "Corr = `: display %5.3f `rho''") ///
            xtitle("i_between") ytitle("`y'_between") ///
            legend(order(2 "Linear" 3 "Quadratic" 4 "LOWESS"))
        graph export "$FIGURES_DIR/q8_bivariate_between_i_`y'.png", replace width(800)
    }
restore

* Within
foreach var in WR LS i {
    by country_id: egen `var'_within_q8 = mean(`var') * 0
    by country_id: replace `var'_within_q8 = `var' - `var'_between
}
foreach y in WR LS {
    qui corr `y'_within_q8 i_within_q8
    local rho = r(rho)
    twoway ///
        (scatter `y'_within_q8 i_within_q8, mcolor(black%55)) ///
        (lfit    `y'_within_q8 i_within_q8, lcolor(blue))      ///
        (qfit    `y'_within_q8 i_within_q8, lcolor(orange))    ///
        (lowess  `y'_within_q8 i_within_q8, lcolor(green) bwidth(.3)), ///
        title("Q8 Within: i and `y'" "Corr = `: display %5.3f `rho''") ///
        xtitle("i_within") ytitle("`y'_within") ///
        legend(order(2 "Linear" 3 "Quadratic" 4 "LOWESS"))
    graph export "$FIGURES_DIR/q8_bivariate_within_i_`y'.png", replace width(800)
}

* FD
use `q7_data', clear
foreach y in WR LS {
    qui corr d_`y' d_i
    local rho = r(rho)
    twoway ///
        (scatter d_`y' d_i, mcolor(black%55)) ///
        (lfit    d_`y' d_i, lcolor(blue))      ///
        (qfit    d_`y' d_i, lcolor(orange))    ///
        (lowess  d_`y' d_i, lcolor(green) bwidth(.3)), ///
        title("Q8 FD: d_i and d_`y'" "Corr = `: display %5.3f `rho''") ///
        xtitle("d_i") ytitle("d_`y'") ///
        legend(order(2 "Linear" 3 "Quadratic" 4 "LOWESS"))
    graph export "$FIGURES_DIR/q8_bivariate_fd_i_`y'.png", replace width(800)
}

* TWFE
use `twfe_data', clear
foreach y in WR LS {
    qui corr twfe_`y' twfe_i
    local rho = r(rho)
    twoway ///
        (scatter twfe_`y' twfe_i, mcolor(black%55)) ///
        (lfit    twfe_`y' twfe_i, lcolor(blue))      ///
        (qfit    twfe_`y' twfe_i, lcolor(orange))    ///
        (lowess  twfe_`y' twfe_i, lcolor(green) bwidth(.3)), ///
        title("Q8 TWFE: i and `y'" "Corr = `: display %5.3f `rho''") ///
        xtitle("twfe_i") ytitle("twfe_`y'") ///
        legend(order(2 "Linear" 3 "Quadratic" 4 "LOWESS"))
    graph export "$FIGURES_DIR/q8_bivariate_twfe_i_`y'.png", replace width(800)
}

di "Question 8 outputs saved."


* ============================================================
* QUESTION 9: COUNTRY HETEROGENEITY
* ============================================================

* FD heterogeneity
use `q7_data', clear

foreach y in WR LS {
    tempfile het_fd_`y'
    cap erase `het_fd_`y''
    levelsof country, local(ctry_list)

    foreach ctry of local ctry_list {
        qui count if country == "`ctry'" & !missing(d_`y') & !missing(d_i)
        if r(N) >= 3 {
            qui corr d_`y' d_i if country == "`ctry'"
            local rho = r(rho)
            qui sum d_`y' if country == "`ctry'"
            local sig_y = r(sd)
            qui sum d_i if country == "`ctry'"
            local sig_x = r(sd)
        }
        else {
            local rho   = .
            local sig_y = .
            local sig_x = .
        }

        local slope = cond(!missing(`rho') & `sig_x' != 0, `rho' * `sig_y' / `sig_x', .)

        clear
        set obs 1
        gen dep_var   = "`y'"
        gen trans     = "First differences"
        gen ctry_name = "`ctry'"
        gen corr      = `rho'
        gen sigma_y   = `sig_y'
        gen sigma_x   = `sig_x'
        gen beta      = `slope'
        gen group     = cond(missing(`rho'), "Missing", ///
                            cond(`rho' > .08, "Positive", ///
                            cond(`rho' < -.08, "Negative", "Weak")))

        cap append using `het_fd_`y''
        save `het_fd_`y'', replace
    }
}

* TWFE heterogeneity
use `twfe_data', clear

foreach y in WR LS {
    tempfile het_twfe_`y'
    cap erase `het_twfe_`y''
    levelsof country, local(ctry_list)

    foreach ctry of local ctry_list {
        qui count if country == "`ctry'" & !missing(twfe_`y') & !missing(twfe_i)
        if r(N) >= 3 {
            qui corr twfe_`y' twfe_i if country == "`ctry'"
            local rho = r(rho)
            qui sum twfe_`y' if country == "`ctry'"
            local sig_y = r(sd)
            qui sum twfe_i if country == "`ctry'"
            local sig_x = r(sd)
        }
        else {
            local rho   = .
            local sig_y = .
            local sig_x = .
        }

        local slope = cond(!missing(`rho') & `sig_x' != 0, `rho' * `sig_y' / `sig_x', .)

        clear
        set obs 1
        gen dep_var   = "`y'"
        gen trans     = "Two-way fixed effects"
        gen ctry_name = "`ctry'"
        gen corr      = `rho'
        gen sigma_y   = `sig_y'
        gen sigma_x   = `sig_x'
        gen beta      = `slope'
        gen group     = cond(missing(`rho'), "Missing", ///
                            cond(`rho' > .08, "Positive", ///
                            cond(`rho' < -.08, "Negative", "Weak")))

        cap append using `het_twfe_`y''
        save `het_twfe_`y'', replace
    }
}

* Combine all heterogeneity results
use `het_fd_WR',   clear
append using `het_fd_LS'
append using `het_twfe_WR'
append using `het_twfe_LS'
sort dep_var trans corr

export excel using "$TABLES_DIR/q9_country_heterogeneity_fd_twfe.xlsx", ///
    sheet("heterogeneity") firstrow(variables) replace

* Group diagnosis
contract dep_var trans group, freq(n_countries)
export excel using "$TABLES_DIR/q9_group_diagnosis_summary.xlsx", ///
    sheet("diagnosis") firstrow(variables) replace

di "Question 9 outputs saved."


* ============================================================
* QUESTION 10: PANEL ESTIMATORS
* ============================================================
* DIFFICULTY: Python uses -linearmodels- which mirrors Stata's panel
* commands closely. The Mundlak CRE uses -xtreg, re- with group means
* added manually — identical in logic to the Python version.

use "$CLEAN_DATA_FILE", clear
encode country, gen(country_id)
xtset country_id year

* Drop rows missing any of the key variables
drop if missing(LS) | missing(WR) | missing(i) | missing(GDP) | missing(UN) | missing(REER)

foreach dep_var in LS WR {

    di _newline(2)
    di "===================================================="
    di "RESULTS FOR DEPENDENT VARIABLE: `dep_var'"
    di "===================================================="

    * ----------------------------------------------------------
    * 1. Between estimator
    * ----------------------------------------------------------
    di _newline "======== BETWEEN ESTIMATOR ========"
    xtreg `dep_var' $EXPL_VARS, be
    estimates store between_`dep_var'
    cap log using "$TABLES_DIR/q10_between_results_`dep_var'.txt", text replace
        xtreg `dep_var' $EXPL_VARS, be
    cap log close

    * ----------------------------------------------------------
    * 2. Fixed effects (one-way within)
    * ----------------------------------------------------------
    di _newline "======== FIXED EFFECTS (WITHIN) ========"
    xtreg `dep_var' $EXPL_VARS, fe
    estimates store fe_`dep_var'
    cap log using "$TABLES_DIR/q10_fe_results_`dep_var'.txt", text replace
        xtreg `dep_var' $EXPL_VARS, fe
    cap log close

    * ----------------------------------------------------------
    * 3. Two-way fixed effects
    * DIFFICULTY: Stata's -xtreg, fe- absorbs entity effects only.
    * For TWFE add time dummies explicitly or use -areg- with -absorb-.
    * Here we use -reghdfe- (SSC) which handles both efficiently.
    * Alternative without reghdfe: add i.year dummies manually.
    * ----------------------------------------------------------
    di _newline "======== TWO-WAY FIXED EFFECTS ========"
    cap which reghdfe
    if _rc == 0 {
        reghdfe `dep_var' $EXPL_VARS, absorb(country_id year)
        estimates store twfe_`dep_var'
    }
    else {
        di "NOTE: -reghdfe- not installed. Falling back to xtreg with year dummies."
        di "      Install with: ssc install reghdfe"
        xi: xtreg `dep_var' $EXPL_VARS i.year, fe
        estimates store twfe_`dep_var'
    }
    cap log using "$TABLES_DIR/q10_twfe_results_`dep_var'.txt", text replace
        estimates replay twfe_`dep_var'
    cap log close

    * ----------------------------------------------------------
    * 4. First differences
    * ----------------------------------------------------------
    di _newline "======== FIRST DIFFERENCES ========"
    preserve
        foreach var in `dep_var' $EXPL_VARS {
            gen d_`var'_q10 = D.`var'
        }
        reg d_`dep_var'_q10 d_i_q10 d_GDP_q10 d_UN_q10 d_REER_q10, nocons
        estimates store fd_`dep_var'
    restore
    cap log using "$TABLES_DIR/q10_fd_results_`dep_var'.txt", text replace
        estimates replay fd_`dep_var'
    cap log close

    * ----------------------------------------------------------
    * 5. Mundlak / Correlated Random Effects
    * ----------------------------------------------------------
    di _newline "======== MUNDLAK RANDOM EFFECTS ========"
    preserve
        foreach var in $EXPL_VARS {
            by country_id: egen `var'_mean = mean(`var')
        }
        xtreg `dep_var' $EXPL_VARS i_mean GDP_mean UN_mean REER_mean, re
        estimates store mundlak_`dep_var'
    restore
    cap log using "$TABLES_DIR/q10_mundlak_results_`dep_var'.txt", text replace
        estimates replay mundlak_`dep_var'
    cap log close
}

di _newline "All estimations completed successfully."


* ============================================================
* SUMMARY OF KNOWN STATA DIFFICULTIES
* ============================================================
*
* 1. ONE DATASET IN MEMORY: Stata cannot hold multiple dataframes
*    simultaneously. The script uses tempfiles and preserve/restore
*    throughout, which adds complexity compared to Python.
*
* 2. SCATTER WITH MARGINAL HISTOGRAMS: Not natively available.
*    Produced as plain scatterplots with linear fit. The -plotplain-
*    or -marginsplot- community commands do not replicate this exactly.
*
* 3. CORRELATION HEATMAPS: No native heatmap. Install -heatplot-
*    from SSC (-ssc install heatplot-) for an approximate equivalent.
*
* 4. TWO-WAY FIXED EFFECTS: Requires -reghdfe- (SSC) for clean output.
*    Without it, the script falls back to xi: xtreg with i.year dummies,
*    which works but is slower and produces a large coefficient table.
*
* 5. CONSECUTIVE BLOCKS: Python's consecutive_blocks() is implemented
*    manually using by-group run-length counters, which is more verbose.
*
* 6. DYNAMIC PATH RESOLUTION: Python uses __file__ to find the script
*    location. Stata requires the user to hard-code BASE_DIR at the top.
*
* 7. EXCEL EXPORT: Requires -putexcel- or -export excel-; matrix export
*    for correlation tables uses -putexcel- which needs Stata 13+.
*
* 8. SKEWNESS / KURTOSIS IN DETAIL: Available via -sum, detail- but
*    not exported as a table automatically; manual extraction needed.
*
* ============================================================
