# Replication package for: "Tradable Factor Risk Premia and Oracle Tests of Asset Pricing Models"

Svetlana Bryzgalova, Alberto Quaini, Fabio Trojani, and Ming Yuan.
Journal of Financial Economics, forthcoming.

This README follows the template of the Social Science Data Editors
(https://social-science-data-editors.github.io/template_README/).

## Overview

The replication package is organized as the R package `reproduceTFRP`. It
contains the data, estimation routines, and scripts that reproduce all
empirical results in the main text of the paper (Figures 1-9 and Table 3).
Tables 1 and 2 of the main text are theoretical and involve no computation.
All estimators and tests (TFRP, Oracle TFRP, Fama-MacBeth, the
misspecification-robust two-pass estimator of Kan, Robotti, and Shanken 2013,
the HJ misspecification distance, and the Chen and Fang 2019 beta rank test)
are implemented in the companion R package `intrinsicFRP`
(https://github.com/a91quaini/intrinsicFRP).

The replicator should expect the two model-evaluation scripts
(`results_MEBTM_IND.R`, `results_SSIND.R`) to be the time-consuming step:
each evaluates roughly 80,000 randomized factor models, each involving a
bootstrap rank test, in parallel on all available cores minus one.

## Data availability and provenance statements

All data used in the paper are provided in full in this package. **No pseudo,
simulated, masked, or otherwise altered datasets are used; the files contain
the empirical data used in the analysis.**

Sources:

- **Test-asset returns** (all files in `data-raw/` except
  `F-F_Research_Data_Factors.CSV`): monthly value-weighted portfolio returns
  from the Kenneth French Data Library
  (https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html),
  freely available and redistributable with attribution.
- **Risk-free rate and Fama-French factors** (`data-raw/F-F_Research_Data_Factors.CSV`):
  same source; the risk-free rate `RF` is used to construct excess returns.
- **51 risk factors** (`data/factors51.rda`): the factor zoo compiled by
  Bryzgalova, S., Huang, J., and Julliard, C. (2023), "Bayesian Solutions for
  the Factor Zoo: We Just Ran Two Quadrillion Models," Journal of Finance,
  78(1), 487-557, DOI: 10.1111/jofi.13197. Factor series are standardized to
  unit sample variance, so risk premia map into monthly Sharpe-ratio units.
  The standardized series are included in full; the raw series are available
  from the replication material of the cited paper.

Data construction: `data-raw/make_returns.R` converts each raw return panel
into excess returns by matching each monthly observation to the Fama-French
risk-free rate `RF`, subtracting `RF`, and dividing by 100; restricts the
sample to 1973:10-2016:12; and stores the panels as numeric matrices in
`data/*.rda`. The object `returns_ss` collects the extreme quintile
portfolios (`Lo.20`, `Hi.20`) of the eleven single-sort panels.

### Dataset list

| Data file | Raw source file (`data-raw/`) | Description | Provided |
|---|---|---|---|
| `data/factors51.rda` | none (see provenance above) | 51 factors of Bryzgalova, Huang, and Julliard (2023), unit-variance standardized, 1973:10-2016:12 | Yes |
| `data/returns_ind17.rda` | `17_Industry_Portfolios.CSV` | 17 industry portfolios, excess returns | Yes |
| `data/returns_ind49.rda` | `49_Industry_Portfolios.CSV` | 49 industry portfolios, excess returns | Yes |
| `data/returns_mebeme25.rda` | `25_Portfolios_5x5.CSV` | 25 size/book-to-market portfolios | Yes |
| `data/returns_opinv25.rda` | `25_Portfolios_OP_INV_5x5.CSV` | 25 profitability/investment portfolios | Yes |
| `data/returns_bemeinv25.rda` | `25_Portfolios_BEME_INV_5x5.CSV` | 25 book-to-market/investment portfolios | Yes |
| `data/returns_bemeop25.rda` | `25_Portfolios_BEME_OP_5x5.CSV` | 25 book-to-market/profitability portfolios | Yes |
| `data/returns_meac25.rda` | `25_Portfolios_ME_AC_5x5.csv` | 25 size/accruals portfolios | Yes |
| `data/returns_mebeta25.rda` | `25_Portfolios_ME_BETA_5x5.csv` | 25 size/beta portfolios | Yes |
| `data/returns_meinv25.rda` | `25_Portfolios_ME_INV_5x5.CSV` | 25 size/investment portfolios | Yes |
| `data/returns_meni35.rda` | `25_Portfolios_ME_NI_5x5.csv` | 25 size/net-issuance portfolios | Yes |
| `data/returns_meop25.rda` | `25_Portfolios_ME_OP_5x5.CSV` | 25 size/profitability portfolios | Yes |
| `data/returns_mepriorone25.rda` | `25_Portfolios_ME_Prior_1_0.CSV` | 25 size/short-term-reversal portfolios | Yes |
| `data/returns_mepriorsixty25.rda` | `25_Portfolios_ME_Prior_60_13.CSV` | 25 size/long-term-reversal portfolios | Yes |
| `data/returns_mepriortwelve25.rda` | `25_Portfolios_ME_Prior_12_2.CSV` | 25 size/momentum portfolios | Yes |
| `data/returns_mevar25.rda` | `25_Portfolios_ME_VAR_5x5.csv` | 25 size/variance portfolios | Yes |
| `data/returns_ss.rda` | `Portfolios_Formed_on_{ME, BE-ME, BETA, NI, E-P, D-P, CF-P, OP, VAR, RESVAR, AC}` | 22 single-sorted portfolios: extreme quintiles (`Lo.20`, `Hi.20`) of the 11 listed sorts | Yes |
| `data-raw/F-F_Research_Data_Factors.CSV` | - | Fama-French three factors and risk-free rate (input to `make_returns.R`) | Yes |

## Computational requirements

### Software

- R (>= 4.3).
- R packages: `doParallel`, `foreach`, `fs`, `ggplot2`, `MASS`, `patchwork`,
  `reshape2` (all from CRAN), plus `devtools` to install/load packages from
  source.
- `intrinsicFRP` **version 2.1.0 from GitHub**, commit
  `164e809f15c7667b2e1d4aeef93cd2ede3a5082b`
  (https://github.com/a91quaini/intrinsicFRP).
  Note: the CRAN release of `intrinsicFRP` (0.1.0) predates the estimator
  interfaces used here and will NOT work; the GitHub version is required:

  ```r
  install.packages("devtools")
  devtools::install_github(
    "a91quaini/intrinsicFRP",
    ref = "164e809f15c7667b2e1d4aeef93cd2ede3a5082b"
  )
  ```

- Because `intrinsicFRP` compiles Rcpp/RcppArmadillo code from source, a
  working C++ toolchain is required: Rtools (Windows), Xcode Command Line
  Tools (macOS), or standard build tools (Linux). The `reproduceTFRP` package
  itself is pure R.

### Hardware and runtime

Runtimes below were measured by sourcing each script after
`devtools::load_all(".")` on a MacBook Air with Apple M2, 16 GB memory, macOS
15.6.1, and R 4.6.0. R reported 8 available cores; the two model-evaluation
scripts therefore used 7 parallel workers.

| Program | Runtime |
|---|---:|
| `inst/empirics/results_MEBTM_IND.R` | approx. 3h |
| `inst/empirics/results_SSIND.R` | approx. 2h |
| `inst/empirics/factor_risk_premia_table.R` | <1s |
| `inst/empirics/robustness_test_assets.R`, `MODEL_NAME = "intermediary"` | approx. 30s |
| `inst/empirics/robustness_test_assets.R`, `MODEL_NAME = "uncertainty"` | approx. 30s |
| `inst/empirics/robustness_test_assets.R`, `MODEL_NAME = "sentiment"` | approx. 30s |

The three robustness runs take about 2m in total on this machine. Actual
runtimes may vary with processor load, R version, BLAS/LAPACK configuration,
and the number of cores visible to R.

### Randomness and reproducibility

- All scripts set `set.seed(1)`, and `EvaluateVariousFactorModelsR` seeds the
  parallel workers' RNG streams (`parallel::clusterSetRNGStream`) with
  `rng_seed = 1`.
- The random model draws are executed on parallel workers. Results are
  reproducible across runs on the same machine with the same number of
  workers; minor variation can occur if the number of available cores
  differs.

## Description of programs

- `R/` - package functions:
  - `evaluate_various_random_factor_models.R`: evaluates randomized factor
    models (identification and misspecification tests, MRRP and Oracle TFRP
    estimation, before and after Oracle screening).
  - `evalutate_risk_premia_by_varying_test_returns.R`: FM, MRRP, and TFRP
    estimates across random subsets of test assets, and associated density
    plots.
  - `filter_nontradable_models.R`: filters randomized factor-model results to
    models containing at least one nontradable factor.
  - `plot_functions_empirics.R`: plotting functions used by the empirics
    scripts.
  - `globals.R`: indices of nontradable factors and global variable
    declarations.
- `data-raw/make_returns.R` - builds all `data/returns_*.rda` objects from the
  raw Kenneth French CSV files (already run; provided for provenance).
- `inst/empirics/` - the four scripts that produce all main-text exhibits (see
  table below).
- Figures are written to `inst/figures/`; numerical results and the LaTeX
  table are written to `inst/results/`. Both folders are created by the
  scripts if missing.

## Instructions to replicators

1. Install R (>= 4.3), the CRAN packages listed above, and `intrinsicFRP`
   2.1.0 from GitHub.
2. Open R in the root directory of `reproduceTFRP` (e.g., open
   `reproduceTFRP.Rproj` in RStudio).
3. Install the package, then load it with `devtools::load_all()` (this also
   makes the internal plotting functions available to the scripts):

   ```r
   install.packages(".", repos = NULL, type = "source")
   devtools::load_all(".")
   ```

4. Run the scripts:

   ```r
   source("inst/empirics/results_MEBTM_IND.R")   # Figures 1, 2, 3, 4, 5 (left panels), 6
   source("inst/empirics/results_SSIND.R")       # Figures 2, 3, 4, 5 (right panels)
   source("inst/empirics/factor_risk_premia_table.R")  # Table 3
   source("inst/empirics/robustness_test_assets.R")    # Figures 7-9 (see below)
   ```

5. `robustness_test_assets.R` must be run three times, setting `MODEL_NAME`
   (line 12) to `"intermediary"` (Figure 7), `"uncertainty"` (Figure 8), and
   `"sentiment"` (Figure 9) in turn.

The multi-panel exhibits in the paper (e.g., left/right panels of Figures 2-5,
Panels A-C of Figure 6, and the per-factor panels of Figures 7-9) are
assembled in LaTeX from the individual output files listed below.

## List of tables and programs

All programs are in `inst/empirics/`. Line numbers refer to the lines where
each exhibit is computed and written to disk. Figures are written to
`inst/figures/`, tables and intermediate results to `inst/results/`.

| Exhibit | Program | Lines | Output file(s) |
|---|---|---|---|
| Table 1 | (theoretical, no computation) | - | - |
| Table 2 | (theoretical, no computation) | - | - |
| Figure 1 | `results_MEBTM_IND.R` | 68-74 | `identification_frequency_noscreen_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 2, left panel | `results_MEBTM_IND.R` | 60-66 | `identification_frequency_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 2, right panel | `results_SSIND.R` | 60-66 | `identification_frequency_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 3, left panel | `results_MEBTM_IND.R` | 85-91 | `factor_selection_distribution_frequency_MEBTM25IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 3, right panel | `results_SSIND.R` | 68-74 | `factor_selection_distribution_frequency_SS22IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 4, left panel | `results_MEBTM_IND.R` | 76-83 | `factor_selection_frequency_MEBTM25IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 4, right panel | `results_SSIND.R` | 76-83 | `factor_selection_frequency_SS22IND17_AIC_GCV_keptmkt_high.pdf` |
| Figure 5, left panel | `results_MEBTM_IND.R` | 100-106 | `tradable_risk_premia_10perc_MEBTM25IND17_AIC_GCV_keptmkt.pdf` |
| Figure 5, right panel | `results_SSIND.R` | 92-98 | `tradable_risk_premia_10perc_SS22IND17_AIC_GCV_keptmkt.pdf` |
| Figure 6, Panels A-C | `results_MEBTM_IND.R` | 112-125 | `ranked_risk_premia_{ICR,BEH_PEAD,TERM}_5perc_MEBTM25IND17_AIC_GCV_keptmkt.pdf` |
| Table 3 | `factor_risk_premia_table.R` | 153-159 | `inst/results/factor_risk_premia_table.tex` |
| Figure 7 | `robustness_test_assets.R`, `MODEL_NAME = "intermediary"` (line 12) | 129-137 (plots written at 98-106) | `robustness_N50_density_riskpremia_{MKT,ICR}_(MKT,ICR).png` |
| Figure 8 | `robustness_test_assets.R`, `MODEL_NAME = "uncertainty"` (line 12) | 129-137 (plots written at 98-106) | `robustness_N50_density_riskpremia_{MKT,SMB,FIN_UNC,MACRO_UNC}_(MKT,SMB,FIN_UNC,MACRO_UNC).png` |
| Figure 9 | `robustness_test_assets.R`, `MODEL_NAME = "sentiment"` (line 12) | 129-137 (plots written at 98-106) | `robustness_N50_density_riskpremia_{MKT,SMB,BW_ISENT,HJTZ_ISENT}_(MKT,SMB,BW_ISENT,HJTZ_ISENT).png` |

Intermediate results: the model-evaluation output matrices are saved as
`inst/results/evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt.rda`
(`results_MEBTM_IND.R`, lines 50-53) and
`inst/results/evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt.rda`
(`results_SSIND.R`, lines 50-53); the robustness simulation objects are saved
as `inst/results/robustness_test_assets_{intermediary,uncertainty,sentiment}.rds`.

## References

- Bryzgalova, S., Huang, J., and Julliard, C. (2023). Bayesian Solutions for
  the Factor Zoo: We Just Ran Two Quadrillion Models. Journal of Finance,
  78(1), 487-557. DOI: 10.1111/jofi.13197.
- Chen, Q., and Fang, Z. (2019). Improved inference on the rank of a matrix.
  Quantitative Economics, 10(4), 1787-1824. DOI: 10.3982/QE1139.
- French, K. R. Data Library.
  https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html.
  DOI: not available.
- Kan, R., Robotti, C., and Shanken, J. (2013). Pricing model performance and
  the two-pass cross-sectional regression methodology. Journal of Finance,
  68(6), 2617-2649. DOI: 10.1111/jofi.12035.
- Quaini, A. intrinsicFRP: An R Package for Factor Model Asset Pricing.
  https://github.com/a91quaini/intrinsicFRP. DOI:
  10.32614/CRAN.package.intrinsicFRP.
