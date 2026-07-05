## Robustness to the composition of test assets.
## This script compares FM, MRRP, and TFRP across random draws of test assets.

# --- configuration ------------------------------------------------------------
MODEL_SPECS <- list(
  intermediary = c("MKT", "ICR"),
  uncertainty = c("MKT", "SMB", "FIN_UNC", "MACRO_UNC"),
  sentiment = c("MKT", "SMB", "BW_ISENT", "HJTZ_ISENT")
)

# change MODEL_NAME to "intermediary", "uncertainty", or "sentiment" to run the robustness test for the corresponding model specification
MODEL_NAME <- "sentiment"
N_RETURNS <- 50
N_SAMPLES <- 10000
SEED <- 1
METHODS <- c("FM", "KRS", "TFRP")
TRADABLE_FACTOR_NAMES <- c("MKT", "SMB", "HML")

RESULTS_DIR <- "inst/results"
FIGURES_DIR <- "inst/figures"

# --- helpers ------------------------------------------------------------------
BuildRobustnessReturns <- function() {
  cbind(
    reproduceTFRP::returns_mebeme25[, -1],
    reproduceTFRP::returns_bemeinv25[, -1],
    reproduceTFRP::returns_bemeop25[, -1],
    reproduceTFRP::returns_ind49[, -1],
    reproduceTFRP::returns_meac25[, -1],
    reproduceTFRP::returns_mebeta25[, -1],
    reproduceTFRP::returns_meinv25[, -1],
    reproduceTFRP::returns_meni35[, -1],
    reproduceTFRP::returns_meop25[, -1],
    reproduceTFRP::returns_mepriorone25[, -1],
    reproduceTFRP::returns_mepriorsixty25[, -1],
    reproduceTFRP::returns_mepriortwelve25[, -1],
    reproduceTFRP::returns_mevar25[, -1],
    reproduceTFRP::returns_opinv25[, -1]
  )
}

ComputeAverageSE <- function(res) {
  avg_se_by_factor_method <- apply(
    res$details$standard_errors,
    c(2, 3),
    mean,
    na.rm = TRUE
  )

  list(
    by_factor_method = as.data.frame(avg_se_by_factor_method),
    overall_by_factor = rowMeans(avg_se_by_factor_method, na.rm = TRUE)
  )
}

SaveRobustnessResults <- function(res, model_name, save_dir) {
  fs::dir_create(save_dir)
  file_path <- file.path(
    save_dir,
    paste0("robustness_test_assets_", model_name, ".rds")
  )
  saveRDS(res, file_path)
  message("Results saved to: ", file_path)
  invisible(file_path)
}

RunRobustnessAnalysis <- function(
    model_name,
    n_returns,
    n_samples,
    rng_seed,
    methods,
    results_dir,
    figures_dir
) {
  selected_factors <- MODEL_SPECS[[model_name]]
  if (is.null(selected_factors)) {
    stop("Unknown model specification: ", model_name)
  }

  factors <- reproduceTFRP::factors51[, -1, drop = FALSE]
  factors <- factors[, selected_factors, drop = FALSE]
  returns <- BuildRobustnessReturns()
  tradable_factors <- intersect(selected_factors, TRADABLE_FACTOR_NAMES)

  res <- EvaluateRiskPremiaByVaryingTestReturns(
    returns = returns,
    n_returns = n_returns,
    n_samples = n_samples,
    factors = factors,
    rng_seed = rng_seed,
    save_results = FALSE,
    save_dir = results_dir
  )

  result_file <- SaveRobustnessResults(res, model_name, results_dir)

  PlotRobustnessOverlaidDensities(
    res = res,
    methods = methods,
    save_dir = figures_dir,
    display_plots = TRUE,
    tradable = tradable_factors,
    factor_series = factors,
    model_name = paste0("(", paste(selected_factors, collapse = ","), ")")
  )

  summary_stats <- ComputeAverageSE(res)

  message("Average standard errors by factor and method:")
  print(round(summary_stats$by_factor_method, 4))

  message("Average standard errors across all methods (per factor):")
  print(round(summary_stats$overall_by_factor, 4))

  invisible(list(
    results = res,
    result_file = result_file,
    average_standard_errors = summary_stats
  ))
}

# --- execution ----------------------------------------------------------------
fs::dir_create(RESULTS_DIR)
fs::dir_create(FIGURES_DIR)

set.seed(SEED)

robustness_output <- RunRobustnessAnalysis(
  model_name = MODEL_NAME,
  n_returns = N_RETURNS,
  n_samples = N_SAMPLES,
  rng_seed = SEED,
  methods = METHODS,
  results_dir = RESULTS_DIR,
  figures_dir = FIGURES_DIR
)

