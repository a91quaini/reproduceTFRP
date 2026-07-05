# Author: Alberto Quaini

#' Evaluate
#'
#' `EvaluateVariousFactorModelsR`
#'
#' @param returns returns
#' @param factors all_factors
#' @param penalty_parameters penalty_parameters
#' @param n_evaluated_models n_models
#' @param n_kept_factors n_kept_factors
#' @param weighting_type character specifying the type of adaptive weights:
#' based on the correlation between factors and returns 'c'; based on the
#' regression coefficients of returns on factors 'b'; based on the first-step
#' intrinsic risk premia estimator 'a'; otherwise a vector of ones (any other
#' character). Default is 'c'.
#' @param one_stddev_rule boolean TRUE for picking the most parsimonious model
#' whose score is not higher than one standard error above the score of the
#' best model; FALSE for picking the best model. Default is TRUE.
#' @param gcv_vr_weighting boolean `TRUE` for scaling pricing errors by
#' the inverse variance matrix of asset excess returns; `FALSE` otherwise.
#' Default is `FALSE`.
#' @param gcv_scaling_n_assets (only relevant for `tuning_type ='g'`)
#' boolean `TRUE` for log(n_assets) scaling (`log(n_assets) / n_observations`);
#' `FALSE` otherwise (`1 / n_observations`). Default is `FALSE`.
#' @param gcv_identification_check (only relevant for `tuning_type ='g'`)
#' boolean `TRUE` for a loose check for model identification; `FALSE` otherwise.
#' Default is `FALSE`.
#' @param n_bootstrap_cf2019_rank_test (only relevant if
#' `gcv_identification_check` is `TRUE`) number of bootstrap samples in the
#' Chen Fang 2019 rank test. Default is `500`.
#' @param target_level_kp2006_rank_test (only relevant if `gcv_identification_check` is
#' `TRUE`) numeric level of the Kleibergen Paap 2006 rank test. If it is
#' strictly grater than zero, then the iterative Kleibergen Paap 2006 rank
#' test at `level = target_level_kp2006_rank_test / n_factors` is used to compute an initial estimator
#' of the rank of the factor loadings in the Chen Fang 2019 rank test.
#' Otherwise, the initial rank estimator is taken to be the number of singular
#' values above `n_observations^(-1/4)`. Default is `0.05` (as correction
#' for multiple testing).
#' @param hj_test_level numeric level of the HJ misspecification test.
#' Default is `0.05`.
#' @param rng_seed random number generation seed
#'
#' @return a list containing the values of the HJ-misspecification statistics
#' and the beta rank statistics
#'
#' @export
EvaluateVariousFactorModelsR = function(
  returns,
  factors,
  penalty_parameters,
  n_evaluated_models = 10000,
  n_kept_factors = 0,
  weighting_type = 'c',
  one_stddev_rule = TRUE,
  gcv_vr_weighting = FALSE,
  gcv_scaling_n_assets = FALSE,
  gcv_identification_check = FALSE,
  n_bootstrap_cf2019_rank_test = 500,
  target_level_kp2006_rank_test = .05,
  hj_test_level = .05,
  rng_seed = 1
) {

  n_factors = ncol(factors)
  max_model_size = min(10 - n_kept_factors, n_factors - n_kept_factors)
  list_n_included_factors = seq_len(max_model_size)
  list_n_evaluated_models = c(
    min(n_evaluated_models , n_factors - n_kept_factors),
    ceiling(min(n_evaluated_models, choose(n_factors - n_kept_factors, 2) * 1.3)),
    ceiling(min(n_evaluated_models, choose(n_factors - n_kept_factors, 3) * 1.3)),
    rep(n_evaluated_models, max(0, max_model_size - 3))
  )

  totalCores = parallel::detectCores()
  n_workers = max(1L, as.integer(ifelse(is.na(totalCores[1]), 2L, totalCores[1])) - 1L)
  writeLines(paste("cores =", n_workers))
  cluster <- tryCatch(parallel::makeCluster(n_workers), error = function(e) NULL)
  if (!is.null(cluster)) {
    `%op%` <- foreach::`%dopar%`
    doParallel::registerDoParallel(cluster)
    # seed the parallel workers' RNG streams for reproducibility
    parallel::clusterSetRNGStream(cluster, rng_seed)
  } else {
    `%op%` <- foreach::`%do%`
  }


  set.seed(rng_seed)

  start_time <- Sys.time()
  output = foreach::foreach(
    idx=1:length(list_n_included_factors),
    .combine='rbind',
    .packages = "reproduceTFRP",
    .export = "EvaluateVariousFactorModels"
    # ) %do% {
  ) %op% {

# for (idx in 1:length(list_n_included_factors)) {
#
#   EvaluateVariousFactorModels(
#     returns,
#     factors,
#     penalty_parameters,
#     list_n_included_factors[idx],
#     list_n_evaluated_models[idx],
#     n_kept_factors,
#     weighting_type,
#     one_stddev_rule,
#     gcv_vr_weighting,
#     gcv_scaling_n_assets,
#     gcv_identification_check,
#     n_bootstrap_cf2019_rank_test,
#     target_level_kp2006_rank_test
#   )
#
# }
    EvaluateVariousFactorModels(
      returns,
      factors,
      penalty_parameters,
      list_n_included_factors[idx],
      list_n_evaluated_models[idx],
      n_kept_factors,
      weighting_type,
      one_stddev_rule,
      gcv_scaling_n_assets,
      gcv_identification_check,
      n_bootstrap_cf2019_rank_test,
      target_level_kp2006_rank_test,
      hj_test_level
    )

  }

  if (!is.null(cluster)) {
    parallel::stopCluster(cluster)
  }

  end_time <- Sys.time()

  writeLines(paste("Computation time =", end_time - start_time))

  # turn cpp indices to r indices
  output[,8:27] = output[,8:27] + 1

  colnames(output) = c(
    "n_factors",
    "n_selected_factors",
    "penalty_parameter",
    "hj_misspecification_pvalue",
    "beta_rank_pvalue_cf2019",
    "hj_misspecification_pvalue_screened",
    "beta_rank_pvalue_cf2019_screened",
    paste("included_", 1:10, sep=""),
    paste("selected_", 1:10, sep=""),
    paste("risk_premia_included_", 1:10, sep=""),
    paste("risk_premia_selected_", 1:10, sep=""),
    paste("se_risk_premia_included_", 1:10, sep=""),
    paste("se_risk_premia_selected_", 1:10, sep=""),
    "sr_mimicking_factors",
    "sr_mimicking_factors_screened"
  )

  return(output)
}

EvaluateVariousFactorModels = function(
  returns,
  factors,
  penalty_parameters,
  n_included_factors,
  n_evaluated_models,
  n_kept_factors,
  weighting_type = "c",
  one_stddev_rule = TRUE,
  gcv_scaling_n_assets = FALSE,
  gcv_identification_check = FALSE,
  n_bootstrap_cf2019_rank_test = 500,
  target_level_kp2006_rank_test = 0.05,
  hj_test_level = 0.05
) {

  SafeSolve = function(a, b) {
    tryCatch(
      solve(a, b),
      error = function(e) {
        tryCatch(
          qr.solve(a, b),
          error = function(e2) MASS::ginv(a) %*% b
        )
      }
    )
  }

  EvaluateFactorModelSpecificationAndIdentification = function(
    returns,
    factors,
    n_bootstrap_cf2019_rank_test,
    target_level_kp2006_rank_test,
    hj_test_level
  ) {

    hj_output = intrinsicFRP::HJMisspecificationDistance(
      returns,
      factors,
      ci_coverage = 1. - hj_test_level,
      hac_prewhite = FALSE,
      check_arguments = FALSE
    )

    cf_output = intrinsicFRP::ChenFang2019BetaRankTest(
      returns,
      factors,
      n_bootstrap = n_bootstrap_cf2019_rank_test,
      target_level_kp2006_rank_test = target_level_kp2006_rank_test,
      check_arguments = FALSE
    )

    c(
      as.numeric(hj_output[["lower_bound"]] <= 0.),
      as.numeric(cf_output[["p-value"]])
    )
  }

  EvaluateFactorModel = function(included_factor_indices) {

    output = rep(NA_real_, 69)
    factors_included = factors[, included_factor_indices, drop = FALSE]
    n_included = length(included_factor_indices)

    output[1] = n_included
    output[8:(8 + n_included - 1)] = included_factor_indices - 1

    output[4:5] = EvaluateFactorModelSpecificationAndIdentification(
      returns,
      factors_included,
      n_bootstrap_cf2019_rank_test,
      target_level_kp2006_rank_test,
      hj_test_level
    )

    frp_output = intrinsicFRP::FRP(
      returns,
      factors_included,
      misspecification_robust = TRUE,
      include_standard_errors = TRUE,
      hac_prewhite = FALSE,
      check_arguments = FALSE
    )

    frp = as.numeric(frp_output[["risk_premia"]])
    se_frp = as.numeric(frp_output[["standard_errors"]])

    output[28:(28 + n_included - 1)] = frp
    output[48:(48 + n_included - 1)] = se_frp

    mean_returns = colMeans(returns)
    variance_returns = stats::cov(returns)
    covariance_factors_returns = stats::cov(factors_included, returns)

    var_ret_inv_mean_ret = SafeSolve(variance_returns, mean_returns)
    mean_mim_fac = covariance_factors_returns %*% var_ret_inv_mean_ret
    var_mim_fac = covariance_factors_returns %*%
      SafeSolve(variance_returns, t(covariance_factors_returns))

    output[68] = as.numeric(crossprod(
      mean_mim_fac,
      SafeSolve(var_mim_fac, mean_mim_fac)
    ))

    otfrp = intrinsicFRP::OracleTFRP(
      returns,
      factors_included,
      penalty_parameters = penalty_parameters,
      weighting_type = weighting_type,
      tuning_type = "g",
      one_stddev_rule = one_stddev_rule,
      gcv_scaling_n_assets = gcv_scaling_n_assets,
      gcv_identification_check = gcv_identification_check,
      target_level_kp2006_rank_test = target_level_kp2006_rank_test,
      relaxed = TRUE,
      include_standard_errors = TRUE,
      hac_prewhite = FALSE,
      plot_score = FALSE,
      check_arguments = FALSE
    )

    otfrp_rp = as.numeric(otfrp[["risk_premia"]])
    output[3] = as.numeric(otfrp[["penalty_parameter"]])

    selected_factor_positions = which(abs(otfrp_rp) > sqrt(.Machine$double.eps))
    selected_factor_indices = included_factor_indices[selected_factor_positions]
    n_selected = length(selected_factor_positions)
    output[2] = n_selected

    if (n_selected == n_included) {

      output[6:7] = output[4:5]
      output[18:(18 + n_included - 1)] = included_factor_indices - 1
      output[38:(38 + n_included - 1)] = frp
      output[58:(58 + n_included - 1)] = se_frp
      output[69] = output[68]

    } else if (n_selected > 0) {

      factors_selected = factors_included[, selected_factor_positions, drop = FALSE]
      output[6:7] = EvaluateFactorModelSpecificationAndIdentification(
        returns,
        factors_selected,
        n_bootstrap_cf2019_rank_test,
        target_level_kp2006_rank_test,
        hj_test_level
      )

      output[18:(18 + n_selected - 1)] = selected_factor_indices - 1

      frp_selected_output = intrinsicFRP::FRP(
        returns,
        factors_selected,
        misspecification_robust = TRUE,
        include_standard_errors = TRUE,
        hac_prewhite = FALSE,
        check_arguments = FALSE
      )

      frp_selected = as.numeric(frp_selected_output[["risk_premia"]])
      se_frp_selected = as.numeric(frp_selected_output[["standard_errors"]])

      output[38:(38 + n_selected - 1)] = frp_selected
      output[58:(58 + n_selected - 1)] = se_frp_selected

      cov_fac_sel_ret = stats::cov(factors_selected, returns)
      mean_mim_fac_sel = cov_fac_sel_ret %*% var_ret_inv_mean_ret
      var_mim_fac_sel = cov_fac_sel_ret %*%
        SafeSolve(variance_returns, t(cov_fac_sel_ret))

      output[69] = as.numeric(crossprod(
        mean_mim_fac_sel,
        SafeSolve(var_mim_fac_sel, mean_mim_fac_sel)
      ))

    }

    output

  }

  output = matrix(NA_real_, nrow = n_evaluated_models, ncol = 69)

  indices_kept_factors = if (n_kept_factors > 0) seq_len(n_kept_factors) else integer(0)

  if (n_included_factors == 1) {

    for (model in seq_len(n_evaluated_models)) {
      index_additional_factor = model + n_kept_factors
      included_factor_indices = c(indices_kept_factors, index_additional_factor)
      output[model, ] = EvaluateFactorModel(included_factor_indices)
    }

    return(output)

  }

  for (model in seq_len(n_evaluated_models)) {
    index_additional_factors = sample.int(
      ncol(factors) - n_kept_factors,
      size = n_included_factors,
      replace = FALSE
    ) + n_kept_factors

    included_factor_indices = c(indices_kept_factors, index_additional_factors)
    output[model, ] = EvaluateFactorModel(included_factor_indices)
  }

  output
}
