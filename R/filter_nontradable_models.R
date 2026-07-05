# Filter models containing at least one nontradable factor.
# `results` is the output matrix of `EvaluateVariousFactorModelsR`.
# Nontradable factor indices are defined in `R/globals.R`.
FilterNontradableModels = function(results) {

  columns_to_check = paste0("included_", 1:10)

  results[
    apply(results[, columns_to_check], 1, function(row) {
      any(row %in% idx_nontraded_factors)
    }),
    ,
    drop = FALSE
  ]

}
