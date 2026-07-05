### create output folders

fs::dir_create("inst/results")
fs::dir_create("inst/figures")


### load data

set.seed(1)

## load 51 factors and add a useless factor
factors = reproduceTFRP::factors51[,-1]
factors = cbind(
  factors,
  matrix(rnorm(nrow(factors)), nrow(factors), 1)
)
colnames(factors) = c(colnames(reproduceTFRP::factors51[,-1]), "Useless")

## load 22 single-sorted and 17 industry returns
returns = as.matrix(cbind(
  reproduceTFRP::returns_ss[,-1],
  reproduceTFRP::returns_ind17[, -1]
))


### settings

n_penalty_parameters = 300
lower_penalty_parameter = 1. / nrow(returns)^2
upper_penalty_parameter = 1.5
penalty_parameters = c(exp(seq(
  from = log(lower_penalty_parameter),
  to = log(upper_penalty_parameter),
  length.out = n_penalty_parameters
)))

### main text and online appendix: 22 single-sorted + 17 industry,
### market always included and model contains at least one nontradable factor

n_evaluated_models = 10000
evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt = EvaluateVariousFactorModelsR(
  returns,
  factors,
  penalty_parameters,
  n_evaluated_models,
  n_kept_factors = 1,
  one_stddev_rule = TRUE,
  gcv_identification_check = FALSE
)
save(
  evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt,
  file = "inst/results/evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt.rda"
)

load(file = "inst/results/evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt.rda")

evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable =
  FilterNontradableModels(evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt)

PlotModelIdentification(
  evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 3,
  ],
  name = "SS22IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

PlotSelectionDistribution(
  evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 3,
  ],
  name = "SS22IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

PlotSelection(
  evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 3,
  ],
  factors,
  name = "SS22IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

ifrp = intrinsicFRP::TFRP(
  returns,
  factors,
  include_standard_errors = TRUE,
  check_arguments = FALSE
)

PlotTradableRiskPremia(
  evaluate_1_to_10_models_SS22IND17_AIC_GCV_keptmkt_nontradable,
  factors,
  ifrp,
  name = "SS22IND17_AIC_GCV_keptmkt",
  save_plot = TRUE
)
