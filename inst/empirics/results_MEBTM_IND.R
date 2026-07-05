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

## load 25 MEBTM and 17 IND returns
returns = as.matrix(cbind(
  reproduceTFRP::returns_mebeme25[, -1],
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

### main text: 25 MEBTM + 17 IND, market always included
### and model contains at least one nontradable factor

n_evaluated_models = 10000
evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt = EvaluateVariousFactorModelsR(
  returns,
  factors,
  penalty_parameters,
  n_evaluated_models,
  n_kept_factors = 1,
  one_stddev_rule = TRUE,
  gcv_identification_check = FALSE
)
save(
  evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt,
  file = "inst/results/evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt.rda"
)

load(file = "inst/results/evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt.rda")

evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable =
  FilterNontradableModels(evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt)

PlotModelIdentification(
  evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 3,
  ],
  name = "MEBTM25IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

PlotModelIdentificationNoScreen(
  evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 3,
  ],
  name = "MEBTM25IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

PlotSelection(
  evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 5,
  ],
  factors,
  name = "MEBTM25IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

PlotSelectionDistribution(
  evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[
    evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable[,"n_factors"] >= 3,
  ],
  name = "MEBTM25IND17_AIC_GCV_keptmkt_high",
  save_plot = TRUE
)

ifrp = intrinsicFRP::TFRP(
  returns,
  factors,
  include_standard_errors = TRUE,
  check_arguments = FALSE
)

PlotTradableRiskPremia(
  evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable,
  factors,
  ifrp,
  name = "MEBTM25IND17_AIC_GCV_keptmkt",
  save_plot = TRUE
)


### factor-specific TFRP figures in the paper (Figure 6):
### Panel A: ICR (factor 25), Panel B: BEH_PEAD (factor 39), Panel C: TERM (factor 32)

for (factr in c(25, 32, 39)) {

  PlotRankedRiskPremia(
    evaluate_1_to_10_models_MEBTM25IND17_AIC_GCV_keptmkt_nontradable,
    factors,
    ifrp,
    factor_idx = factr,
    ci_level = .90,
    name = "5perc_MEBTM25IND17_AIC_GCV_keptmkt",
    save_plot = TRUE,
    initial_n_factors = 6
  )

}
