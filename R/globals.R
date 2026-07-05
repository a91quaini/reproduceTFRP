# 1-based indices into the factor matrix AFTER the Date column is removed
# and the simulated "Useless" factor is appended (52 columns):
# 11 = LIQ_NT, 25 = ICR, 29:38 = FIN_UNC, REAL_UNC, MACRO_UNC, TERM, DEFAULT,
# DIV, UNRATE, PE, BW_ISENT, HJTZ_ISENT, 47:52 = NONDUR, SERV, IPGrowth, Oil,
# DeltaSLOPE, Useless
idx_nontraded_factors = c(
  11, 25,
  29:38, 47:52
)

utils::globalVariables(c(
  "idx",
  "colour",
  "EvaluateVariousFactorModels",
  "Factor",
  "FRP",
  "method",
  "Model",
  "name",
  "risk_premia",
  "rp",
  "Screening",
  "se",
  "standard_errors",
  "Value"
))
