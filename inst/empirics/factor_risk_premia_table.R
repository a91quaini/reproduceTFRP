### table: risk premia for the two-factor {MKT, ICR} model

fs::dir_create("inst/results")

ComputeRiskPremiaTableBlock = function(returns, factors) {

  returns = as.matrix(returns)
  storage.mode(returns) = "double"
  factors = as.matrix(factors)
  storage.mode(factors) = "double"

  tfrp = intrinsicFRP::TFRP(
    returns,
    factors,
    include_standard_errors = TRUE,
    check_arguments = FALSE
  )
  frp = intrinsicFRP::FRP(
    returns,
    factors,
    include_standard_errors = TRUE,
    check_arguments = FALSE
  )
  cf2019test = intrinsicFRP::ChenFang2019BetaRankTest(
    returns,
    factors,
    check_arguments = FALSE
  )

  beta = t(solve(stats::cov(factors), stats::cov(factors, returns)))
  xR2 = stats::cov(beta %*% frp$risk_premia) / stats::var(colMeans(returns))

  z_90 = stats::qnorm(0.95)

  list(
    tfrp = tfrp,
    frp = frp,
    tfrp_lower = drop(tfrp$risk_premia - z_90 * tfrp$standard_errors),
    tfrp_upper = drop(tfrp$risk_premia + z_90 * tfrp$standard_errors),
    frp_lower = drop(frp$risk_premia - z_90 * frp$standard_errors),
    frp_upper = drop(frp$risk_premia + z_90 * frp$standard_errors),
    xR2 = as.numeric(xR2),
    p_value = unname(cf2019test[["p-value"]])
  )

}

FormatEstimate = function(x) {
  sprintf("%.3f", x)
}

FormatCiValue = function(x) {
  out = sprintf("%.3f", x)
  if (x < 0) {
    out = paste0("$", out, "$")
  }
  out
}

FormatCi = function(lower, upper) {
  paste0("(", FormatCiValue(lower), ", ", FormatCiValue(upper), ")")
}

FormatPValue = function(x) {
  if (is.na(x)) {
    return("")
  }
  if (x < 0.001) {
    return("$<0.001$")
  }
  sprintf("%.3f", x)
}

BuildRiskPremiaTable = function(panel_a, panel_b) {

  c(
    "\\begin{tabular}{lccccccc}",
    "\\\\[-1.8ex]",
    "\\hline \\hline \\\\ [-3ex]",
    "& \\multicolumn{3}{c}{\\textbf{Panel A: 25 Size-Value + 17 Industry}} & & \\multicolumn{3}{c}{\\textbf{Panel B: 22 Single Sorts + 17 Industry}} \\\\",
    "& {MRRP} && TFRP & & {MRRP} && TFRP \\\\",
    "\\hline",
    sprintf(
      "\\textbf{Mkt} & %s && %s & & %s && %s \\\\",
      FormatEstimate(panel_a$frp$risk_premia[1, 1]),
      FormatEstimate(panel_a$tfrp$risk_premia[1, 1]),
      FormatEstimate(panel_b$frp$risk_premia[1, 1]),
      FormatEstimate(panel_b$tfrp$risk_premia[1, 1])
    ),
    sprintf(
      "& %s && %s & & %s && %s \\\\",
      FormatCi(panel_a$frp_lower[1], panel_a$frp_upper[1]),
      FormatCi(panel_a$tfrp_lower[1], panel_a$tfrp_upper[1]),
      FormatCi(panel_b$frp_lower[1], panel_b$frp_upper[1]),
      FormatCi(panel_b$tfrp_lower[1], panel_b$tfrp_upper[1])
    ),
    sprintf(
      "\\textbf{ICR} & %s && %s & & %s && %s \\\\",
      FormatEstimate(panel_a$frp$risk_premia[2, 1]),
      FormatEstimate(panel_a$tfrp$risk_premia[2, 1]),
      FormatEstimate(panel_b$frp$risk_premia[2, 1]),
      FormatEstimate(panel_b$tfrp$risk_premia[2, 1])
    ),
    sprintf(
      "& %s && %s & & %s && %s \\\\",
      FormatCi(panel_a$frp_lower[2], panel_a$frp_upper[2]),
      FormatCi(panel_a$tfrp_lower[2], panel_a$tfrp_upper[2]),
      FormatCi(panel_b$frp_lower[2], panel_b$frp_upper[2]),
      FormatCi(panel_b$tfrp_lower[2], panel_b$tfrp_upper[2])
    ),
    "\\cline{1-8}",
    sprintf(
      "\\textbf{CS-$R^2$} & \\multicolumn{3}{c}{%s} && \\multicolumn{3}{c}{%s} \\\\",
      FormatEstimate(panel_a$xR2),
      FormatEstimate(panel_b$xR2)
    ),
    "\\cline{1-8}",
    sprintf(
      "\\textbf{Reduced-rank, p-value} & \\multicolumn{3}{c}{%s} && \\multicolumn{3}{c}{%s} \\\\",
      FormatPValue(panel_a$p_value),
      FormatPValue(panel_b$p_value)
    ),
    "\\hline \\hline",
    "\\end{tabular}"
  )

}


### load factors

factors = reproduceTFRP::factors51[,-1]
factors = factors[, c(1, 25)]


### panel A: 25 size-value + 17 industry
returns_panel_a = cbind(
  reproduceTFRP::returns_mebeme25[, -1],
  reproduceTFRP::returns_ind17[, -1]
)


### panel B: 22 single sorts + 17 industry

returns_panel_b = cbind(
  reproduceTFRP::returns_ss[,-1],
  reproduceTFRP::returns_ind17[, -1]
)


### estimation and table export

panel_a = ComputeRiskPremiaTableBlock(returns_panel_a, factors)
panel_b = ComputeRiskPremiaTableBlock(returns_panel_b, factors)

table_lines = BuildRiskPremiaTable(panel_a, panel_b)

output_file = "inst/results/factor_risk_premia_table.tex"
writeLines(table_lines, con = output_file)

cat(paste(table_lines, collapse = "\n"))
cat("\n")
