library(usethis)

ff3 <- read.csv("data-raw/F-F_Research_Data_Factors.CSV")
common_start <- 197310
common_end <- 201612

read_excess_returns <- function(path, start = NULL, end = NULL) {
  returns <- read.csv(path)
  rf <- ff3$RF[match(returns$Date, ff3$Date)]
  returns[, -1] <- (returns[, -1] - rf) / 100

  if (!is.null(start)) {
    returns <- returns[returns$Date >= start, , drop = FALSE]
  }
  if (!is.null(end)) {
    returns <- returns[returns$Date <= end, , drop = FALSE]
  }

  returns
}

as_numeric_matrix <- function(x) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  x
}

returns_ind49 <- read_excess_returns(
  "data-raw/49_Industry_Portfolios.CSV",
  start = common_start,
  end = common_end
)
returns_ind17 <- read_excess_returns(
  "data-raw/17_Industry_Portfolios.CSV",
  start = common_start,
  end = common_end
)

returns_mebeme25 <- read_excess_returns(
  "data-raw/25_Portfolios_5x5.CSV",
  start = common_start,
  end = common_end
)

returns_opinv25 <- read_excess_returns(
  "data-raw/25_Portfolios_OP_INV_5x5.CSV",
  start = common_start,
  end = common_end
)

returns_bemeinv25 <- read_excess_returns(
  "data-raw/25_Portfolios_BEME_INV_5x5.CSV",
  start = common_start,
  end = common_end
)
returns_bemeop25 <- read_excess_returns(
  "data-raw/25_Portfolios_BEME_OP_5x5.CSV",
  start = common_start,
  end = common_end
)
returns_meac25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_AC_5x5.csv",
  start = common_start,
  end = common_end
)
returns_mebeta25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_BETA_5x5.csv",
  start = common_start,
  end = common_end
)
returns_meinv25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_INV_5x5.CSV",
  start = common_start,
  end = common_end
)
returns_meni35 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_NI_5x5.csv",
  start = common_start,
  end = common_end
)
returns_meop25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_OP_5x5.CSV",
  start = common_start,
  end = common_end
)
returns_mepriorone25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_Prior_1_0.CSV",
  start = common_start,
  end = common_end
)
returns_mepriorsixty25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_Prior_60_13.CSV",
  start = common_start,
  end = common_end
)
returns_mepriortwelve25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_Prior_12_2.CSV",
  start = common_start,
  end = common_end
)
returns_mevar25 <- read_excess_returns(
  "data-raw/25_Portfolios_ME_VAR_5x5.csv",
  start = common_start,
  end = common_end
)

returns_me <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_ME.CSV",
  start = common_start,
  end = common_end
)
returns_beme <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_BE-ME.CSV",
  start = common_start,
  end = common_end
)
returns_beta <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_BETA.csv",
  start = common_start,
  end = common_end
)
returns_ni <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_NI.csv",
  start = common_start,
  end = common_end
)
returns_ep <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_E-P.CSV",
  start = common_start,
  end = common_end
)
returns_dp <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_D-P.CSV",
  start = common_start,
  end = common_end
)
returns_cfp <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_CF-P.CSV",
  start = common_start,
  end = common_end
)
returns_op <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_OP.CSV",
  start = common_start,
  end = common_end
)
returns_var <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_VAR.csv",
  start = common_start,
  end = common_end
)
returns_resvar <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_RESVAR.csv",
  start = common_start,
  end = common_end
)
returns_ac <- read_excess_returns(
  "data-raw/Portfolios_Formed_on_AC.csv",
  start = common_start,
  end = common_end
)

quantiles <- c("Lo.20", "Hi.20")

returns_ss <- cbind(
  returns_me[, 1, drop = FALSE],
  returns_me[, quantiles],
  returns_beme[, quantiles],
  returns_beta[, quantiles],
  returns_ni[, quantiles],
  returns_ep[, quantiles],
  returns_dp[, quantiles],
  returns_cfp[, quantiles],
  returns_op[, quantiles],
  returns_var[, quantiles],
  returns_resvar[, quantiles],
  returns_ac[, quantiles]
)
colnames(returns_ss)[1] <- "Date"

returns_ind49 <- as_numeric_matrix(returns_ind49)
returns_ind17 <- as_numeric_matrix(returns_ind17)
returns_mebeme25 <- as_numeric_matrix(returns_mebeme25)
returns_opinv25 <- as_numeric_matrix(returns_opinv25)
returns_bemeinv25 <- as_numeric_matrix(returns_bemeinv25)
returns_bemeop25 <- as_numeric_matrix(returns_bemeop25)
returns_meac25 <- as_numeric_matrix(returns_meac25)
returns_mebeta25 <- as_numeric_matrix(returns_mebeta25)
returns_meinv25 <- as_numeric_matrix(returns_meinv25)
returns_meni35 <- as_numeric_matrix(returns_meni35)
returns_meop25 <- as_numeric_matrix(returns_meop25)
returns_mepriorone25 <- as_numeric_matrix(returns_mepriorone25)
returns_mepriorsixty25 <- as_numeric_matrix(returns_mepriorsixty25)
returns_mepriortwelve25 <- as_numeric_matrix(returns_mepriortwelve25)
returns_mevar25 <- as_numeric_matrix(returns_mevar25)
returns_ss <- as_numeric_matrix(returns_ss)

use_data(
  returns_ind49,
  returns_ind17,
  returns_mebeme25,
  returns_opinv25,
  returns_bemeinv25,
  returns_bemeop25,
  returns_meac25,
  returns_mebeta25,
  returns_meinv25,
  returns_meni35,
  returns_meop25,
  returns_mepriorone25,
  returns_mepriorsixty25,
  returns_mepriortwelve25,
  returns_mevar25,
  returns_ss,
  overwrite = TRUE
)
