#' Evaluate risk premia across alternative test-asset sets
#'
#' `EvaluateRiskPremiaByVaryingTestReturns` repeatedly samples subsets of the
#' available test assets and computes FM, MRRP, and TFRP estimates.
#'
#' @param returns Matrix or data frame of test asset returns.
#' @param n_returns Number of assets sampled in each draw.
#' @param n_samples Number of repeated draws.
#' @param factors Matrix or data frame of candidate factors.
#' @param rng_seed Random seed.
#' @param save_results Logical indicating whether the result object is saved.
#' @param save_dir Directory used when `save_results = TRUE`.
#'
#' @return A list containing average estimates, average standard errors, full
#'   simulation details, and configuration metadata.
#'
#' @export
EvaluateRiskPremiaByVaryingTestReturns <- function(
    returns,
    n_returns,
    n_samples,
    factors,
    rng_seed = 1,
    save_results = TRUE,
    save_dir = "inst/results"
) {
  # --- strip Date if present ---------------------------------------------------
  drop_date_col <- function(X) {
    if (!is.null(colnames(X)) && "Date" %in% colnames(X)) {
      X <- X[, setdiff(colnames(X), "Date"), drop = FALSE]
    }
    X
  }
  returns <- drop_date_col(returns)
  factors <- drop_date_col(factors)

  # coerce to numeric matrices (defensive)
  returns <- as.matrix(returns); storage.mode(returns) <- "double"
  factors <- as.matrix(factors); storage.mode(factors) <- "double"

  # --- checks ------------------------------------------------------------------
  stopifnot(is.numeric(returns), is.numeric(factors))
  stopifnot(nrow(returns) == nrow(factors))
  stopifnot(n_returns >= 1, n_returns <= ncol(returns))

  set.seed(rng_seed)

  N <- ncol(returns)
  K <- ncol(factors)
  if (is.null(colnames(factors))) colnames(factors) <- paste0("F", seq_len(K))
  est_names <- c("FM", "KRS", "TFRP")

  rp_arr <- array(NA_real_, dim = c(n_samples, K, length(est_names)),
                  dimnames = list(NULL, colnames(factors), est_names))
  se_arr <- array(NA_real_, dim = c(n_samples, K, length(est_names)),
                  dimnames = list(NULL, colnames(factors), est_names))
  sampled_asset_indices <- vector("list", n_samples)

  for (s in seq_len(n_samples)) {
    idx <- sample.int(N, size = n_returns, replace = FALSE)
    sampled_asset_indices[[s]] <- idx
    R_s <- returns[, idx, drop = FALSE]
    F_s <- factors

    # FM (classic two-pass)
    fm <- intrinsicFRP::FRP(
      R_s, F_s,
      include_standard_errors = TRUE,
      misspecification_robust = FALSE,
      check_arguments = FALSE
    )
    rp_arr[s, , "FM"] <- fm$risk_premia
    se_arr[s, , "FM"] <- fm$standard_errors

    # KRS (misspecification-robust)
    krs <- intrinsicFRP::FRP(
      R_s, F_s,
      include_standard_errors = TRUE,
      check_arguments = FALSE
    )
    rp_arr[s, , "KRS"] <- krs$risk_premia
    se_arr[s, , "KRS"] <- krs$standard_errors

    # TFRP
    tfrp <- intrinsicFRP::TFRP(
      R_s, F_s,
      include_standard_errors = TRUE,
      check_arguments = FALSE
    )
    rp_arr[s, , "TFRP"] <- tfrp$risk_premia
    se_arr[s, , "TFRP"] <- tfrp$standard_errors

  }

  # summaries (mean absolute risk premia and mean standard error by sample)
  estimates_mat <- cbind(
    FM    = rowMeans(abs(rp_arr[, , "FM",    drop = FALSE]), dims = 1, na.rm = TRUE),
    KRS   = rowMeans(abs(rp_arr[, , "KRS",   drop = FALSE]), dims = 1, na.rm = TRUE),
    TFRP  = rowMeans(abs(rp_arr[, , "TFRP",  drop = FALSE]), dims = 1, na.rm = TRUE)
  )
  ses_mat <- cbind(
    FM    = rowMeans(se_arr[, , "FM",    drop = FALSE], dims = 1, na.rm = TRUE),
    KRS   = rowMeans(se_arr[, , "KRS",   drop = FALSE], dims = 1, na.rm = TRUE),
    TFRP  = rowMeans(se_arr[, , "TFRP",  drop = FALSE], dims = 1, na.rm = TRUE)
  )

  colnames(estimates_mat) <- est_names
  colnames(ses_mat)       <- est_names
  rownames(estimates_mat) <- paste0("sample_", seq_len(n_samples))
  rownames(ses_mat)       <- paste0("sample_", seq_len(n_samples))

  out <- list(
    estimates = estimates_mat,
    standard_errors = ses_mat,
    details = list(
      risk_premia = rp_arr,
      standard_errors = se_arr,
      sampled_asset_indices = sampled_asset_indices
    ),
    config = list(
      n_returns = n_returns,
      n_samples = n_samples,
      rng_seed = rng_seed
    )
  )

  if (isTRUE(save_results)) {
    if (requireNamespace("fs", quietly = TRUE)) {
      fs::dir_create(save_dir)
    } else {
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    }
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    save_path <- file.path(save_dir, paste0("robustness_test_assets_", timestamp, ".rds"))
    saveRDS(out, save_path)
    message("Results saved to: ", save_path)
    out$file_path <- save_path
  }

  out
}

PlotRiskPremiaRobustness <- function(
    methodology,
    factor_name,
    risk_premia_vec,
    se_vec,
    save_dir = "inst/figures",
    display_plot = TRUE
) {
  # --- dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' must be installed to use this function.")
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' must be installed to combine plots.")
  if (!requireNamespace("fs", quietly = TRUE))
    stop("Package 'fs' must be installed to ensure directory creation.")

  # --- check inputs
  stopifnot(is.character(methodology), length(methodology) == 1,
            is.character(factor_name), length(factor_name) == 1,
            is.numeric(risk_premia_vec), is.numeric(se_vec),
            length(risk_premia_vec) == length(se_vec))

  # --- ensure output directory exists
  fs::dir_create(save_dir)

  # --- filenames
  file_tag <- paste0("robustness_test_assets_", factor_name, "_", methodology)
  file_path <- file.path(save_dir, paste0(file_tag, ".png"))

  # --- build data.frame
  df <- data.frame(
    risk_premia = risk_premia_vec,
    se = se_vec
  )

  # --- display labels for legend ----------------------------------------------
  method_labels <- c(
    "FM"    = "FM",
    "KRS"   = "MRRP",
    "TFRP"  = "TFRP"
  )

  # --- base theme
  theme_base <- ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = ggplot2::element_blank()
    )

  # --- histogram of risk premia
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = risk_premia)) +
    ggplot2::geom_histogram(
      bins = 30, color = "black", fill = "steelblue", alpha = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = mean(risk_premia_vec, na.rm = TRUE),
      color = "red", linewidth = 0.8, linetype = "dashed"
    ) +
    ggplot2::labs(
      title = paste0("Risk Premia Distribution - ", factor_name, " (", methodology, ")"),
      x = expression(hat(lambda)),
      y = "Frequency"
    ) +
    theme_base

  # --- histogram of standard errors
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = se)) +
    ggplot2::geom_histogram(
      bins = 30, color = "black", fill = "darkorange", alpha = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = mean(se_vec, na.rm = TRUE),
      color = "red", linewidth = 0.8, linetype = "dashed"
    ) +
    ggplot2::labs(
      title = paste0("Standard Error Distribution - ", factor_name, " (", methodology, ")"),
      x = expression(SE(hat(lambda))),
      y = "Frequency"
    ) +
    theme_base

  # --- combine and save
  combined <- p1 + p2 + patchwork::plot_layout(ncol = 1)
  if (isTRUE(display_plot)) {
    print(combined)
  }
  ggplot2::ggsave(file_path, combined, width = 8, height = 10, dpi = 300)

  message("Saved: ", file_path)
  invisible(file_path)
}

PlotRobustnessOverlaidDensities <- function(
    res,
    methods = c("FM","KRS","TFRP"),
    save_dir = "inst/figures",
    width = 9,
    height = 6,
    dpi = 300,
    display_plots = TRUE,
    tradable = FALSE,
    factor_series = NULL,
    model_name = NULL
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  if (requireNamespace("fs", quietly = TRUE)) {
    fs::dir_create(save_dir)
  } else if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }

  # --- helper: drop Date column if present ------------------------------------
  drop_date_col <- function(X) {
    if (!is.null(X) && !is.null(colnames(X)) && "Date" %in% colnames(X)) {
      X <- X[, setdiff(colnames(X), "Date"), drop = FALSE]
    }
    X
  }

  # --- helper: determine whether factor f is tradable -------------------------
  is_factor_tradable <- function(f, tradable, all_factors) {
    if (is.logical(tradable) && length(tradable) == 1L) {
      return(isTRUE(tradable))
    }

    if (is.logical(tradable) && length(tradable) > 1L) {
      if (is.null(names(tradable))) {
        if (length(tradable) != length(all_factors)) {
          stop("If `tradable` is an unnamed logical vector, it must have length equal to the number of factors.")
        }
        names(tradable) <- all_factors
      }
      return(isTRUE(tradable[[f]]))
    }

    if (is.character(tradable)) {
      return(f %in% tradable)
    }

    stop("`tradable` must be either a logical scalar, a logical vector, or a character vector of factor names.")
  }

  # --- helper: compute factor mean --------------------------------------------
  compute_factor_mean <- function(f, factor_series) {
    if (is.null(factor_series)) {
      stop("`factor_series` must be provided when at least one factor is tradable.")
    }

    factor_series <- drop_date_col(factor_series)

    if (is.vector(factor_series) && !is.list(factor_series)) {
      f_vec <- as.numeric(factor_series)
    } else {
      factor_series <- as.matrix(factor_series)
      storage.mode(factor_series) <- "double"

      if (is.null(colnames(factor_series))) {
        stop("`factor_series` must have column names.")
      }
      if (!(f %in% colnames(factor_series))) {
        stop("Factor `", f, "` not found in `factor_series`.")
      }

      f_vec <- factor_series[, f]
    }

    f_vec <- as.numeric(f_vec)
    f_vec <- f_vec[is.finite(f_vec)]

    if (length(f_vec) == 0L) {
      stop("Factor `", f, "` has no finite observations in `factor_series`.")
    }

    mean(f_vec)
  }

  # --- display labels for legend ----------------------------------------------
  method_labels <- c(
    "FM"    = "FM",
    "KRS"   = "MRRP",
    "TFRP"  = "TFRP"
  )

  # --- file name tag for number of returns ------------------------------------
  N <- if (!is.null(res$config$n_returns)) res$config$n_returns else NA_integer_
  n_tag <- if (!is.na(N)) paste0("N", N, "_") else ""

  # --- methods and factors -----------------------------------------------------
  dim_methods <- dimnames(res$details$risk_premia)[[3]]
  methods <- intersect(methods, dim_methods)
  if (length(methods) == 0L) {
    stop(
      "No requested methods found in result object. Available: ",
      paste(dim_methods, collapse = ", ")
    )
  }

  factors <- dimnames(res$details$risk_premia)[[2]]
  if (is.null(factors) || length(factors) == 0L) {
    stop("Could not recover factor names from `res$details$risk_premia`.")
  }

  # --- model tag to append to filenames ---------------------------------------
  # Default: derive from the factors present in the result object
  if (is.null(model_name)) {
    model_tag <- paste0("(", paste(factors, collapse = ","), ")")
  } else {
    if (!is.character(model_name) || length(model_name) != 1L) {
      stop("`model_name` must be NULL or a single character string.")
    }
    model_tag <- model_name
  }

  # --- helper: wide -> long ----------------------------------------------------
  make_df <- function(arr3, factor_name, value_name) {
    m <- arr3[, factor_name, methods, drop = FALSE]
    df <- as.data.frame(m)
    names(df) <- methods
    df$sample <- seq_len(nrow(df))

    long <- reshape(
      df,
      varying = methods,
      v.names = value_name,
      timevar = "method",
      times = methods,
      direction = "long"
    )

    long <- long[, c("sample", "method", value_name)]
    long$method <- factor(long$method, levels = methods)
    long
  }

  out_files <- list()

  for (f in factors) {

    this_is_tradable <- is_factor_tradable(f, tradable, factors)

    factor_mean <- NULL
    if (this_is_tradable) {
      factor_mean <- compute_factor_mean(f, factor_series)
    }

    # --- risk premia densities -------------------------------------------------
    df_rp <- make_df(res$details$risk_premia, f, "rp")

    p_rp <- ggplot2::ggplot(df_rp, ggplot2::aes(x = rp, color = method)) +
      ggplot2::geom_density(linewidth = 1, adjust = 1.0, fill = NA) +
      ggplot2::scale_color_discrete(
        breaks = methods,
        labels = unname(method_labels[methods])
      ) +
      ggplot2::labs(
        title = paste0("Risk premia distributions - ", f),
        x = expression(hat(lambda)),
        y = "Density",
        color = "Method"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(legend.position = "bottom")

    if (this_is_tradable) {
      p_rp <- p_rp +
        ggplot2::geom_vline(
          xintercept = factor_mean,
          color = "black",
          linewidth = 0.9,
          linetype = "dashed"
        )
    }

    if (isTRUE(display_plots)) {
      print(p_rp)
    }

    f_rp <- file.path(
      save_dir,
      paste0("robustness_", n_tag, "density_riskpremia_", f, "_", model_tag, ".png")
    )
    ggplot2::ggsave(f_rp, p_rp, width = width, height = height, dpi = dpi)
    message("Saved: ", f_rp)
    out_files[[paste0(f, "_rp")]] <- f_rp

    # --- standard error densities ---------------------------------------------
    df_se <- make_df(res$details$standard_errors, f, "se")

    p_se <- ggplot2::ggplot(df_se, ggplot2::aes(x = se, color = method)) +
      ggplot2::geom_density(linewidth = 1, adjust = 1.0, fill = NA) +
      ggplot2::scale_color_discrete(
        breaks = methods,
        labels = unname(method_labels[methods])
      ) +
      ggplot2::labs(
        title = paste0("Standard error distributions - ", f),
        x = expression(SE(hat(lambda))),
        y = "Density",
        color = "Method"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(legend.position = "bottom")

    if (isTRUE(display_plots)) {
      print(p_se)
    }

    f_se <- file.path(
      save_dir,
      paste0("robustness_", n_tag, "density_se_", f, "_", model_tag, ".png")
    )
    ggplot2::ggsave(f_se, p_se, width = width, height = height, dpi = dpi)
    message("Saved: ", f_se)
    out_files[[paste0(f, "_se")]] <- f_se
  }

  invisible(out_files)
}
