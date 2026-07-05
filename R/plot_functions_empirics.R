PlotModelIdentification = function(
  results,
  name="",
  contains_nontradable = FALSE,
  save_plot=FALSE
) {

  if (contains_nontradable) {

    results = FilterNontradableModels(results)

  }

  list_n_models = unique(results[,"n_factors"])
  n_models = list_n_models[length(list_n_models)]
  n_kept_factors = list_n_models[1] - 1

  label_y = "Identification frequency"

  frequencies = matrix(0., n_models - n_kept_factors, 2)
  colnames(frequencies) = c(
    "beta_rank_pvalue_cf2019",
    "beta_rank_pvalue_cf2019_screened"
  )

  for (idx in 1:(n_models - n_kept_factors)) {

    rows_idx = results[,"n_factors"] == idx + n_kept_factors
    frequencies[idx,c("beta_rank_pvalue_cf2019", "beta_rank_pvalue_cf2019_screened")] = colMeans(
      results[rows_idx, c("beta_rank_pvalue_cf2019","beta_rank_pvalue_cf2019_screened"), drop=FALSE] < 0.05, na.rm=TRUE)

  }

  col_idx = c("beta_rank_pvalue_cf2019", "beta_rank_pvalue_cf2019_screened")

  df = data.frame(

    n_factors = factor(rep(list_n_models, 2)),
    value = c(frequencies[,col_idx]),
    Screening = c(
      rep("No", n_models - n_kept_factors),
      rep("Yes", n_models - n_kept_factors)
    )
  )

  plot(
    ggplot2::ggplot(df, ggplot2::aes(
      x=.data$n_factors,
      y=.data$value,
      fill=.data$Screening
    )) +
      ggplot2::theme(
        text=ggplot2::element_text(size=22),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major.y = ggplot2::element_line(
          color="gray", linewidth =0.25
        ),
        #axis.title.x = ggplot2::element_blank(),
        #axis.title.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::geom_bar(
        stat="identity",
        position="dodge",
        width=0.7
      ) +
      # Add text on bars in percentage format
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(round(.data$value * 100, 1), "%"),
          group = .data$Screening
        ),
        position = ggplot2::position_dodge(width = 0.8), # Align text with bars
        vjust = -0.5, # Adjust text position above bars
        size = 5 # Font size for text
      ) +
      ggplot2::xlab("# factors") +
      ggplot2::ylab(label_y) #+
    # ggplot2::scale_fill_manual(
    #   values=c("black", "darkgray")
    # )

  )

  if (save_plot) {

    figure_name = paste(
      "identification_frequency_1_to_10_models_",
      name,
      sep=""
    )

    path_to_fig = "inst/figures"
    fig_height = 14
    fig_width = 24
    fig_device = "pdf"
    fig_dpi = 700
    fig_units = "cm"

    name_image = paste(
      figure_name,
      ".",
      fig_device,
      sep=""
    )

    ggplot2::ggsave(
      filename=name_image,
      device=fig_device,
      path=path_to_fig,
      width=fig_width,
      height=fig_height,
      units=fig_units,
      dpi=fig_dpi
    )


  }

}

PlotModelIdentificationNoScreen = function(
  results,
  name="",
  contains_nontradable = FALSE,
  save_plot=FALSE
) {

  if (contains_nontradable) {

    results = FilterNontradableModels(results)

  }

  list_n_models = unique(results[,"n_factors"])
  n_models = list_n_models[length(list_n_models)]
  n_kept_factors = list_n_models[1] - 1

  frequencies = matrix(0., n_models - n_kept_factors, 2)
  colnames(frequencies) = c(
    "beta_rank_pvalue_cf2019",
    "beta_rank_pvalue_cf2019_bonferroni"
  )

  n_evaluations = 0
  for (idx in 1:(n_models - n_kept_factors)) {
    n_evaluations = n_evaluations + sum(
      results[,"n_factors"] == idx + n_kept_factors
    )
  }

  for (idx in 1:(n_models - n_kept_factors)) {

    rows_idx = results[,"n_factors"] == idx + n_kept_factors

    frequencies[idx,"beta_rank_pvalue_cf2019"] = colMeans(
      results[rows_idx, "beta_rank_pvalue_cf2019", drop=FALSE] < 0.05, na.rm=TRUE
    )
    frequencies[idx,"beta_rank_pvalue_cf2019_bonferroni"] = colMeans(
      results[rows_idx, "beta_rank_pvalue_cf2019", drop=FALSE] < 0.05 / n_evaluations, na.rm=TRUE
    )

  }

  df = data.frame(
    n_factors = factor(list_n_models),
    value = c(frequencies[,1])
  )

  plot(
    ggplot2::ggplot(df, ggplot2::aes(
      x=.data$n_factors,
      y=.data$value#,
      #fill="red"
    )) +
      ggplot2::theme(
        text=ggplot2::element_text(size=22),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major.y = ggplot2::element_line(
          color="gray", linewidth =0.25
        ),
        #axis.title.x = ggplot2::element_blank(),
        #axis.title.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::geom_bar(
        stat="identity",
        fill="brown3",
        # position="dodge",
        width=0.7
      ) + # Adding text on top of bars
    ggplot2::geom_text(
      ggplot2::aes(label=paste0(round(.data$value * 100, 2), "%")), # Labels are the bar values, rounded to 2 decimal places
      vjust=-0.5, # Vertical adjustment to place text above the bars
      size=5 # Size of the text
    ) +
      ggplot2::xlab("# factors") +
      ggplot2::ylab("Identification frequency") #+
    # ggplot2::scale_fill_manual(
    #   values=c("black", "darkgray")
    # )

  )


  if (save_plot) {

    figure_name = paste(
      "identification_frequency_noscreen_1_to_10_models_",
      name,
      sep=""
    )

    path_to_fig = "inst/figures"
    fig_height = 14
    fig_width = 24
    fig_device = "pdf"
    fig_dpi = 700
    fig_units = "cm"

    name_image = paste(
      figure_name,
      ".",
      fig_device,
      sep=""
    )

    ggplot2::ggsave(
      filename=name_image,
      device=fig_device,
      path=path_to_fig,
      width=fig_width,
      height=fig_height,
      units=fig_units,
      dpi=fig_dpi
    )


  }

}

PlotSelection = function(
  results,
  factors,
  name="",
  contains_nontradable = FALSE,
  save_plot = FALSE
) {


  if (contains_nontradable) {

    results = FilterNontradableModels(results)

  }
  n_factors = ncol(factors)
  list_n_models = unique(results[,"n_factors"])
  n_models = 10

  selection = matrix(0., n_factors, length(list_n_models))

  for (idx in list_n_models) {

    row_idx = results[,"n_factors"] == idx

    included = table(results[row_idx,8:(8+idx-1)], useNA = "no")
    selected = table(results[row_idx,(8+n_models):(8+n_models+idx-1)], useNA = "no")

    denominator = as.numeric(included[names(included) %in% names(selected)])

    selection[as.numeric(names(selected)), idx - list_n_models[1] + 1] = as.numeric(selected) / denominator

  }

  rownames(selection) = colnames(factors)

  title = "Selected factors"
  heat_colors = c("lightgray", "darkgreen")

  colnames(selection) = paste(unique(results[,"n_factors"]))

  long <- reshape2::melt(selection)
  colnames(long) <- c("factor", "n_factors", "Frequency")
  long$factor = as.factor(long$factor)
  long$n_factors = as.factor(long$n_factors)

  plot(
    ggplot2::ggplot(long, ggplot2::aes(
      x = .data$n_factors, y = .data$factor, fill = .data$Frequency
    )) +
      ggplot2::theme(
        text=ggplot2::element_text(size=22),
        panel.background = ggplot2::element_rect(fill = "white"),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::geom_tile(colour="white", size=0.05) +
      ggplot2::ggtitle(title) +
      ggplot2::labs(x = "# factors") +
      ggplot2::scale_y_discrete(expand=c(0, 0)) +
      ggplot2::scale_x_discrete(expand=c(0, 0)) +
      ggplot2::scale_fill_gradient(low = heat_colors[1], high = heat_colors[2])

  )

  if (save_plot) {

    figure_name = paste(
      "factor_selection_frequency_",
      name,
      sep=""
    )

    path_to_fig = "inst/figures"
    fig_height = 24
    fig_width = 26
    fig_device = "pdf"
    fig_dpi = 700
    fig_units = "cm"

    name_image = paste(
      figure_name,
      ".",
      fig_device,
      sep=""
    )

    ggplot2::ggsave(
      filename=name_image,
      device=fig_device,
      path=path_to_fig,
      width=fig_width,
      height=fig_height,
      units=fig_units,
      dpi=fig_dpi
    )
  }

}

PlotSelectionDistribution = function(
  results,
  name="",
  contains_nontradable = FALSE,
  save_plot = FALSE
) {

  if (contains_nontradable) {

    results = FilterNontradableModels(results)

  }

  list_n_models = unique(results[,"n_factors"])
  n_models = 10
  n_kept_factors = list_n_models[1] - 1

  selection = matrix(0., list_n_models[length(list_n_models)], length(list_n_models))
  for (col in 1:length(list_n_models)) {

    row_idx = results[,"n_factors"] == col + n_kept_factors
    selected = results[row_idx, "n_selected_factors"]#rowSums(!is.na(results[row_idx, (8+n_models):(8+n_models + col + n_kept_factors - 1), drop = FALSE]))

    for (row in 1:list_n_models[length(list_n_models)]) {

      selection[row, col] = sum(selected == row) / length(selected)

    }

  }

  selection_mat = reshape2::melt(selection)
  colnames(selection_mat) = c("selected", "included", "Frequency")
  selection_mat[,"included"] = selection_mat[,"included"] + n_kept_factors

  selection_mat$selected = as.factor(selection_mat$selected)
  selection_mat$included = as.factor(selection_mat$included)

  plot(

    ggplot2::ggplot(selection_mat, ggplot2::aes(
      x = .data$included, y = .data$selected, fill = .data$Frequency
    )) +
      ggplot2::theme(
        text=ggplot2::element_text(size=22),
        panel.background = ggplot2::element_rect(fill = "white"),
        axis.text.y = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::geom_tile(ggplot2::aes(fill = .data$Frequency), color='white') +
      # ggplot2::geom_tile(colour="white", size=0.05) +
      # ggplot2::ggtitle(title[idx]) +
      # ggplot2::scale_fill_gradient(low = "white", high = "blue") +
      ggplot2::xlab("# included factors") +
      ggplot2::ylab("# selected factors") +
      ggplot2::scale_y_discrete(expand=c(0, 0)) +
      ggplot2::scale_x_discrete(expand=c(0, 0)) +
      ggplot2::scale_fill_gradient(low = "white", high = "black")


  )

  if (save_plot) {

    figure_name = paste(c(
      "factor_selection_distribution_frequency_"
    ), name, sep="")

    path_to_fig = "inst/figures"
    fig_height = 24
    fig_width = 24
    fig_device = "pdf"
    fig_dpi = 700
    fig_units = "cm"

    name_image = paste(
      figure_name,
      ".",
      fig_device,
      sep=""
    )

    ggplot2::ggsave(
      filename=name_image,
      device=fig_device,
      path=path_to_fig,
      width=fig_width,
      height=fig_height,
      units=fig_units,
      dpi=fig_dpi
    )
  }

}

PlotTradableRiskPremia = function(
  results,
  factors,
  tfrp,
  # idx_factors = 1:52,
  contains_nontradable = FALSE,
  name = "",
  save_plot = FALSE,
  initial_n_factors = 10
) {

  if (contains_nontradable) {

    results = FilterNontradableModels(results)

  }

  results = results[results[,"n_factors"] == initial_n_factors,]

  selection_frequency = c()
  for (fac in 1:ncol(factors)) {

    included = apply(results[,8:17] == fac, 1, any)
    selected = apply(results[,18:27] == fac, 1, any)
    n_selected = sum(selected, na.rm = TRUE)
    if (n_selected == 0) {
      selection_frequency = c(selection_frequency, 0)
    } else {
      selection_frequency = c(selection_frequency, n_selected / sum(included))
    }
    colnames(factors)[fac] = paste(
      colnames(factors)[fac],
      " (",
      round(selection_frequency[fac] * 100, 1),
      "%)",
      sep=""
    )

  }

  nonzero_selection = which(selection_frequency > 0)
  factors = factors[,nonzero_selection]
  risk_premia = tfrp$risk_premia[nonzero_selection]
  risk_premia_se = tfrp$standard_errors[nonzero_selection]
  selection_frequency = selection_frequency[nonzero_selection]

  order_idx = order(selection_frequency)
  factors = factors[,order_idx]
  selection_frequency = selection_frequency[order_idx]
  risk_premia = risk_premia[order_idx]
  risk_premia_se = risk_premia_se[order_idx]


  # create dataframe
  for (level in c(0.975, 0.95)) {


    quantile = stats::qnorm(level)
    df = data.frame(
      Factor = colnames(factors),
      risk_premia = risk_premia,
      standard_errors = risk_premia_se,
      Significant = (0. < risk_premia - quantile * risk_premia_se) |
        (0. > risk_premia + quantile * risk_premia_se)
      # selection_frequency = as.numeric(selected) / denominator
    )

    df$Factor = factor(df$Factor, levels = df$Factor)

    plot(
      ggplot2::ggplot(df, ggplot2::aes(
        x = .data$Factor,
        y = .data$risk_premia,
        fill = .data$Significant
      )) +
        ggplot2::theme(
          text=ggplot2::element_text(size=22),
          panel.background = ggplot2::element_rect(fill = "white"),
          panel.grid.major = ggplot2::element_line(color = 'gray'),
          panel.grid.minor = ggplot2::element_line(color = 'gray'),
          axis.title.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 12),
          plot.title = ggplot2::element_text(hjust = 0.5)
        ) +
        ggplot2::geom_bar(
          stat = "identity",
          position = "dodge",
          width=0.7
        ) +
        ggplot2::ylab("Tradable risk Premia") +
        ggplot2::geom_errorbar(ggplot2::aes(
          x=as.factor(Factor),
          ymin=risk_premia - quantile * standard_errors,
          ymax=risk_premia + quantile * standard_errors),
          linewidth=.8,
          position = ggplot2::position_dodge(0.5),
          width = 0.25
        ) #+
      #ggplot2::coord_flip()


    )

    if (save_plot) {

      if (level == 0.975) {
        say = 5
      } else {
        say = 10
      }
      figure_name = paste(
        "tradable_risk_premia_",
        say,
        "perc_",
        name,
        sep=""
      )

      path_to_fig = "inst/figures"
      fig_height = 16
      fig_width = 40
      fig_device = "pdf"
      fig_dpi = 700
      fig_units = "cm"

      name_image = paste(
        figure_name,
        ".",
        fig_device,
        sep=""
      )

      ggplot2::ggsave(
        filename=name_image,
        device=fig_device,
        path=path_to_fig,
        width=fig_width,
        height=fig_height,
        units=fig_units,
        dpi=fig_dpi
      )
    }

  }


}


PlotRankedRiskPremia = function(
    results,
    factors,
    tfrp,
    factor_idx = 1,
    ci_level = .975,
    contains_nontradable = FALSE,
    name = "",
    save_plot = FALSE,
    initial_n_factors = 10,
    screened = FALSE
) {

  if (contains_nontradable) {

    results = FilterNontradableModels(results)

  }

  results = results[results[,"n_factors"] == initial_n_factors,]
  risk_premia = c()
  upper_ci = c()
  lower_ci = c()

  scr_adj = 0
  if (screened) {
    scr_adj = 10
  }

  row_idx_factor <- which(apply(results[, 8:17 + scr_adj] == factor_idx, 1, any))
  results = results[row_idx_factor,,drop=FALSE]

  for (idx in 1:nrow(results)) {

    col_idx = results[idx, 8:17 + scr_adj] == factor_idx

    risk_premia = c(
      risk_premia,
      results[idx, 27 + scr_adj + which(col_idx)]
    )

    se = results[idx, 47 + scr_adj + which(col_idx)]

    upper_ci = c(
      upper_ci,
      risk_premia[idx] + stats::qnorm(ci_level) * se
    )

    lower_ci = c(
      lower_ci,
      risk_premia[idx] - stats::qnorm(ci_level) * se
    )

  }

  significance = sign(lower_ci * upper_ci) > 0 #(0 > lower_ci) & (0 < upper_ci)
  significance_frequency = round(sum(significance) / length(risk_premia), 2)

  risk_premia = risk_premia

  rank_idx    = order(risk_premia)
  risk_premia = risk_premia[rank_idx]
  upper_ci    = upper_ci[rank_idx]
  lower_ci    = lower_ci[rank_idx]
  model       = 1:length(risk_premia)

  # Create the first dataframe for points
  df <- data.frame(
    Value = c(risk_premia, upper_ci, lower_ci),
    colour = c(
      rep("MR", length(risk_premia)),
      rep("MR CI", 2 * length(risk_premia))
    ),
    Model = rep(model, 3)
  )

  # Create the second dataframe for dashed lines
  df_lines <- data.frame(
    values = c(
      tfrp$risk_premia[factor_idx],
      tfrp$risk_premia[factor_idx] + stats::qnorm(ci_level) * tfrp$standard_errors[factor_idx],
      tfrp$risk_premia[factor_idx] - stats::qnorm(ci_level) * tfrp$standard_errors[factor_idx]
    ),
    colour = c("TFRP", "TFRP CI", "TFRP CI")
  )

  # Create the ggplot
  plot(ggplot2::ggplot() +
         # Plot points for the first dataframe (MR method)
         ggplot2::geom_point(data = df, ggplot2::aes(
           x = Model,
           y = Value,
           colour = colour
         ), size = 1) +
         # Add dashed horizontal lines for the second dataframe (TFRP method)
         ggplot2::geom_hline(ggplot2::aes(yintercept = df_lines$values[1], color = "TFRP"),
                             linetype = "dashed", size = 1) +
         ggplot2::geom_hline(ggplot2::aes(yintercept = df_lines$values[2], color = "TFRP CI"),
                             linetype = "dashed", size = 1) +
         ggplot2::geom_hline(ggplot2::aes(yintercept = df_lines$values[3], color = "TFRP CI"),
                             linetype = "dashed", size = 1) +
         # Customize the color mapping
         ggplot2::scale_color_manual(
           values = c("MR" = "blue", "MR CI" = "red", "TFRP" = "blue", "TFRP CI" = "red")
         ) +
         # Add custom legend title
         ggplot2::guides(
           color = ggplot2::guide_legend(title = "")
         ) +
         # Adjust theme for better visualization
         ggplot2::theme(
           text = ggplot2::element_text(size = 22),
           panel.background = ggplot2::element_rect(fill = "white"),
           panel.grid.major = ggplot2::element_line(color = 'gray'),
           panel.grid.minor = ggplot2::element_line(color = 'gray'),
           axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
           axis.title.x = ggplot2::element_blank(),
           axis.text.y = ggplot2::element_text(size = 12),
           plot.title = ggplot2::element_text(hjust = 0.5)
         ) +
         # Set plot title
         ggplot2::ggtitle(paste(
           colnames(factors51[,-1])[factor_idx],
           #" (significance freq. ",
           #significance_frequency * 100,
           #"%)",
           sep = ""
         ))
  )


  if (save_plot) {

    factor_names = c(colnames(factors51[,-1]), "useless")

    figure_name = paste(
      "ranked_risk_premia_",
      factor_names[factor_idx],
      "_",
      name,
      sep=""
    )

    path_to_fig = "inst/figures"
    fig_height = 10
    fig_width = 22
    fig_device = "pdf"
    fig_dpi = 700
    fig_units = "cm"

    name_image = paste(
      figure_name,
      ".",
      fig_device,
      sep=""
    )

    ggplot2::ggsave(
      filename=name_image,
      device=fig_device,
      path=path_to_fig,
      width=fig_width,
      height=fig_height,
      units=fig_units,
      dpi=fig_dpi
    )
  }

}

ComputeSelectionFrequency = function(
    results,
    factors,
    indices_factors_selected=1:52,
    initial_n_factors = 10
    ) {

  results = results[results[,"n_factors"] == initial_n_factors,]
  selection_frequency = c()

  for (fac in 1:length(indices_factors_selected)) {

    fac_idx = indices_factors_selected[fac]
    included = apply(results[,8:17] == fac_idx, 1, any)
    selected = apply(results[,18:27] == fac_idx, 1, any)
    n_included = sum(included, na.rm=TRUE)
    n_selected = sum(selected, na.rm = TRUE)
    if (n_selected == 0) {
      selection_frequency = c(selection_frequency, 0)
    } else {
      selection_frequency = c(selection_frequency, n_selected / n_included)
    }

  }

  return(selection_frequency)
}

PlotKRSRiskPremia = function(
    results,
    returns,
    factors,
    factor_selection_frequencies,
    indices_factors_selected = 1:52,
    CI_level = .1,
    initial_n_factors = 10,
    # idx_factors = 1:52,
    name = "",
    save_plot = FALSE
) {

  krs_results = intrinsicFRP::FRP(
    returns,
    factors[, indices_factors_selected],
    include_standard_errors = TRUE
  )

  results = results[results[,"n_factors"] == initial_n_factors,]
  selection_frequencies = factor_selection_frequencies[indices_factors_selected]
  for (fac in 1:length(indices_factors_selected)) {

    fac_idx = indices_factors_selected[fac]
    colnames(factors)[fac_idx] = paste(
      colnames(factors)[fac_idx],
      " (",
      round(selection_frequencies[fac] * 100, 1),
      "%)",
      sep=""
    )

  }

  risk_premia = krs_results$risk_premia
  risk_premia_se = krs_results$standard_errors
  factors = factors[,indices_factors_selected]

  # create dataframe
  quantile = stats::qnorm(1. - CI_level/2.)
  df = data.frame(
    Factor = colnames(factors),
    risk_premia = risk_premia,
    standard_errors = risk_premia_se,
    Significant = (0. < risk_premia - quantile * risk_premia_se) |
      (0. > risk_premia + quantile * risk_premia_se)
  )

  df$Factor = factor(df$Factor, levels = df$Factor)

  plot(
    ggplot2::ggplot(df, ggplot2::aes(
      x = .data$Factor,
      y = .data$risk_premia,
      fill = .data$Significant
    )) +
      ggplot2::theme(
        text=ggplot2::element_text(size=22),
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = ggplot2::element_line(color = 'gray'),
        panel.grid.minor = ggplot2::element_line(color = 'gray'),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = "dodge",
        width=0.7
      ) +
      ggplot2::ylab("KRS risk Premia") +
      ggplot2::geom_errorbar(ggplot2::aes(
        x=as.factor(Factor),
        ymin=risk_premia - quantile * standard_errors,
        ymax=risk_premia + quantile * standard_errors),
        linewidth=.8,
        position = ggplot2::position_dodge(0.5),
        width = 0.25
      ) #+
    #ggplot2::coord_flip()


  )

  if (save_plot) {


    say = 10
    figure_name = paste(
      "krs_risk_premia_",
      say,
      "perc_",
      name,
      sep=""
    )

    path_to_fig = "inst/figures"
    fig_height = 16
    fig_width = 30
    fig_device = "pdf"
    fig_dpi = 700
    fig_units = "cm"

    name_image = paste(
      figure_name,
      ".",
      fig_device,
      sep=""
    )

    ggplot2::ggsave(
      filename=name_image,
      device=fig_device,
      path=path_to_fig,
      width=fig_width,
      height=fig_height,
      units=fig_units,
      dpi=fig_dpi
    )

  }

}
