#' @title build_plot_continuous_estimates
#'
#' @description Internal function
#'
#' @noRd

build_plot_continuous_estimates <-
  function(ev,
           variable,
           type,
           variable_label,
           contrasts,
           groups,
           y_min,
           y_max,
           x_min,
           x_max,
           round_dec,
           color_palette,
           one_tailed_test,
           p_val_threshold,
           p_val_type) {
    ev <- ev %>%
      mutate(Group = as.character(Group))

    n_ticks_x <- 4
    n_ticks_y <- 5

    breaks_pretty_y <- labeling::extended(
      m = n_ticks_y,
      dmin = y_min,
      dmax = y_max,
      only.loose = TRUE
    )
    y_max <- breaks_pretty_y[length(breaks_pretty_y)]
    y_min <- breaks_pretty_y[1]

    # Define group with the highest value range (min confidence to max confidence)
    max_y_range <- abs(y_max - y_min)
    max_x_range <- abs(x_max - x_min)

    # Define sequence of group ranges
    groups_y_range <-
      rev(seq(max_y_range, length(unique(ev$Group)) * max_y_range, by = max_y_range))
    names(groups_y_range) <- unique(ev$Group)

    ratio <- abs(x_max - x_min) / n_ticks_x * 3

    breaks_pretty_y_mod <- breaks_pretty_y
    breaks_pretty_y_mod[length(breaks_pretty_y)] <- ""
    labels_y <-
      rep(breaks_pretty_y_mod, times = length(unique(ev$Group)))
    labels_y[length(labels_y)] <-
      breaks_pretty_y[length(breaks_pretty_y)]

    breaks_y <- c()
    for (i in 1:length(unique(ev$Group))) {
      breaks_y <- c(breaks_y, breaks_pretty_y + ((i - 1) * max_y_range))
    }

    labels_x <-
      round(seq(x_min, x_max, abs(x_max - x_min) / n_ticks_x), round_dec)
    labels_x <-
      c(labels_x, rep("", times = (length(unique(
        ev$Group
      )) - 1) * length(labels_x)))
    breaks_x <-
      round(seq(x_min, x_max, abs(x_max - x_min) / n_ticks_x), round_dec)
    for (i in 2:length(unique(ev$Group)) - 1) {
      breaks_x <- c(
        breaks_x,
        seq(
          x_max + max_y_range * ratio * (i - 1),
          x_max + max_y_range * ratio * i,
          abs((x_max + max_y_range * ratio * i) - (x_max +
                                                     max_y_range * ratio * (i - 1))
          ) / n_ticks_x
        )
      )
    }

    # Initialize ggplot object with scales
    plot_ci <- ggplot2::ggplot() + ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7),
      axis.text.x = ggplot2::element_text(size = 7),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour = "grey75",
        linetype = 3,
        size = 0.5
      ),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 0, 1, 1), "cm"),
      legend.key.width = grid::unit(0.25, "cm"),
      legend.margin = ggplot2::margin(c(0, .5, 0, .5), unit = 'cm'),
      legend.key.height = grid::unit(max_y_range * length(unique(ev$Group)), "line"),
      legend.text = ggplot2::element_text(size = 7)
    ) +
      ggplot2::scale_x_continuous(
        expand = c(0, .001),
        limits = c(x_min, x_max + ((
          length(unique(ev$Group)) - 1
        ) * max_y_range * ratio)),
        breaks = breaks_x,
        labels = labels_x
      ) +
      ggplot2::coord_fixed(ratio = ratio,
                           clip = "off") +
      ggplot2::scale_y_continuous(
        limits = c(0, max(groups_y_range)),
        breaks = breaks_y,
        labels = labels_y,
        expand = c(0, 0)
      )

    # Define axis descriptions
    caption_exp <- data.frame(exp_val = c(variable_label))
    caption_stat <-
      data.frame(stat = c("Statistical Significances of\nPairwise Differences"))
    caption_pval <- data.frame(pval = c(p_val_type))

    # Write axis labels
    plot_ci <- plot_ci +
      ggplot2::geom_text(
        data = caption_exp,
        ggplot2::aes(label = caption_exp),
        size = 2.5,
        y = -max_y_range / n_ticks_y
      ) +
      ggplot2::geom_text(
        data = caption_stat,
        ggplot2::aes(label = stat),
        size = 2.5,
        x = (x_max + (x_max + ((length(unique(ev$Group)) - 1) * max_y_range * ratio
        ))) / 2,
        y = -max_y_range / n_ticks_y
      ) +
      ggplot2::geom_text(
        data = caption_pval,
        ggplot2::aes(label = pval),
        size = 2.5,
        x = (x_max + ((
          length(unique(ev$Group)) - 1
        ) * max_y_range * ratio)),
        y = -max_y_range / n_ticks_y,
        hjust = grid::unit(-1.0, "cm")
      )

    # Draw basic layout of left part of the plot
    plot_ci <- plot_ci +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = x_min,
          xmax = x_max,
          ymin = groups_y_range - max_y_range,
          ymax = groups_y_range * .999,
          colour = I("black")
        ),
        fill = NA
      ) +
      ggplot2::geom_text(
        x = x_min - (abs(x_max) - abs(x_min)) / n_ticks_y,
        y = groups_y_range - max_y_range * .5,
        vjust = "center",
        hjust = "middle",
        angle = 90,
        ggplot2::aes(label = unique(ev$Group)),
        size = 2,
        fontface = 'bold'
      )

    # Draw lines and confidence intervals
    for (group in shiny::req(unique(ev$Group))) {
      ev_filtered <- ev %>%
        filter(Group == group) %>%
        as.data.frame()

      plot_ci <- plot_ci +
        ggplot2::geom_ribbon(
          data = cbind.data.frame(
            x = ev_filtered[, variable],
            y = groups_y_range[group] -
              max_y_range + ev_filtered$EV,
            lwr = groups_y_range[group] -
              max_y_range + ev_filtered$lower,
            upr = groups_y_range[group] -
              max_y_range + ev_filtered$upper
          ),
          ggplot2::aes(
            x = x,
            y = y,
            ymin = lwr,
            ymax = upr
          ),
          alpha = 0.5
        ) +
        ggplot2::geom_line(
          data = cbind.data.frame(x = ev_filtered[, variable],
                                  y = groups_y_range[group] -
                                    max_y_range + ev_filtered$EV),
          ggplot2::aes(x, y)
        )
    }

    contrasts <- data.frame(contrasts) %>%
      mutate(Group1 = as.character(Group1)) %>%
      mutate(Group2 = as.character(Group2)) %>%
      arrange(Group2, Group1)

    ratio_y_range <- max_y_range * ratio
    unique_groups <- unique(ev$Group)
    # Draw p-value boxes and arrows
    for (group1 in 1:length(unique_groups)) {
      filtered_group <- contrasts %>%
        filter(Group1 == unique_groups[group1])

      # Arrows
      if (group1 < length(unique_groups)) {
        plot_ci <- plot_ci +
          ggplot2::geom_segment(
            data = cbind.data.frame(x = group1 - 1,
                                    y = max_y_range * (length(
                                      unique_groups
                                    ) - group1)),
            ggplot2::aes(
              x = x * ratio_y_range + x_max,
              y = y + max_y_range * 0.5,
              xend = x * ratio_y_range + ratio_y_range *
                0.5 + x_max,
              yend = y + max_y_range * 0.5
            )
          ) +
          ggplot2::geom_segment(
            data = cbind.data.frame(x = group1 - 1,
                                    y = max_y_range * (length(
                                      unique_groups
                                    ) - group1)),
            ggplot2::aes(
              x = x * ratio_y_range + ratio_y_range * 0.5 + x_max,
              y = y + max_y_range * 0.5,
              xend = x * ratio_y_range + ratio_y_range *
                0.5 + x_max,
              yend = y
            ),
            arrow = ggplot2::arrow(length = grid::unit(0.25, "cm"))
          )
      }

      for (group2 in 1:length(unique_groups)) {
        if ((group2 > 1) & (group1 < group2)) {
          data <- cbind.data.frame(
            x = group1 - 1,
            y = max_y_range * (length(unique_groups) - group2),
            group1 = unique_groups[group1],
            group2 = unique_groups[group2],
            tooltip = paste0(
              "<i>Click on tile to see first differences.</i>\n",
              "First Difference: ",
              unique_groups[group1],
              " - ",
              unique_groups[group2]
            )
          )
          filtered_group_pval <- filtered_group %>%
            filter(Group2 == unique_groups[group2]) %>%
            mutate(p = replace(p, p == 0, 0.0001))

          p_val_plot <-
            ggplot2::ggplot(filtered_group_pval,
                            ggplot2::aes_string(variable, "p")) +
            ggplot2::geom_col(width = 0.25, ggplot2::aes(fill = p)) +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1))
          if (!one_tailed_test) {
            if (type %in% c("analytical", "simulation", "bootstrap")) {
              p_val_plot <- p_val_plot +
                ggplot2::scale_fill_continuous(
                  type = color_palette,
                  trans = "reverse",
                  "",
                  limits = c(0, 1)
                )
            } else {
              p_val_plot <- p_val_plot +
                ggplot2::scale_fill_continuous(type = color_palette,
                                               trans = "reverse",
                                               "",
                                               limits = c(1, 0))
            }
          } else {
            if (type == "bayesian") {
              p_val_plot <- p_val_plot +
                ggplot2::scale_fill_gradientn(colors = c(rev(viridis::viridis(4)), viridis::viridis(4)),
                                              limits = c(0, 1))
            } else {
              p_val_plot <- p_val_plot +
                ggplot2::scale_fill_gradientn(
                  colors = c(rev(viridis::viridis(4)), viridis::viridis(4)),
                  trans = 'reverse',
                  limits = c(1, 0)
                )
            }
          }

          if (!is.na(p_val_threshold)) {
            if (!one_tailed_test) {
              p_val_plot <- p_val_plot +
                ggplot2::geom_hline(
                  yintercept = ifelse(
                    type == "bayesian",
                    1 - p_val_threshold,
                    p_val_threshold
                  ),
                  size = .5
                )
            } else {
              p_val_plot <- p_val_plot +
                ggplot2::geom_hline(yintercept = p_val_threshold, size = .5) +
                ggplot2::geom_hline(yintercept = 1 - p_val_threshold,
                                    size = .5)
            }
          }

          p_val_plot <- p_val_plot +
            ggplot2::theme(
              axis.text = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              panel.border = ggplot2::element_blank(),
              plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
              axis.ticks.length = grid::unit(0, "pt"),
              plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
              panel.spacing = grid::unit(c(0, 0, 0, 0), "cm"),
              legend.position = "none"
            )

          # Draw p-value box (with interactive tooltip)
          plot_ci <- plot_ci +
            ggplot2::annotation_custom(
              grob = ggplot2::ggplotGrob(p_val_plot),
              xmin = data$x * ratio_y_range + x_max,
              xmax = data$x * ratio_y_range + ratio_y_range + x_max,
              ymin = data$y,
              ymax = data$y + max_y_range
            ) +
            ggiraph::geom_rect_interactive(
              data = data,
              ggplot2::aes(
                xmin = x * ratio_y_range + x_max,
                xmax = x * ratio_y_range +
                  ratio_y_range + x_max,
                ymin = y,
                ymax = y + max_y_range,
                data_id = paste0(group1, "+", group2, "_fd"),
                colour = "black",
                tooltip = tooltip
              ),
              alpha = 0.01
            )
        }
      }
    }


    plot_ci <- plot_ci +
      ggplot2::geom_col(data = ev, ggplot2::aes(fill = p, x = 0, y = 0))

    if (!one_tailed_test) {
      if (type == "bayesian") {
        plot_ci <- plot_ci +
          ggplot2::scale_fill_continuous(
            type = color_palette,
            trans = 'reverse',
            name = "",
            limits = c(1, 0),
            breaks = seq(0, 1, by = 0.25)
          )
      } else {
        plot_ci <- plot_ci +
          ggplot2::scale_fill_continuous(
            type = color_palette,
            trans = "reverse",
            name = "",
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.25)
          )
      }
    } else {
      plot_ci <- plot_ci +
        ggplot2::scale_fill_gradientn(
          colors = c(rev(viridis::viridis(4)), viridis::viridis(4)),
          breaks = seq(0, 1, by = 0.25),
          limits = c(1, 0),
          name = "",
          trans = 'reverse'
        )
    }

    return(plot_ci)
  }
