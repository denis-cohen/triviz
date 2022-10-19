#' @title build_plot_point_estimates
#'
#' @description Internal function
#'
#' @noRd

build_plot_point_estimates <-
  function(ev,
           contrasts,
           type,
           groups,
           x_min,
           x_max,
           round_dec,
           color_palette,
           p_val_threshold,
           p_val_type,
           p_bars) {
    # Initialize ggplot object with scales

    n_ticks <- 5
    breaks <- labeling::extended(
      m = n_ticks,
      dmin = x_min,
      dmax = x_max,
      only.loose = TRUE
    )
    x_max <- breaks[length(breaks)]
    x_min <- breaks[1]
    ratio <- abs(x_max - x_min) / n_ticks

    plot_ci <-
      ggplot2::ggplot() + ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size =
                                              7),
        axis.ticks.y = ggplot2::element_blank(),
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
        panel.spacing = grid::unit(c(1, 0, 1, 0), "cm"),
        plot.margin = grid::unit(c(1, 0, 1, 1), "cm")
      ) +
      ggplot2::coord_fixed(ratio = ratio, clip = "off") +
      ggplot2::scale_x_continuous(breaks = breaks, expand = c(0, 0.00, 0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, length(groups)), expand = c(0, 0, 0, 0))

    # Define axis descriptions
    caption_exp <- data.frame(exp_val = c("Expected Values"))
    caption_stat <-
      data.frame(stat = c("Statistical Significance of\nPairwise Differences"))

    plot_ci <- plot_ci +
      ggplot2::geom_text(
        data = caption_exp,
        ggplot2::aes(label = exp_val),
        size = 2.5,
        x = (x_min + x_max) / 2,
        y = 0,
        fontface = "bold",
        vjust = grid::unit(4, "cm")
      ) +
      ggplot2::geom_text(
        data = caption_stat,
        ggplot2::aes(label = stat),
        size = 2.5,
        x = (x_max + (x_max + (length(
          groups
        ) - 1) * ratio)) / 2,
        y = 0,
        fontface = "bold",
        vjust = grid::unit(2, "cm"))

    # Draw basic layout of left part of the plot
    plot_ci <- plot_ci +
      ggpattern::scale_pattern_manual(values = c("none", "stripe")) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = x_min,
          xmax = x_max,
          ymin = groups - 1,
          ymax = groups - .0001,
          colour = I("black")
        ),
        fill = NA
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = x_min - (breaks[2] - breaks[1]) / 10,
          y = groups - .5,
          label = ev$Group
        ),
        size = 2,
        hjust = "center",
        vjust = "middle",
        angle = 90,
        fontface = 'bold'
      ) +
      ggplot2::geom_point(ggplot2::aes(ev$EV, groups - .5), size = 1) +
      ggplot2::geom_segment(ggplot2::aes(
        x = ev$lower,
        xend = ev$upper,
        y = groups - .5,
        yend = groups - .5
      ))

    # Draw first differences with Confidence Intervals
    rows <- ifelse((is.null(nrow(contrasts[, , 1]))), 1, nrow(contrasts[, , 1]))
    for (i in 1:length(contrasts[1, 1, ])) {
      for (j in rows:1) {
        data <- cbind.data.frame(
          x = i - 1,
          y = j,
          p_val = contrasts[j, , i]["p"],
          hatched = ifelse(!is.na(p_val_threshold),
                           ifelse(
                             type == "bayesian",
                             contrasts[j, , i]["p"] > 1 - p_val_threshold,
                             contrasts[j, , i]["p"] < p_val_threshold
                           ),
                           FALSE),
          tooltip = paste0(
            "<i>Click on tile to see ",
            ifelse(type == "bayesian", "posterior", "sampling"),
            " distribution.</i>\n",
            "First Difference: ",
            ev[j + 1, ]$Group,
            " - ",
            ev[i, ]$Group,
            "\n Estimate: ",
            format(
              round(contrasts[j, , i]["FD"], 2),
              nsmall = 2,
              scientific = F
            ),
            paste0("\n ", round(100 * (1 - p_val_threshold), 1), "%-Interval: ["),
            format(
              round(contrasts[j, , i]["lower"], 2),
              nsmall = 2,
              scientific = F
            ),
            ";",
            format(
              round(contrasts[j, , i]["upper"], 2),
              nsmall = 2,
              scientific = F
            ),
            paste0("] \n ", p_val_type, ": "),
            format(
              round(contrasts[j, , i]["p"], 2),
              nsmall = 2,
              scientific = F
            )
          )
        )
        if (i == j) {
          plot_ci <- plot_ci +
            ggplot2::geom_segment(
              data = data,
              ggplot2::aes(
                x = x * ratio + abs(x_max),
                y = length(contrasts[1, 1, ]) - y + .5,
                xend = (x + 0.5) * ratio +
                  abs(x_max),
                yend = length(contrasts[1, 1, ]) - y + .5
              )
            ) +
            ggplot2::geom_segment(
              data = data,
              ggplot2::aes(
                x = (x + 0.5) * ratio + x_max,
                y = length(contrasts[1, 1, ]) - y + .5,
                xend =  (x + 0.5) * ratio +
                  abs(x_max),
                yend = length(contrasts[1, 1, ]) - y
              ),
              arrow = ggplot2::arrow(length = grid::unit(0.25, "cm"))
            )
        }
        if (i <= j) {
          if (p_bars) {
            p_val_plot <-
              ggplot2::ggplot(data, ggplot2::aes_string(x = 0, "p_val")) +
              ggplot2::geom_col(width = 0.25, ggplot2::aes(fill = p_val)) +
              ggplot2::scale_x_continuous(expand = c(0, 0)) +
              ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
              ggplot2::theme(
                axis.text = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                axis.ticks.length = grid::unit(0, "pt"),
                plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
                panel.spacing = grid::unit(c(0, 0, 0, 0), "cm"),
                legend.position = "none"
              )
            if (type == "bayesian") {
              p_val_plot <- p_val_plot +
                ggplot2::scale_fill_continuous(type = color_palette,
                                               "",
                                               limits = c(0, 1))
            } else {
              p_val_plot <- p_val_plot +
                ggplot2::scale_fill_continuous(
                  type = color_palette,
                  trans = "reverse",
                  "",
                  limits = c(1, 0)
                )
            }

            if (!is.na(p_val_threshold)) {
              p_val_plot <- p_val_plot +
                ggplot2::geom_hline(
                  yintercept = ifelse(
                    type == "bayesian",
                    1 - p_val_threshold,
                    p_val_threshold
                  ),
                  size = .5,
                  color = "red"
                )
            }

            # Draw p-value box (with interactive tooltip)
            plot_ci <- plot_ci +
              ggplot2::annotation_custom(
                grob = ggplot2::ggplotGrob(p_val_plot),
                xmin = data$x * ratio + abs(x_max),
                xmax = (data$x + 1) * ratio + abs(x_max),
                ymin = length(contrasts[1, 1, ]) - data$y,
                ymax = length(contrasts[1, 1, ]) - data$y - 1
              )
          } else {
            # Draw Patterns and interactive rectangles (right part)
            plot_ci <- plot_ci +
              ggpattern::geom_rect_pattern(
                data = data,
                ggplot2::aes(
                  pattern = hatched,
                  fill = p_val,
                  xmin = x * ratio + abs(x_max),
                  xmax = (x + 1) * ratio +
                    abs(x_max),
                  ymin = length(contrasts[1, 1, ]) -
                    y,
                  ymax = length(contrasts[1, 1, ]) -
                    y - 1
                ),
                pattern_colour = "gray35",
                pattern_fill = "white",
                pattern_angle = 45,
                pattern_density = 0.005,
                pattern_spacing = 0.02
              ) +
              ggplot2::guides(pattern = "none")
          }

          plot_ci <- plot_ci +
            ggiraph::geom_rect_interactive(
              data = data,
              ggplot2::aes(
                xmin = x * ratio + abs(x_max),
                xmax = (x + 1) * ratio +
                  abs(x_max),
                ymin = length(contrasts[1, 1, ]) -
                  y,
                ymax = length(contrasts[1, 1, ]) -
                  y - 1,
                data_id = paste0(x, "_", y, "_fd"),
                tooltip = tooltip
              ),
              color = "black",
              alpha = 0.01
            )
        }
      }
    }

    # Add legend with custom color
    plot_ci <- plot_ci +
      ggplot2::geom_col(data = ev,
                        width = 0,
                        ggplot2::aes(fill = p, x = 0, y = p))

    if (type == "bayesian") {
      plot_ci <- plot_ci +
        ggplot2::scale_fill_continuous(
          type = color_palette,
          "",
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.25)
        )
    } else {
      plot_ci <- plot_ci +
        ggplot2::scale_fill_continuous(
          type = color_palette,
          trans = 'reverse',
          "",
          limits = c(1, 0),
          breaks = seq(0, 1, by = 0.25)
        )
    }

    plot_ci <- plot_ci +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = p_val_type)) +
      ggplot2::theme(legend.title=ggplot2::element_text(size=7, face = "bold"),
                     legend.text=ggplot2::element_text(size=7))

    return(plot_ci)
  }
