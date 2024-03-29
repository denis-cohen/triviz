#' @title build_plot_point_estimates_interactive
#'
#' @description Internal function
#'
#' @noRd

build_plot_point_estimates_interactive <-
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
           p_bars,
           caption_exp,
           caption_stat,
           add_vline,
           title,
           one_tailed_test) {
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
      ggplot2::ggplot() +
      ggplot2::theme(
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
        panel.spacing = grid::unit(c(1, 0, 1, 1), "cm"),
        plot.margin = grid::unit(c(1, 0, 1, 2), "cm"),
        legend.box.margin = ggplot2::margin(0, 0, 0, 0),
        legend.key.width = grid::unit(0.25, "cm"),
        legend.key.height = grid::unit(length(groups) * .25, "cm"),
        legend.margin = ggplot2::margin(c(0, .5, 0, 0), unit =
                                          'cm'),
        legend.text = ggplot2::element_text(size =
                                              7)
      ) +
      ggplot2::scale_x_continuous(breaks = breaks, expand = c(0, 0)) +
      ggplot2::coord_fixed(ratio = ratio, clip = "off") +
      ggplot2::scale_y_continuous(limits = c(0, length(groups)), expand = c(0, 0))


    # Define axis descriptions
    caption_exp <- data.frame(exp_val = c(caption_exp))
    caption_stat <-
      data.frame(stat = c(caption_stat))
    caption_pval <- data.frame(pval = c(p_val_type))

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
      )

    if (!is.null(add_vline)) {
      plot_ci <- plot_ci +
        ggplot2::geom_vline(
          xintercept = add_vline,
          linetype = 1,
          color = "red",
          size = 0.33,
          alpha = 0.33
        )
    }

    plot_ci <- plot_ci +
      ggplot2::geom_text(
        ggplot2::aes(
          x = x_min - .1 * ratio,
          y = groups - .5,
          label = ev$Group
        ),
        size = 2,
        hjust = "right",
        vjust = "middle",
        angle = 0,
        fontface = 'bold'
      ) +
      ggplot2::geom_segment(ggplot2::aes(
        x = ev$lower,
        xend = ev$upper,
        y = groups - .5,
        yend = groups - .5
      )) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(ev$EV, groups - .5),
        size = 1.5,
        data_id = ev$Group,
        colour = "black",
        tooltip = paste0(
          "Expected value: ",
          ev$Group,
          "\n Estimate: ",
          format(
            round(ev$EV, 2),
            nsmall = 2,
            scientific = F
          ),
          paste0("\n ", round(100 * (
            1 - p_val_threshold
          ), 1), "%-Interval: ["),
          format(
            round(ev$lower, 2),
            nsmall = 2,
            scientific = F
          ),
          ";",
          format(
            round(ev$upper, 2),
            nsmall = 2,
            scientific = F
          ),
          paste0("]")
        )
      )

    # Write axis labels
    plot_ci <- plot_ci +
      ggplot2::geom_text(
        data = caption_exp,
        ggplot2::aes(label = exp_val),
        size = 2.5,
        x = (x_min + x_max) / 2,
        y = 0,
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
        vjust = grid::unit(2, "cm")
      ) +
      ggplot2::geom_text(
        data = caption_pval,
        ggplot2::aes(label = pval),
        size = 2.5,
        x = (x_max + (length(groups) - 1) * ratio),
        y = 0,
        vjust = grid::unit(4, "cm"),
        hjust = grid::unit(-0.5, "cm")
      )

    # Draw first differences with confidence intervals
    rows <-
      ifelse((is.null(nrow(contrasts[, , 1]))), 1, nrow(contrasts[, , 1]))
    data <- NULL
    for (i in 1:length(contrasts[1, 1,])) {
      for (j in rows:1) {
        if (i <= j) {
          data <- rbind.data.frame(
            data,
            cbind.data.frame(
              i = i,
              j = j,
              x = i - 1,
              y = j,
              p_val = contrasts[j, , i]["p"],
              hatched = ifelse(
                !is.na(p_val_threshold),
                ifelse(
                  type == "bayesian",
                  contrasts[j, , i]["p"] > 1 - p_val_threshold,
                  contrasts[j, , i]["p"] < p_val_threshold
                ),
                FALSE
              ),
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
                paste0("\n ", round(100 * (
                  1 - p_val_threshold
                ), 1), "%-Interval: ["),
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
              ),
              data_id = paste0(i - 1, "_", j, "_fd")
            )
          )
        }
      }
    }

    # Guiding arrows
    plot_ci <- plot_ci +
      ggplot2::geom_segment(
        data = data %>%
          dplyr::filter(i == j),
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
        arrow = ggplot2::arrow(length = grid::unit(3 * (2 + rows) ^ (-1.75), "cm"))
      )

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
          ggplot2::scale_fill_continuous(type = color_palette,
                                         trans = "reverse",
                                         "",
                                         limits = c(1, 0))
      }

      if (!is.na(p_val_threshold)) {
        p_val_plot <- p_val_plot +
          ggplot2::geom_hline(
            yintercept = ifelse(type == "bayesian",
                                1 - p_val_threshold,
                                p_val_threshold),
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
          ymin = length(contrasts[1, 1,]) - data$y,
          ymax = length(contrasts[1, 1,]) - data$y - 1
        )
    } else {
      # Draw Patterns and interactive rectangles (right part)
      plot_ci <- plot_ci +
        ggpattern::geom_rect_pattern(
          data = data,
          ggplot2::aes(
            fill = p_val,
            pattern_alpha = ifelse(hatched, 1, 0),
            xmin = x * ratio + abs(x_max),
            xmax = (x + 1) * ratio +
              abs(x_max),
            ymin = length(contrasts[1, 1, ]) -
              y,
            ymax = length(contrasts[1, 1, ]) -
              y - 1
          ),
          pattern = "stripe",
          pattern_colour = "gray35",
          pattern_fill = "white",
          pattern_angle = 45,
          pattern_density = .3 * (1 + rows) ^ (-1.75),
          pattern_spacing = 3 * (7 + rows) ^ (-1.75),
          color = "black"
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
          ymin = length(contrasts[1, 1,]) -
            y,
          ymax = length(contrasts[1, 1,]) -
            y - 1,
          data_id = data_id,
          colour = "black",
          tooltip = tooltip
        ),
        alpha = 0.01
      )

    # Add legend with custom color
    plot_ci <- plot_ci +
      ggplot2::geom_col(data = ev,
                        width = 0,
                        ggplot2::aes(fill = p, x = 0, y = p))

    if (one_tailed_test) {
      p_val_limits <- c(0, .55)
      p_val_breaks <- seq(0, .5, by = .125)
    } else {
      p_val_limits <- c(0, 1)
      p_val_breaks <- seq(0, 1, by = 0.25)
    }

    if (type == "bayesian") {
      plot_ci <- plot_ci +
        ggplot2::scale_fill_continuous(
          type = color_palette,
          "",
          limits = p_val_limits,
          breaks = p_val_breaks
        )
    } else {
      plot_ci <- plot_ci +
        ggplot2::scale_fill_continuous(
          type = color_palette,
          trans = 'reverse',
          "",
          limits = rev(p_val_limits),
          breaks = p_val_breaks
        )
    }

    # Optionally, add title
    if (!is.null(title)) {
      plot_ci <- plot_ci +
        ggplot2::ggtitle(title)
    }

    return(plot_ci)
  }
