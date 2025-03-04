utils::globalVariables(c(
  "exp_val",
  "stat",
  "pval",
  "hatched",
  "p_val",
  "tooltip",
  "x",
  "y",
  "color_palette"
))

# Plot first difference distributions in a modal
plotModal <- function(session, id, groups) {
  modalDialog(ggiraph::renderGirafe({
    i <- as.integer(gsub(".*_(.*?)_.*", "\\1", id))
    j <- as.integer(gsub("_(.*)", "", id)) + 1

    if (type == "analytical") {
      xrange <- contrasts[i, 1, j] + c(-5, 5) * contrasts[i, 2, j]
      xvals <- seq(min(xrange), max(xrange), length.out = 501L)
      yvals <-
        dnorm(xvals, contrasts[i, 1, j], contrasts[i, 2, j])
      y_mid <- 0.5 * max(yvals)
    } else {
      xrange <- contrasts_draws[i, , j] %>%
        range(na.rm = TRUE)
      xrange <- xrange + c(-0.15, .15) * abs(diff(xrange))
      yvals <- contrasts_draws[i, , j]
    }
    point <- contrasts[i, 1, j]
    lower <- contrasts[i, 4, j]
    upper <- contrasts[i, 5, j]

    distribution <- ggplotify::as.ggplot(function() {
      if (type == "analytical") {
        plot(
          1,
          1,
          main = paste0(ev[i + 1,]$Group, " - ", ev[j,]$Group),
          xlab = "First Difference",
          ylab = "Density",
          xlim = xrange,
          ylim = c(0, max(yvals)),
          type = "n"
        )

        xvals_95 <- xvals
        yvals_95 <- yvals
        polygon(
          c(xvals_95, rev(xvals_95)),
          c(yvals_95, rep(0, length(yvals_95))),
          border = "gray50",
          col = adjustcolor("gray50", alpha.f = .5)
        )
      } else {
        histogram <- hist(yvals,
                          plot = FALSE,
                          breaks = 50)
        histogram$density <-
          histogram$density / sum(histogram$density)
        y_mid <- 0.5 * max(histogram$density)
        plot(
          histogram,
          freq = FALSE,
          main = paste0(ev[i + 1, ]$Group, " - ", ev[j, ]$Group),
          xlab = "First Difference",
          ylab = "Proportion of draws",
          xlim = xrange,
          col = adjustcolor("gray50", alpha.f = .5),
          border = adjustcolor("gray10", alpha.f = .5)
        )
        box()
      }
      arrows(
        lower,
        y_mid,
        upper,
        y_mid,
        angle = 90,
        code = 3,
        length = 0.05
      )
      points(point,
             y_mid,
             pch = 19)
      if (sign(prod(xrange)) == -1)
        abline(v = 0, col = "red")
    })

    ggiraph::girafe(code = print(distribution))
  }))
}

ui <- shiny::fluidPage(
  # Arrange the layout
  shiny::titlePanel("Triangular visualization"),
  shiny::fluidPage(downloadButton('save', label = "Save as PDF")),
  shiny::fluidRow(
    tags$div(
      style = "position: relative;",
      ggiraph::girafeOutput(outputId = "triangularmatrix", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  plot_ci <-
    triviz:::build_plot_point_estimates_interactive(
      ev,
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
      one_tailed_test
    )

  # Preparing for output using ggiraph
  output$triangularmatrix <- ggiraph::renderGirafe({
    plot <- ggiraph::girafe(ggobj = plot_ci, width = 10)
    plot <- ggiraph::girafe_options(
      plot,
      ggiraph::opts_hover(css = "cursor: pointer;"),
      ggiraph::opts_selection(
        css = NULL,
        type = "single",
        only_shiny = TRUE
      ),
      ggiraph::opts_toolbar(
        position = "topright",
        saveaspng = TRUE,
        pngname = 'triviz_plot'
      )
    )

    if (interactive())
      print(plot)
  })

  # Open modal on plot click using _selected
  shiny::observeEvent(
    input$triangularmatrix_selected,
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    shiny::showModal(
      plotModal(session, input$triangularmatrix_selected, length(groups))
    )
  )

  # Adding Download Button (as .pdf)
  output$save = shiny::downloadHandler(
    filename = 'triviz_plot.pdf',
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot_ci,
        device = cairo_pdf,
        width = 11,
        height = 4,
        dpi = 300,
        units = "in"
      )
    }
  )
}

# Store variables in internal shiny space
ev <- getShinyOption("ev")
ev_draws <- getShinyOption("ev_draws")
contrasts <- getShinyOption("contrasts")
type <- getShinyOption("type")
contrasts_draws <- getShinyOption("contrasts_draws")
groups <- getShinyOption("groups")
x_min <- getShinyOption("x_min")
x_max <- getShinyOption("x_max")
round_dec <- getShinyOption("round_dec")
color_palette <- getShinyOption("color_palette")
p_val_threshold <- getShinyOption("p_val_threshold")
p_val_type <- getShinyOption("p_val_type")
p_bars <- getShinyOption("p_bars")
caption_exp <- getShinyOption("caption_exp")
caption_stat <- getShinyOption("caption_stat")
add_vline <- getShinyOption("add_vline")
title <- getShinyOption("title")
one_tailed_test <- getShinyOption("one_tailed_test")

# Run the shiny application
shiny::shinyApp(ui = ui, server = server)
