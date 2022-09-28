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
  modalDialog(ggiraph::renderggiraph({
    i <- as.integer(gsub(".*_(.*?)_.*", "\\1", id))
    j <- as.integer(gsub("_(.*)", "", id)) + 1

    h_vals <- seq(-.4, 1.2, .3)
    v_vals <- seq(0, 1.0, .25)

    if (!is.na(contrasts_draws)) {
      xrange <- contrasts_draws[[1]][i, , j] %>%
        range(na.rm = TRUE)
      yvals <- contrasts_draws[[1]][i, , j]
      yvals <- yvals / max(yvals)
    } else {
      xrange <- contrasts[, 1:2, ] %>%
        apply(1:2, function (row)
          row[1] + c(-3, 3) * row[2]) %>%
        range(na.rm = TRUE)
      xvals <- seq(min(xrange), max(xrange), length.out = 501L)
      yvals <-
        dnorm(xvals, contrasts[i, 1, j], contrasts[i, 2, j])
      yvals <- yvals / max(yvals)
      lower <- which.min(abs(xvals - contrasts[i, 4, j]))
      upper <- which.min(abs(xvals - contrasts[i, 5, j]))
    }

    distribution <- ggplotify::as.ggplot(function() {
      if (!is.na(contrasts_draws[[1]])) {
        hist(
          yvals,
          main = paste0(ev[i + 1, ]$Group, " - ", ev[j, ]$Group),
          xlab = "First Difference",
          ylab = "Frequency",
          xlim = c(min(yvals),
                   max(yvals))
        )
      } else {
        plot(
          1,
          1,
          main = paste0(ev[i + 1, ]$Group, " - ", ev[j, ]$Group),
          xlab = "First Difference",
          ylab = "Density",
          xlim = xrange,
          ylim = c(0, 1.25),
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
        segments(xvals[yvals == 1],
                 0,
                 xvals[yvals == 1],
                 yvals[yvals == 1],
                 col = "gray50")
        arrows(
          xvals_95[lower],
          0.5,
          xvals_95[upper],
          0.5,
          angle = 90,
          code = 3,
          length = 0.05
        )
        points(xvals[yvals == 1],
               0.5,
               pch = 19)
      }
      abline(v = 0, col = "red")
    })

    ggiraph::girafe(code = print(distribution))
  }))
}

ui <- shiny::fluidPage(
  # Arrange the layout
  shiny::titlePanel("Lower Triangular Matrix"),
  shiny::fluidPage(downloadButton('save', label = "Save as PDF")),
  shiny::fluidRow(
    tags$div(
      style = "position: relative;",
      ggiraph::ggiraphOutput(outputId = "triangularmatrix", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  plot_ci <-
    build_plot_point_estimates(
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
      column_vis
    )

  # Preparing for output using ggiraph
  output$triangularmatrix <- ggiraph::renderggiraph({
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
column_vis <- getShinyOption("column_vis")

# Run the shiny application
shiny::shinyApp(ui = ui, server = server)
