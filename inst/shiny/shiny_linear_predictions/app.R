utils::globalVariables(c("caption_exp", "stat", "pval", "tooltip", "x", "y"))

# Plot first difference distributions in a modal
plotModal <- function(session, id) {
  modalDialog(ggiraph::renderggiraph({
    group2 <- gsub(".*\\+(.*?)_.*", "\\1", id)
    group1 <- gsub("(.*)\\+.*", "\\1", id)

    filtered_group <- contrasts %>%
      filter(Group1 == group1, Group2 == group2)

    n_ticks_x <- 5
    fd_predictions <- ggplot2::ggplot() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(colour = "black", fill =
                                                   NA),
        plot.title = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::xlab(variable_caption) +
      ggplot2::ylab("First Difference") +
      ggplot2::ggtitle(paste0(group1, " - ", group2)) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  breaks = scales::breaks_extended(n_ticks_x)) +
      ggplot2::scale_y_continuous(expand = c(0, 0),
                                  breaks = scales::breaks_extended(n_ticks_x)) +
      ggplot2::geom_hline(yintercept = 0,
                          size = .5,
                          color = "red") +
      ggplot2::geom_line(data = cbind.data.frame(x = filtered_group[[variable]],
                                                 y = filtered_group$FD),
                         ggplot2::aes(x, y)) +
      ggplot2::geom_ribbon(
        data = cbind.data.frame(
          x = filtered_group[[variable]],
          y = filtered_group$FD,
          lwr = filtered_group$lower,
          upr = filtered_group$upper
        ),
        ggplot2::aes(
          x = x,
          y = y,
          ymin = lwr,
          ymax = upr
        ),
        alpha = 0.1
      )

    ggiraph::girafe(code = print(fd_predictions))
  }))
}

ui <- shiny::fluidPage(
  # Arrange the layout
  shiny::titlePanel("Triangular visualization"),
  fluidPage(shiny::downloadButton('save', label = "Save as PDF")),
  shiny::fluidRow(
    tags$div(
      style = "position: relative;",
      ggiraph::girafeOutput(outputId = "triangularmatrix", height = "900px")
    )
  )
)

server <- function(input, output, session) {
  plot_ci <-
    triviz:::build_plot_continuous_estimates_interactive(
      ev,
      variable,
      type,
      variable_caption,
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
      p_val_type
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
    shiny::showModal(plotModal(
      session, input$triangularmatrix_selected
    ))
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
variable <- getShinyOption("variable")
type <- getShinyOption("type")
variable_caption <- getShinyOption("variable_caption")
contrasts <- getShinyOption("contrasts")
x_min <- getShinyOption("x_min")
x_max <- getShinyOption("x_max")
y_min <- getShinyOption("y_min")
y_max <- getShinyOption("y_max")
round_dec <- getShinyOption("round_dec")
color_palette <- getShinyOption("color_palette")
one_tailed_test <- getShinyOption("one_tailed_test")
p_val_threshold <- getShinyOption("p_val_threshold")
p_val_type <- getShinyOption("p_val_type")

# Run the application
shiny::shinyApp(ui = ui, server = server)
