#' Lay out panels in a grid of ragged rows
#'
#' @param rows,cols A set of variables or expressions quoted by [ggplot2::vars()],
#'   the combinations of which define panels to be inlcuded in the grid. Panels
#'   for `cols` are wrapped independently within `rows` to form a ragged grid.
#' @param ncol Number of columns on a row. If the number of panels on a row
#'   exceeds this, the remaining panels will be wrapped on to a new row.
#' @inheritParams ggplot2::facet_wrap
#'
#' @examples
#' p <- ggplot(Indometh, aes(time, conc)) + geom_line()
#'
#' # Ragged grid with cohorts on separate rows
#' p + facet_ragged_rows(
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   vars(Subject = as.character(Subject)),
#'   labeller = label_both
#' )
#'
#' # Specify ncol to wrap long rows
#' p + facet_ragged_rows(
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   vars(Subject = as.character(Subject)),
#'   labeller = label_both,
#'   ncol = 3
#' )
#' @export
facet_ragged_rows <- function(rows, cols, ncol = NULL, labeller = "label_value") {
  ggproto(
    NULL,
    FacetRaggedRows,
    params = list(
      rows = rlang::quos_auto_name(rows),
      cols = rlang::quos_auto_name(cols),
      ncol = ncol,
      labeller = labeller
    )
  )
}

#' @export
FacetRaggedRows <- ggproto(
  "FacetRaggedRows",
  FacetWrap,

  setup_params = function(data, params) {
    params <- FacetWrap$setup_params(data, params)

    # Add parameters expected by facet_wrap
    params$free <- list(x = FALSE, y = FALSE)
    params$strip.position <- "top"
    params$facets <- params$cols

    params
  },

  compute_layout = function(data, params) {
    rows <- params$rows
    cols <- params$cols
    vars <- c(rows, cols)

    panels <- combine_vars(
      data = data,
      env = params$plot_env,
      vars = vars,
      drop = TRUE
    )
    panels <- vctrs::vec_sort(panels)
    panel_id <- seq_len(nrow(panels))

    # Map variables to layout
    g <- panels[names(rows)]
    r <- vctrs::vec_group_rle(g)
    n <- vctrs::field(r, "length")
    i <- rep(seq_along(n), n)
    j <- sequence(n)

    # Wrap rows within groups
    if (!is.null(params$ncol)) {
      k <- 1L + (j - 1L) %/% params$ncol
      j <- 1L + (j - 1L) %% params$ncol
      i <- i + cumsum((k > 1) & (j == 1))
    }

    layout <- data.frame(
      PANEL = panel_id,
      ROW = i,
      COL = j,
      SCALE_X = 1L,
      SCALE_Y = 1L
    )

    cbind(layout, panels)
  },

  map_data = function(data, layout, params) {
    FacetGrid$map_data(data, layout, params)
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    panel_table <- FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)

    # Render strips for rows. facet_wrap doesn't know about these.
    # Vars could be wrapped across multiple layout rows if ncol is set.
    layout_rows <- vctrs::vec_unique(layout[c("ROW", names(params$rows))])
    strips <- render_strips(NULL, layout_rows[-1L], params$labeller, theme)

    # For each row, find the furthest out column to add strips to.
    strip_layout <- dplyr::summarise(dplyr::group_by(layout, ROW), COL = max(COL))

    # Map strip position in layout to position in gtable.
    panel_pos_rows <- panel_rows(panel_table)
    panel_pos_cols <- panel_cols(panel_table)
    strip_pos_t <- panel_pos_rows$t[strip_layout$ROW]
    strip_pos_l <- panel_pos_cols$r[strip_layout$COL] + 1
    strip_name <- sprintf("strip-r-%d", strip_layout$ROW)

    # Justify strips to start at the edge of the panel.
    strips$y$right <- lapply(strips$y$right, function(strip) {
      gtable_add_cols(strip, gtable_width(strip), 0)
    })

    # Add strips identifying the row variable to farthest out panels on each row.
    panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right) / 2, max(strip_pos_l) + 1)
    panel_table <- gtable_add_grob(panel_table, strips$y$right, strip_pos_t, strip_pos_l, clip = "off", name = strip_name, z = 2)

    panel_table
  },

  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  },
)
