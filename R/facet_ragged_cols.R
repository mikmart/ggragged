#' Lay out panels in a grid of ragged columns
#'
#' @param rows,cols A set of variables or expressions quoted by [ggplot2::vars()],
#'   the combinations of which define panels to be inlcuded in the grid. Panels
#'   for `rows` are wrapped independently within `cols` to form a ragged grid.
#' @inheritParams ggplot2::facet_wrap
#'
#' @examples
#' p <- ggplot(Indometh, aes(time, conc)) + geom_line()
#'
#' # Ragged grid with cohorts in separate columns
#' p + facet_ragged_cols(
#'   vars(Subject = as.character(Subject)),
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   labeller = label_both
#' )
#' @export
facet_ragged_cols <- function(rows, cols, labeller = "label_value") {
  ggproto(
    NULL,
    FacetRaggedCols,
    params = list(
      rows = rlang::quos_auto_name(rows),
      cols = rlang::quos_auto_name(cols),
      labeller = labeller
    )
  )
}

#' @export
FacetRaggedCols <- ggproto(
  "FacetRaggedCols",
  FacetWrap,

  setup_params = function(data, params) {
    params <- FacetWrap$setup_params(data, params)

    # Add parameters expected by facet_wrap
    params$free <- list(x = FALSE, y = FALSE)
    params$strip.position <- "right"
    params$facets <- params$rows

    params
  },

  compute_layout = function(data, params) {
    rows <- params$rows
    cols <- params$cols
    vars <- c(cols, rows)

    panels <- combine_vars(
      data = data,
      env = params$plot_env,
      vars = vars,
      drop = TRUE
    )
    panels <- vctrs::vec_sort(panels)
    panel_id <- seq_len(nrow(panels))

    # Map variables to layout
    g <- panels[names(cols)]
    r <- vctrs::vec_group_rle(g)
    n <- vctrs::field(r, "length")
    i <- sequence(n)
    j <- rep(seq_along(n), n)

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

    # Render strips for cols facet_wrap doesn't know about these.
    strip_data_cols <- vctrs::vec_unique(layout[names(params$cols)])
    strips <- render_strips(strip_data_cols, NULL, params$labeller, theme)

    # For each row, find the farthest out column to add strips to
    strip_layout_col <- vctrs::vec_unique(layout$COL)
    strip_layout_row <- rep(1L, length(strip_layout_col))
    strip_name <- sprintf("strip-t-%d", strip_layout_col)

    # Map strip position in layout to position in gtable
    panel_pos_rows <- panel_rows(panel_table)
    panel_pos_cols <- panel_cols(panel_table)
    strip_pos_t <- panel_pos_rows$t[strip_layout_row]
    strip_pos_l <- panel_pos_cols$r[strip_layout_col]

    # Add space in the margin for farthest out strips, and then strip grobs
    panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), min(strip_pos_t) - 1)
    panel_table <- gtable_add_grob(panel_table, strips$x$top, strip_pos_t, strip_pos_l, clip = "on", name = strip_name, z = 2)

    panel_table
  },

  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  },
)
