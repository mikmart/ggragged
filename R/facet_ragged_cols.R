#' @include facet_ragged_rows.R
#' @rdname facet_ragged
#' @export
facet_ragged_cols <- function(rows, cols, ..., scales = "fixed", switch = NULL, labeller = "label_value") {
  new_facet_ragged(FacetRaggedCols, rows, cols, ..., scales = scales, switch = switch, labeller = labeller)
}

FacetRaggedCols <- ggproto("FacetRaggedCols", FacetRagged,
  setup_params = function(data, params) {
    params <- FacetRagged$setup_params(data, params)

    # Add parameters expected by FacetWrap
    params$strip.position <- if (params$switch$y) "left" else "right"
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
    layout <- layout_ragged_cols(panels[names(cols)], params$free)

    cbind(layout, panels)
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    params$free$x <- FALSE # Always suppress intermediate axes in columns
    panel_table <- FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)

    # Render column strips that FacetWrap didn't know about
    strip_data <- vctrs::vec_unique(layout[names(params$cols)])
    strips <- render_strips(strip_data, NULL, params$labeller, theme)

    panel_pos_rows <- panel_rows(panel_table)
    panel_pos_cols <- panel_cols(panel_table)

    strip_layout_col <- seq_len(max(layout$COL))
    strip_pos_l <- panel_pos_cols$r[strip_layout_col]

    if (params$switch$x) {
      # Add strips to the bottom of the panels on the last row in each column
      strip_name <- sprintf("strip-b-%d", strip_layout_col)
      strip_layout_row <- tapply(layout$ROW, layout$COL, max)
      strip_pos_t <- panel_pos_rows$b[strip_layout_row] + 1L
      strip_height <- max_height(strips$x$bottom)

      # Pad strips to start at the edge of the panel
      on_last_row <- strip_layout_row == max(strip_layout_row)
      strips$x$bottom <- iapply(strips$x$bottom, !on_last_row, function(strip) {
        if (!is.gtable(strip)) strip else gtable_add_rows(strip, strip_height, 0L)
      })

      # Shift axes to start at the edge of the strip
      row <- strip_layout_row[!on_last_row]
      col <- strip_layout_col[!on_last_row]
      axis_name <- sprintf("axis-b-%d-%d", col, row)
      axes <- gtable_get_grob(panel_table, axis_name)
      axes <- lapply(axes, grob_shift_viewport, y = -strip_height)
      panel_table <- gtable_set_grob(panel_table, axis_name, axes)

      panel_table <- gtable_add_rows(panel_table, strip_height, max(strip_pos_t) - 1L)
      panel_table <- gtable_add_grob(panel_table, strips$x$bottom, strip_pos_t, strip_pos_l, clip = "off", name = strip_name, z = 2)
    } else {
      # Add strips to the top of the panels on the first row
      strip_name <- sprintf("strip-t-%d", strip_layout_col)
      strip_layout_row <- rep(1L, length(strip_layout_col))
      strip_pos_t <- panel_pos_rows$t[strip_layout_row]
      strip_height <- max_height(strips$x$top)

      panel_table <- gtable_add_rows(panel_table, strip_height, min(strip_pos_t) - 1L)
      panel_table <- gtable_add_grob(panel_table, strips$x$top, strip_pos_t, strip_pos_l, clip = "on", name = strip_name, z = 2)
    }

    panel_table
  }
)
