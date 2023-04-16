#' @include facet_ragged.R
#' @rdname facet_ragged
#' @export
facet_ragged_rows <- function(rows, cols, ..., scales = "fixed", switch = NULL, labeller = "label_value") {
  new_facet_ragged(FacetRaggedRows, rows, cols, ..., scales = scales, switch = switch, labeller = labeller)
}

FacetRaggedRows <- ggproto("FacetRaggedRows", FacetRagged,
  setup_params = function(data, params) {
    params <- FacetRagged$setup_params(data, params)

    # Add parameters expected by FacetWrap
    params$strip.position <- if (params$switch$x) "bottom" else "top"
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
    layout <- layout_ragged_rows(panels[names(rows)], params$free)

    cbind(layout, panels)
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    params$free$y <- FALSE # Always suppress intermediate axes on rows
    panel_table <- FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)

    # Render rows strips that FacetWrap didn't know about
    strip_data <- vctrs::vec_unique(layout[names(params$rows)])
    strips <- render_strips(NULL, strip_data, params$labeller, theme)

    panel_pos_rows <- panel_rows(panel_table)
    panel_pos_cols <- panel_cols(panel_table)

    strip_layout_row <- seq_len(max(layout$ROW))
    strip_pos_t <- panel_pos_rows$t[strip_layout_row]

    if (params$switch$y) {
      # Add strips to the left of the panels in the first column
      strip_name <- sprintf("strip-l-%d", strip_layout_row)
      strip_layout_col <- rep(1L, length(strip_layout_row))
      strip_pos_l <- panel_pos_cols$l[strip_layout_col]
      strip_width <- max_width(strips$y$left)

      panel_table <- gtable_add_cols(panel_table, strip_width, min(strip_pos_l) - 1L)
      panel_table <- gtable_add_grob(panel_table, strips$y$left, strip_pos_t, strip_pos_l, clip = "on", name = strip_name, z = 2)
    } else {
      # Add strips to the right of the panels in the last column on each row
      strip_name <- sprintf("strip-r-%d", strip_layout_row)
      strip_layout_col <- tapply(layout$COL, layout$ROW, max)
      strip_pos_l <- panel_pos_cols$r[strip_layout_col] + 1L
      strip_width <- max_width(strips$y$right)

      # Pad strips to start at the edge of the panel
      in_last_col <- strip_layout_col == max(strip_layout_col)
      strips$y$right <- iapply(strips$y$right, !in_last_col, function(strip) {
        if (!is.gtable(strip)) strip else gtable_add_cols(strip, strip_width, 0L)
      })

      # Shift axes to start at the edge of the strip
      row <- strip_layout_row[!in_last_col]
      col <- strip_layout_col[!in_last_col]
      axis_name <- sprintf("axis-r-%d-%d", row, col)
      axes <- gtable_get_grob(panel_table, axis_name)
      axes <- lapply(axes, grob_shift_viewport, x = strip_width)
      panel_table <- gtable_set_grob(panel_table, axis_name, axes)

      panel_table <- gtable_add_cols(panel_table, strip_width, max(strip_pos_l) - 1L)
      panel_table <- gtable_add_grob(panel_table, strips$y$right, strip_pos_t, strip_pos_l, clip = "off", name = strip_name, z = 2)
    }

    panel_table
  },
)
