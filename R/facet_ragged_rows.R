#' @include facet_ragged.R
#' @rdname facet_ragged
#' @export
facet_ragged_rows <- function(rows, cols, ..., scales = "fixed", switch = NULL, labeller = "label_value") {
  rlang::check_dots_empty()

  scales <- rlang::arg_match0(scales, c("fixed", "free_x", "free_y", "free"))
  switch <- if (!is.null(switch)) rlang::arg_match0(switch, c("x", "y", "both")) else "none"

  ggproto(
    NULL,
    FacetRaggedRows,
    params = list(
      rows = rlang::quos_auto_name(rows),
      cols = rlang::quos_auto_name(cols),
      free = list(
        x = scales %in% c("free_x", "free"),
        y = scales %in% c("free_y", "free")
      ),
      switch = list(
        x = switch %in% c("x", "both"),
        y = switch %in% c("y", "both")
      ),
      labeller = labeller
    )
  )
}

FacetRaggedRows <- ggproto("FacetRaggedRows", FacetRagged,
  setup_params = function(data, params) {
    params <- FacetRagged$setup_params(data, params)

    # Add parameters expected by facet_wrap
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
    panel_id <- seq_len(nrow(panels))

    # Map variables to layout
    g <- panels[names(rows)]
    r <- vctrs::vec_group_rle(g)
    n <- vctrs::field(r, "length")
    i <- rep(seq_along(n), n)
    j <- sequence(n)

    layout <- data.frame(
      PANEL = panel_id,
      ROW = i,
      COL = j,
      SCALE_X = if (params$free$x) panel_id else 1L,
      SCALE_Y = if (params$free$y) cumsum(n)[i] else 1L
      # facet_wrap assumes SCALE_Y is an index into panels
    )

    cbind(layout, panels)
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    panel_table <- local({
      params$free$y <- FALSE # Always suppress intermediate axes on rows
      FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    })

    # Render strips for rows. facet_wrap doesn't know about these.
    strip_data_rows <- vctrs::vec_unique(layout[names(params$rows)])
    strips <- render_strips(NULL, strip_data_rows, params$labeller, theme)

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
