#' @include facet_ragged_rows.R
#' @rdname facet_ragged
#' @export
facet_ragged_cols <- function(rows, cols, ..., scales = "fixed", switch = NULL, labeller = "label_value") {
  rlang::check_dots_empty()

  scales <- rlang::arg_match0(scales, c("fixed", "free_x", "free_y", "free"))
  switch <- if (!is.null(switch)) rlang::arg_match0(switch, c("x", "y", "both")) else "none"

  ggproto(
    NULL,
    FacetRaggedCols,
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

FacetRaggedCols <- ggproto("FacetRaggedCols", FacetRagged,
  setup_params = function(data, params) {
    params <- FacetRagged$setup_params(data, params)

    # Add parameters expected by facet_wrap
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
      SCALE_X = if (params$free$x) cumsum(n)[j] else 1L,
      SCALE_Y = if (params$free$y) panel_id else 1L
      # facet_wrap assumes SCALE_X is an index into panels
    )

    cbind(layout, panels)
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    panel_table <- local({
      params$free$x <- FALSE # Always suppress intermediate axes in columns
      FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    })

    # Render strips for cols. facet_wrap doesn't know about these.
    strip_data_cols <- vctrs::vec_unique(layout[names(params$cols)])
    strips <- render_strips(strip_data_cols, NULL, params$labeller, theme)

    # Justify strips to start at the bottom edge of the panels
    strips$x$bottom <- lapply(strips$x$bottom, function(strip) {
      # Strips can be zeroGrobs if e.g. text is element_blank()
      if (!is.gtable(strip) || !params$switch$x) {
        return(strip)
      }
      gtable_add_rows(strip, gtable_height(strip), 0)
    })

    panel_pos_rows <- panel_rows(panel_table)
    panel_pos_cols <- panel_cols(panel_table)

    strip_layout_col <- seq_len(max(layout$COL))
    strip_pos_l <- panel_pos_cols$r[strip_layout_col]

    if (params$switch$x) {
      # Add strips to the bottom of the panels on the last row in each column
      strip_name <- sprintf("strip-b-%d", strip_layout_col)
      strip_layout_row <- tapply(layout$ROW, layout$COL, max)
      strip_pos_t <- panel_pos_rows$b[strip_layout_row] + 1L
      panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom) / 2)
      panel_table <- gtable_add_grob(panel_table, strips$x$bottom, strip_pos_t, strip_pos_l, clip = "off", name = strip_name, z = 2)
    } else {
      # Add strips to the top of the panels on the first row
      strip_name <- sprintf("strip-t-%d", strip_layout_col)
      strip_layout_row <- rep(1L, length(strip_layout_col))
      strip_pos_t <- panel_pos_rows$t[strip_layout_row]
      panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), min(strip_pos_t) - 1L)
      panel_table <- gtable_add_grob(panel_table, strips$x$top, strip_pos_t, strip_pos_l, clip = "on", name = strip_name, z = 2)
    }

    panel_table
  },
)
