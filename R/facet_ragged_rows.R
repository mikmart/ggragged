#' @include facet_ragged.R
#' @rdname facet_ragged
#' @export
facet_ragged_rows <- function(rows, cols, ..., scales = "fixed", labeller = "label_value") {
  rlang::check_dots_empty()
  scales <- rlang::arg_match(scales, c("fixed", "free_x", "free_y", "free"))
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
      labeller = labeller
    )
  )
}

FacetRaggedRows <- ggproto(
  "FacetRaggedRows",
  FacetWrap,

  setup_params = function(data, params) {
    params <- FacetWrap$setup_params(data, params)

    # Add parameters expected by facet_wrap
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

  map_data = function(data, layout, params) {
    FacetGrid$map_data(data, layout, params)
  },

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    panel_table <- local({
      params$free$y <- FALSE # Always suppress intermediate axes on rows
      FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    })

    # Render strips for rows. facet_wrap doesn't know about these.
    strip_data_rows <- vctrs::vec_unique(layout[names(params$rows)])
    strips <- render_strips(NULL, strip_data_rows, params$labeller, theme)

    # For each row, find the farthest out column to add strips to
    strip_layout_row <- tapply(layout$ROW, layout$ROW, head, 1)
    strip_layout_col <- tapply(layout$COL, layout$ROW, max)
    strip_name <- sprintf("strip-r-%d", strip_layout_row)

    # Map strip position in layout to position in gtable
    panel_pos_rows <- panel_rows(panel_table)
    panel_pos_cols <- panel_cols(panel_table)
    strip_pos_t <- panel_pos_rows$t[strip_layout_row]
    strip_pos_l <- panel_pos_cols$r[strip_layout_col] + 1

    # Justify strips to start at the right edge of the panels
    strips$y$right <- lapply(strips$y$right, function(strip) {
      # Strips can be zeroGrobs if e.g. text is element_blank()
      if (!is.gtable(strip)) {
        return(strip)
      }
      gtable_add_cols(strip, gtable_width(strip), 0)
    })

    # Add space in the margin for farthest out strips, and then strip grobs
    panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right) / 2, max(strip_pos_l))
    panel_table <- gtable_add_grob(panel_table, strips$y$right, strip_pos_t, strip_pos_l, clip = "off", name = strip_name, z = 2)

    panel_table
  },

  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  },
)
