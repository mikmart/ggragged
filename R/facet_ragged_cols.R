#' @include facet_ragged_rows.R
#' @rdname facet_ragged
#' @export
facet_ragged_cols <- function(rows, cols, ..., scales = "fixed", switch = "none", strips = "margins", axes = "margins", labeller = "label_value") {
  switch <- switch %||% "none" # Compatibility with old default value NULL

  scales <- rlang::arg_match0(scales, c("fixed", "free_x", "free_y", "free"))
  switch <- rlang::arg_match0(switch, c("none", "x", "y", "both"))
  strips <- rlang::arg_match0(strips, c("margins", "all"))
  axes <- rlang::arg_match0(axes, c("margins", "all_x", "all_y", "all"))

  ggproto(
    NULL,
    FacetRaggedCols,
    params = list(
      rows = rows,
      cols = cols,
      scales = scales,
      switch = switch,
      strips = strips,
      axes = axes,
      labeller = labeller
    )
  )
}

FacetRaggedCols <- ggproto("FacetRaggedCols", FacetRagged,
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

  attach_axes = function(table, layout, ranges, coord, theme, params) {
    table <- FacetRagged$attach_axes(table, layout, ranges, coord, theme, params)

    if (!params$axes$x)
      table <- cull_inner_panel_decorations(table, layout, sides = c("t", "b"), kind = "axis")

    if (!params$axes$y && !params$free$y)
      table <- cull_inner_panel_decorations(table, layout, sides = c("l", "r"), kind = "axis")

    table
  },

  attach_strips = function(table, layout, theme, params) {
    table <- FacetRagged$attach_strips(table, layout, theme, params)

    if (params$strips == "margins")
      table <- cull_inner_panel_decorations(table, layout, sides = c("t", "b"), kind = "strip")

    table
  }
)
