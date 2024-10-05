#' Lay out panels in a ragged grid
#'
#' These facets create layouts in-between [ggplot2::facet_wrap()] and
#' [ggplot2::facet_grid()]. Panels are arranged into groups stacked along the
#' defining dimension, but remain independent in the other dimension, allowing
#' for a grid with ragged edges. This can be useful, for example, to represent
#' nested or partially crossed relationships between faceting variables.
#'
#' @param rows,cols A set of variables or expressions quoted by [ggplot2::vars()],
#'   the combinations of which define panels to be included in the grid.
#' @param ... Arguments reserved for future use.
#' @param scales Should all panels share the same scales (`"fixed"`),
#'   x-axes vary (`"free_x"`), y-axes vary (`"free_y"`), or both (`"free"`)?
#'   Panels within groups always share the scale along the grouping dimension.
#' @param switch By default, facet labels are positioned to the top and right
#'   of the panels. Use `"x"` to switch the top strip to the bottom,
#'   use `"y"` to switch the right strip to the left, or `"both"`.
#' @param strips Should strips be drawn only on the outer margins of groups
#'   (`"margins"`) or also between panels (`"all"`)?
#' @inheritParams ggplot2::facet_wrap
#'
#' @returns A `Facet` that can be added to a `ggplot`.
#'
#' @examples
#' p <- ggplot(Indometh, aes(time, conc)) + geom_line()
#'
#' # Panels for each subject, with cohorts on separate rows
#' p + facet_ragged_rows(
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   vars(Subject = as.character(Subject)),
#'   labeller = label_both
#' )
#'
#' # Independent y-axes between rows of cohorts
#' p + facet_ragged_rows(
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   vars(Subject = as.character(Subject)),
#'   labeller = label_both,
#'   scales = "free_y"
#' )
#'
#' # Panels for each subject, with cohorts in separate columns
#' p + facet_ragged_cols(
#'   vars(Subject = as.character(Subject)),
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   labeller = label_both
#' )
#'
#' # Independent y-axes for all subjects
#' p + facet_ragged_cols(
#'   vars(Subject = as.character(Subject)),
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   labeller = label_both,
#'   scales = "free_y"
#' )
#' @name facet_ragged
NULL

FacetRagged <- ggproto("FacetRagged", Facet,
  shrink = TRUE,

  setup_params = function(data, params) {
    params <- Facet$setup_params(data, params)
    params$rows <- rlang::quos_auto_name(params$rows)
    params$cols <- rlang::quos_auto_name(params$cols)
    params$free <- list(
      x = params$scales %in% c("free_x", "free"),
      y = params$scales %in% c("free_y", "free")
    )
    params$switch <- list(
      x = params$switch %in% c("x", "both"),
      y = params$switch %in% c("y", "both")
    )
    params$axes <- list(
      x = params$axes %in% c("all_x", "all"),
      y = params$axes %in% c("all_y", "all")
    )
    params$strip.position <- c(
      if (params$switch$x) "bottom" else "top",
      if (params$switch$y) "left" else "right"
    )
    params
  },

  map_data = function(data, layout, params) {
    FacetGrid$map_data(data, layout, params)
  },

  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  },

  attach_axes = function(table, layout, ranges, coord, theme, params) {
    axes <- render_axes(ranges, ranges, coord, theme)
    axes <- list(
      t = lapply(axes$x, `[[`, "top"),
      b = lapply(axes$x, `[[`, "bottom"),
      l = lapply(axes$y, `[[`, "left"),
      r = lapply(axes$y, `[[`, "right")
    )
    add_panel_decorations(table, layout, axes, kind = "axis")
  },

  attach_strips = function(table, layout, params, theme) {
    # Render strips with faceting variable data
    cols_data <- layout[names(params$cols)]
    rows_data <- layout[names(params$rows)]
    strips <- render_strips(cols_data, rows_data, params$labeller, theme)
    strips <- c(strips$x, strips$y)

    # Zero out strips which shouldn't be added
    for (side in c("top", "bottom", "left", "right"))
      if (!side %in% params$strip.position)
        strips[[side]][] <- list(zeroGrob())

    # Make strips stick correctly even in zero-sized rows/cols
    for (side in c("top", "bottom", "left", "right"))
      strips[[side]] <- lapply(strips[[side]], set_strip_viewport, side)

    add_panel_decorations(table, layout, strips, kind = "strip")
  }
)

add_panel_decorations <- function(table, layout, grobs, kind) {
  kind <- rlang::arg_match0(kind, c("axis", "strip"))

  # Add rows for horizontal decorations
  for (t in rev(panel_rows(table)$t)) {
    table <- gtable_add_rows(table, max_height(grobs$t), t - 1)
    table <- gtable_add_rows(table, max_height(grobs$b), t + 1)
  }

  # Add columns for vertical decorations
  for (l in rev(panel_cols(table)$l)) {
    table <- gtable_add_cols(table, max_width(grobs$l), l - 1)
    table <- gtable_add_cols(table, max_width(grobs$r), l + 1)
  }

  # Find panel positions after layout changes
  panel_rows_pos <- panel_rows(table)
  panel_cols_pos <- panel_cols(table)

  t <- panel_rows_pos$t[layout$ROW] - 1
  b <- panel_rows_pos$b[layout$ROW] + 1
  l <- panel_cols_pos$l[layout$COL] - 1
  r <- panel_cols_pos$r[layout$COL] + 1

  # Add decorations around panels
  for (i in seq_along(layout$PANEL)) {
    table <- gtable_add_grob(table, grobs$t[i], t[i], l[i] + 1, name = sprintf("%s-t-%d", kind, i))
    table <- gtable_add_grob(table, grobs$b[i], b[i], l[i] + 1, name = sprintf("%s-b-%d", kind, i))
    table <- gtable_add_grob(table, grobs$l[i], t[i] + 1, l[i], name = sprintf("%s-l-%d", kind, i))
    table <- gtable_add_grob(table, grobs$r[i], t[i] + 1, r[i], name = sprintf("%s-r-%d", kind, i))
  }

  table
}

set_strip_viewport <- function(strip, side) {
  strip$vp <- switch(
    substr(side, 1, 1),
    t = grid::viewport(height = grid::grobHeight(strip), y = unit(0, "npc"), just = "bottom"),
    b = grid::viewport(height = grid::grobHeight(strip), y = unit(1, "npc"), just = "top"),
    l = grid::viewport(width = grid::grobWidth(strip), x = unit(1, "npc"), just = "right"),
    r = grid::viewport(width = grid::grobWidth(strip), x = unit(0, "npc"), just = "left"),
    stop("internal error: invalid side: ", side)
  )
  strip
}

cull_inner_panel_decorations <- function(table, layout, sides, kind) {
  kind <- rlang::arg_match0(kind, c("axis", "strip"))
  for (side in sides) {
    panels <- panels_with_neighbour(layout, side)
    names <- sprintf("%s-%s-%d", kind, side, panels)
    table <- gtable_set_grob(table, names, list(zeroGrob()))
    table <- switch(
      side,
      t = ,
      b = gtable_set_height(table, names, unit(0, "cm")),
      l = ,
      r = gtable_set_width(table, names, unit(0, "cm")),
      stop("internal error: invalid side: ", side)
    )
    # TODO: Fix strip and axis overlapping when space was culled.
  }
  table
}

panels_with_neighbour <- function(layout, side) {
  neighbour <- switch(
    side,
    t = list(PANEL = layout$PANEL, ROW = layout$ROW - 1, COL = layout$COL),
    b = list(PANEL = layout$PANEL, ROW = layout$ROW + 1, COL = layout$COL),
    l = list(PANEL = layout$PANEL, ROW = layout$ROW, COL = layout$COL - 1),
    r = list(PANEL = layout$PANEL, ROW = layout$ROW, COL = layout$COL + 1),
    stop("internal error: invalid side: ", side)
  )
  merge(layout[c("ROW", "COL")], neighbour)$PANEL
}
