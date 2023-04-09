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
#' # Ragged grid with cohorts in separate columns
#' p + facet_ragged_cols(
#'   vars(Subject = as.character(Subject)),
#'   vars(Cohort = 1 + Subject %in% 3:6),
#'   labeller = label_both
#' )
#' @name facet_ragged
NULL
