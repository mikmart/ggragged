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

  map_data = function(data, layout, params) {
    FacetGrid$map_data(data, layout, params)
  },

  vars = function(self) {
    names(c(self$params$rows, self$params$cols))
  },
)

new_facet_ragged <- function(parent, rows, cols, ..., scales, switch, labeller) {
  rlang::check_dots_empty()

  scales <- rlang::arg_match0(scales, c("fixed", "free_x", "free_y", "free"))
  switch <- if (!is.null(switch)) rlang::arg_match0(switch, c("x", "y", "both")) else "none"

  ggproto(
    NULL,
    parent,
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
