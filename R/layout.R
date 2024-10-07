layout_ragged <- function(x, groups, align = "start") {
  groups <- rlang::arg_match0(groups, c("rows", "cols"))
  r <- vctrs::vec_group_rle(x)
  n <- vctrs::field(r, "length")

  PANEL <- vctrs::vec_seq_along(x)
  ROW <- rep.int(seq_along(n), n)
  COL <- sequence(n, from = if (align == "end") max(n) - n + 1L else 1L)

  layout <- vctrs::data_frame(PANEL = PANEL, ROW = ROW, COL = COL)
  if (groups == "cols")
    layout[c("ROW", "COL")] <- layout[c("COL", "ROW")]

  layout
}

layout_ragged_rows <- function(x, free = list(), align = "start") {
  layout <- layout_ragged(x, groups = "rows", align = align)
  layout$SCALE_X <- if (!isTRUE(free$x)) 1L else layout$PANEL
  layout$SCALE_Y <- if (!isTRUE(free$y)) 1L else layout$ROW
  layout
}

layout_ragged_cols <- function(x, free = list(), align = "start") {
  layout <- layout_ragged(x, groups = "cols", align = align)
  layout$SCALE_X <- if (!isTRUE(free$x)) 1L else layout$COL
  layout$SCALE_Y <- if (!isTRUE(free$y)) 1L else layout$PANEL
  layout
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

inner_margin_panels <- function(layout, side) {
  setdiff(margin_panels(layout, side), outermost_panels(layout, side))
}

margin_panels <- function(layout, side) {
  setdiff(layout$PANEL, panels_with_neighbour(layout, side))
}

outermost_panels <- function(layout, side) {
  switch(
    side,
    t = layout$PANEL[layout$ROW == min(layout$ROW)],
    b = layout$PANEL[layout$ROW == max(layout$ROW)],
    l = layout$PANEL[layout$COL == min(layout$COL)],
    r = layout$PANEL[layout$COL == max(layout$COL)],
    stop("internal error: invalid side: ", side)
  )
}
