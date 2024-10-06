layout_ragged_rows <- function(x, free = list(), align = "start", transpose = FALSE) {
    r <- vctrs::vec_group_rle(x)
    n <- vctrs::field(r, "length")
    i <- rep.int(seq_along(n), n)
    j <- sequence(n, from = if (align == "end") max(n) - n + 1L else 1L)

    if (transpose) {
      layout <- list(PANEL = vctrs::vec_seq_along(x), ROW = j, COL = i)
    } else {
      layout <- list(PANEL = vctrs::vec_seq_along(x), ROW = i, COL = j)
    }

    layout <- vctrs::new_data_frame(layout, vctrs::vec_size(x))

    # FacetWrap expects SCALE to be an index into panels, so we can't just use ROW/COL directly
    layout$SCALE_X <- if (!isTRUE(free$x)) 1L else if (transpose) cumsum(n)[layout$COL] else layout$PANEL
    layout$SCALE_Y <- if (!isTRUE(free$y)) 1L else if (transpose) layout$PANEL else cumsum(n)[layout$ROW]

    layout
}

layout_ragged_cols <- function(x, free = list(), align = "start", transpose = FALSE) {
  layout_ragged_rows(x, free = free, align = align, transpose = !transpose)
}
