#' @importFrom rlang %||%
grob_shift_viewport <- function(grob, x = NULL, y = NULL) {
  vp <- grob$vp %||% return(grob)
  if (!is.null(x)) {
    vp$x <- vp$x + x
  }
  if (!is.null(y)) {
    vp$y <- vp$y + y
  }
  grob$vp <- vp
  grob
}
