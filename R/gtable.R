gtable_get_grob <- function(x, name) {
  x$grobs[match(name, x$layout$name)]
}

gtable_set_grob <- function(x, name, grob) {
  x$grobs[match(name, x$layout$name)] <- grob
  x
}

gtable_set_height <- function(x, name, height) {
  t <- x$layout$t[match(name, x$layout$name)]
  x$heights[t] <- height
  x
}

gtable_set_width <- function(x, name, width) {
  l <- x$layout$l[match(name, x$layout$name)]
  x$widths[l] <- width
  x
}
