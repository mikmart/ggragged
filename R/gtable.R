gtable_get_grob <- function(x, name) {
  x$grobs[[match(name, x$layout$name)]]
}

gtable_set_grobs <- function(x, name, grob) {
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

gtable_get_grob_position <- function(x, name) {
  x$layout[match(name, x$layout$name), c("t", "b", "l", "r")]
}
