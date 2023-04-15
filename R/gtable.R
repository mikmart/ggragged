gtable_get_grob <- function(x, name) {
  x$grobs[match(name, x$layout$name)]
}

gtable_set_grob <- function(x, name, grob) {
  x$grobs[match(name, x$layout$name)] <- grob
  x
}
