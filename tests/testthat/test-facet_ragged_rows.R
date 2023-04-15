tbl <- data.frame(foo = c("A", "B", "B"), bar = 1:3, x = 1:3, y = 3:1)
p <- ggplot(tbl, aes(x, y)) + facet_ragged_rows(vars(foo), vars(bar))

test_that("axes and strips don't overlap", {
  p <- p + scale_y_continuous(sec.axis = dup_axis())
  vdiffr::expect_doppelganger("y axes on both sides, default", p)
  p <- p + facet_ragged_rows(vars(foo), vars(bar), switch = "y")
  vdiffr::expect_doppelganger("y axes on both sides, switched", p)
})
