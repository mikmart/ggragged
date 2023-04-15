tbl <- data.frame(foo = c("A", "B", "B"), bar = 1:3, x = 1:3, y = 3:1)

test_that("axes and strips don't overlap", {
  p <- ggplot(tbl, aes(x, y)) + scale_y_continuous(sec.axis = dup_axis())
  
  p <- p + facet_ragged_rows(vars(foo), vars(bar))
  vdiffr::expect_doppelganger("y axes on both sides, default", p)
  
  p <- p + facet_ragged_rows(vars(foo), vars(bar), switch = "y")
  vdiffr::expect_doppelganger("y axes on both sides, switched", p)
})
