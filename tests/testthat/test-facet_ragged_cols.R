tbl <- data.frame(foo = c("A", "B", "B"), bar = 1:3, x = 1:3, y = 3:1)

test_that("axes and strips don't overlap", {
  p <- ggplot(tbl, aes(x, y)) + scale_x_continuous(sec.axis = dup_axis())
  
  p <- p + facet_ragged_cols(vars(bar), vars(foo))
  vdiffr::expect_doppelganger("x axes on both sides, default", p)

  p <- p + facet_ragged_cols(vars(bar), vars(foo), switch = "x")
  vdiffr::expect_doppelganger("x axes on both sides, switched", p)
})
