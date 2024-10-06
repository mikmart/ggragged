test_that("panels are positioned correctly", {
  ragged_rows <- layout_ragged_rows(c("A", "B", "B"))
  expect_equal(ragged_rows$ROW, c(1, 2, 2))
  expect_equal(ragged_rows$COL, c(1, 1, 2))

  ragged_cols <- layout_ragged_cols(c("A", "B", "B"))
  expect_equal(ragged_cols$ROW, c(1, 1, 2))
  expect_equal(ragged_cols$COL, c(1, 2, 2))
})

test_that("free scales work", {
  rows_free_x <- layout_ragged_rows(c("A", "B", "B"), free = list(x = TRUE))
  expect_equal(rows_free_x$SCALE_X, c(1, 2, 3))
  expect_equal(rows_free_x$SCALE_Y, c(1, 1, 1))

  rows_free_y <- layout_ragged_rows(c("A", "B", "B"), free = list(y = TRUE))
  expect_equal(rows_free_y$SCALE_X, c(1, 1, 1))
  expect_equal(rows_free_y$SCALE_Y, c(1, 3, 3))

  cols_free_x <- layout_ragged_cols(c("A", "B", "B"), free = list(x = TRUE))
  expect_equal(cols_free_x$SCALE_X, c(1, 3, 3))
  expect_equal(cols_free_x$SCALE_Y, c(1, 1, 1))

  cols_free_y <- layout_ragged_cols(c("A", "B", "B"), free = list(y = TRUE))
  expect_equal(cols_free_y$SCALE_X, c(1, 1, 1))
  expect_equal(cols_free_y$SCALE_Y, c(1, 2, 3))
})

test_that("panels can be aligned to the end", {
  expect_equal(layout_ragged_rows(c("A", "B", "B"), align = "start")$COL, c(1, 1, 2))
  expect_equal(layout_ragged_cols(c("A", "B", "B"), align = "start")$ROW, c(1, 1, 2))
  expect_equal(layout_ragged_rows(c("A", "B", "B"), align = "end")$COL, c(2, 1, 2))
  expect_equal(layout_ragged_cols(c("A", "B", "B"), align = "end")$ROW, c(2, 1, 2))
})
