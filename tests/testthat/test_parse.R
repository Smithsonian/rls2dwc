library(RLS2DWC)

test_that("Load the test spreadsheet - is it the correct dimensions?", {
  expect_equal(ncol(testDF),47)
  expect_equal(nrow(testDF),209)
})
