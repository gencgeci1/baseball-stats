library(testthat)
library(baseball.stats)

test_check("baseball.stats")

test_that("batting average", {
  expect_equal(ba(50, 100), .5)
})


test_that("slugging percentage", {
  expect_equal(slg(5, 8, 2, 9, 50), 1.26)
})

