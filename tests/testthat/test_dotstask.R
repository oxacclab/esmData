context("DotsTask data")
library(esmData)

test_that("select_experiments loads data", {
  expect_silent(select_experiment('dotstask'))
})

test_that("select_experiments filters correctly", {
  try(select_experiment('dotstask'))
  dotstask.full <- dotstask
  select_experiment('dotstask', f = function(x) dplyr::filter(x, study == 'Accuracy'))
  expect_equal(nrow(dotstask) == nrow(dotstask.full), F)
})
