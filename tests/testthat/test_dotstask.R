context("DotsTask data")
library(esmData)

.tEnv2 <- new.env()

test_that("select_experiments loads data", {
  expect_silent(select_experiment('dotstask', envir = environment()))
})

test_that("select_experiments filters correctly", {
  select_experiment('dotstask', f = function(x) dplyr::filter(x, study == 'Accuracy'), envir = .tEnv2)
  expect_equal(nrow(dotstask) == nrow(.tEnv2$dotstask), F)
})
