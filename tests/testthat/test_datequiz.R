context("DateQuiz data")
library(esmData)

test_that("select_experiments loads data", {
  expect_silent(select_experiment('datequiz', envir = environment()))
})

test_that("select_experiments filters correctly", {
  .tmpEnv2 <- new.env()
  select_experiment('datequiz', f = function(x) dplyr::filter(x, study == 'accuracyDates'), envir = .tmpEnv2)
  expect_equal(nrow(datequiz) == nrow(.tmpEnv2$datequiz), F)
})
