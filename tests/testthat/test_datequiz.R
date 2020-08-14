context("DateQuiz data")
library(esmData)

test_that("select_experiments loads data", {
  expect_silent(select_experiment('datequiz'))
})

test_that("select_experiments filters correctly", {
  try(select_experiment('datequiz'))
  datequiz.full <- dotstask
  select_experiment('datequiz', f = function(x) dplyr::filter(x, study == 'accuracyDates'))
  expect_equal(nrow(datequiz) == nrow(datequiz.full), F)
})
