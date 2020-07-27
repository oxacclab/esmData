context("DateQuiz data")
library(esmData)

test_that("select_experiments loads data", {
  select_experiment('datequiz')
  expect_equal('datequiz' %in% ls(), T)
})

test_that("select_experiments filters correctly", {
  try(select_experiment('datequiz'))
  if (!('datequiz' %in% ls()))
    skip('select_experiments failed')
  datequiz.full <- dotstask
  select_experiment('datequiz', f = function(x) dplyr::filter(x, study == 'accuracyDates'))
  expect_equal('datequiz' %in% ls(), T)
  expect_equal(nrow(datequiz) == nrow(datequiz.full), F)
})
