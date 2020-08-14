context("Annotate data")
library(esmData)

try({
  select_experiment(
    project = 'datequiz',
    function(x) dplyr::filter(x, study == 'datesStudy', version == 'v1-0-1')
  )
})

test_that("annotate_responses completes", {
  if (!('AdvisedTrial' %in% ls()))
    skip('select_experiment failed to load data')
  x <- annotate_responses(AdvisedTrial)
  expect_equal('tbl' %in% class(x), T)
})

x <- annotate_responses(AdvisedTrial)

test_that("annotate_responses adds columns", {
  if (!('x' %in% ls()))
    skip('annotate_responses failed')

  expect_gt(ncol(x), ncol(AdvisedTrial))
  expect_equal(nrow(x), nrow(AdvisedTrial))
})

test_that("annotate_responses includes correct and error columns", {
  if (!('x' %in% ls()))
    skip('annotate_responses failed')

  expect_equal('responseCorrect' %in% names(x), T)
  expect_equal('responseError' %in% names(x), T)
})

