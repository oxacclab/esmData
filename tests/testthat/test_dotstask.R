context("DotsTask data")
library(esmData)

test_that("select_experiments loads data", {
  select_experiment('dotstask')
  expect_equal('dotstask' %in% ls(), T)
})

test_that("select_experiments filters correctly", {
  try(select_experiment('dotstask'))
  if (!('dotstask' %in% ls()))
    skip('select_experiments failed')
  dotstask.full <- dotstask
  select_experiment('dotstask', f = function(x) dplyr::filter(x, study == 'Accuracy'))
  expect_equal('dotstask' %in% ls(), T)
  expect_equal(nrow(dotstask) == nrow(dotstask.full), F)
})
