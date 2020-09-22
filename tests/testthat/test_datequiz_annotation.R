context("Annotate data")
library(esmData)
library(dplyr)
library(rlang)

expect_silent(select_experiment('datequiz', envir = environment()))
at <-  filter(AdvisedTrial,
              .data$pid != '' & .data$pid != '0.1.3' & .data$pid != '1' &
                !(.data$studyId == 'markerUse' & .data$studyVersion == '0.2.1') &
                !(.data$studyId == 'minGroups' & .data$studyVersion == '1.2.0'))
x <- annotate_responses(at)

at %>%
  group_by(timestampStart, pid, studyId, studyVersion) %>%
  summarise(n = n()) %>% filter(n > 1) %>%
  mutate(time = timestampStart - 1.558786e12)

test_that("annotate_responses completes", {
  expect_equal('tbl' %in% class(x), T)
})

test_that("annotate_responses adds columns", {
  expect_gt(ncol(x), ncol(at))
  expect_equal(nrow(x), nrow(at))

})

test_that("annotate_responses includes correct and error columns", {
  expect_equal('responseCorrect' %in% names(x), T)
  expect_equal('responseAnswerSideCorrect' %in% names(x), T)
  expect_equal('responseError' %in% names(x), T)
})

test_that("annotate_responses includes influence and WOA columns", {
  expect_equal('advisor0Influence' %in% names(x), T)
  expect_equal('advisor0WOA' %in% names(x), T)
})
