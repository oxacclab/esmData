#' Add variables assessing the correctness, influence etc. of estimates
#' @param X tbl of initial estimates and final decisions made with
#'   advice
#' @return \code{X} with variables:
#'
#' \itemize{
#' \item{[reponse/advisor#]Correct[Final]}{whether the estimate was correct}
#' \item{[reponse/advisor#]Error[Final]}{absolute difference between the correct answer and the centre of the estimate}
#' }
#'
#' @export
annotate_responses <- function(X) {
  changed <- F
  # Detect which kind of data we have using some heuristics
  if ('responseEstimateLeft' %in% names(X)) {
    X <- annotate_responses.AdvisedTrial(X)
    changed <- T
  } else {
    if ('initialAnswer' %in% names(X)) {
      X <- annotate_responses.trials(X)
      changed <- T
    }
  }
  if (!changed)
    warning('Could not determine appropriate annotation function from names(X).')
  X
}

#' Add variables assessing the correctness, influence etc. of estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice in the dates task
#' @return \code{AdvisedTrial} with variables:
#'
#' \itemize{
#' \item{[reponse/advisor#]Correct[Final]}{whether the estimate was correct}
#' \item{[reponse/advisor#]Error[Final]}{absolute difference between the correct answer and the centre of the estimate}
#' \item{estimateIncrease}{Difference between final and initial estimate}
#' \item{influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{woaRaw}{Weight on advice = (final - initial) / (advice - initial)}
#' \item{woa}{Winzorized version of WoA Raw to range [0-1]}
#' }
annotate_responses.AdvisedTrial <- function(AdvisedTrial) {
  AdvisedTrial <- mark_responses(AdvisedTrial)
  AdvisedTrial <- rate_influence(AdvisedTrial)
  AdvisedTrial
}

#' Add variables indicating error, correctness, etc. for estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice
#' @return \code{AdvisedTrial} with correct and error columns for initial estimate, final decision, and any advisory estimates
#' @importFrom dplyr mutate %>% select matches rename_with left_join if_else
#' @importFrom stringr str_replace str_c str_extract str_ends str_to_sentence
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
mark_responses <- function(AdvisedTrial) {
  # Spread into individual estimates
  tmp <- AdvisedTrial %>%
    select(matches('^(pid|study|timestamp|response|advisor|correct)')) %>%
    mutate(
      responseEstimateCentre = .data$responseEstimateLeft + .data$responseMarkerWidth / 2,
      responseEstimateCentreFinal = .data$responseEstimateLeftFinal + .data$responseMarkerWidthFinal / 2
    ) %>%
    rename_with(
      ~ str_replace(., '(advisor[0-9]+)advice(Width)', '\\1Marker\\2'),
      .cols = matches('adviceWidth')
    ) %>%
    pivot_longer(matches('(responseEstimateCentre|advisor[0-9]+adviceCentre)')) %>%
    mutate(
      prefix = str_extract(.data$name, '(advisor[0-9]+|response)'),
      suffix = if_else(str_ends(.data$name, 'Final'), 'Final', '')
    )

  # Determine range of individual estimates
  tmp <- tmp %>%
    mutate(widthCol = match(str_c(.data$prefix, 'MarkerWidth', .data$suffix), names(tmp)))
  tmp$width <- sapply(1:nrow(tmp), function(i) tmp[i, tmp$widthCol[i]]) %>% unlist()
  tmp <- tmp %>%
    select(-.data$widthCol) %>%
    mutate(
      min = .data$value - .data$width / 2,
      max = .data$value + .data$width / 2
    )

  # Mark individual estimates
  tmp <- tmp %>%
    mutate(
      correctAnswer = as.numeric(.data$correctAnswer),
      correct = .data$min <= .data$correctAnswer & .data$max >= .data$correctAnswer,
      error = abs(.data$correctAnswer - .data$value)
    )

  # Restore wide format
  tmp <- tmp %>%
    select(-.data$min, -.data$max, -.data$value, -.data$name, -.data$width) %>%
    rename_with(str_to_sentence, .cols = c(.data$correct, .data$error)) %>%
    pivot_wider(id_cols = matches('^(pid|study|timestamp)'),
                names_from = c(.data$prefix, .data$suffix),
                names_glue = "{prefix}{.value}{suffix}",
                values_from = c(.data$Correct, .data$Error))

  # Join back onto the main tbl
  left_join(AdvisedTrial, tmp, by = names(tmp)[names(tmp) %in% names(AdvisedTrial)])
}

#' Add variables indicating error, correctness, etc. for estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice
#' @return \code{AdvisedTrial} with additional columns \code{advisor#...} where ... is:
#' \itemize{
#' \item{estimateIncrease}{Difference between final and initial estimate}
#' \item{influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{woaRaw}{Weight on advice = (final - initial) / (advice - initial)}
#' \item{woa}{Winzorized version of WoA Raw to range [0-1]}
#' }
#' @importFrom dplyr rename_with %>% starts_with mutate case_when select
#' @improtFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
rate_influence <- function(AdvisedTrial) {
  # Reshape to separate out advisors
  tmp <- AdvisedTrial %>%
    rename_with(~ str_replace(., 'advisor([0-9]+)', 'x\\1'), .cols = matches('advisor[0-9]+$')) %>%
    pivot_longer(
      starts_with('advisor'),
      names_to = c('advisor', '.value'),
      names_pattern = '(advisor[0-9]+)(.*)'
    )

  # Calculate summary values by advisor
  tmp <- tmp %>%
    mutate(
      .middle = .data$responseEstimateLeft + .data$responseMarkerWidth / 2,
      .middleFinal = .data$responseEstimateLeftFinal + .data$responseMarkerWidthFinal / 2,
      estimateIncrease = .data$.middleFinal - .data$.middle,
      influence = case_when(
        .data$.middle < .data$adviceCentre ~ .data$estimateIncrease,
        .data$.middle > .data$adviceCentre ~ -.data$estimateIncrease,
        T ~ NA_real_
      ),
      woaRaw = (.data$.middleFinal - .data$.middle) / (.data$adviceCentre - .data$.middle),
      woa = case_when(
        .data$woaRaw < 0 ~ 0,
        .data$woaRaw > 1 ~ 1,
        T ~ .data$woaRaw
      )
    ) %>%
    select(-starts_with('.'))

  # Restore original structure
  pivot_wider(
    tmp,
    names_from = .data$advisor,
    names_glue = "{advisor}{.value}",
    values_from = (which(names(tmp) == 'advisor') + 1):ncol(tmp)
  ) %>%
    rename_with(~ str_replace(., 'x([0-9]+)', 'advisor\\1'), .cols = matches('x[0-9]+$'))
}

#' Calculate influence for binary advised trials
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice
#' @details Adjusted weight on advice (woa) is capped by limiting the maximum
#'   change where answers are reduced to be equivalent to the maximum possible
#'   change if answers had been increased. This compensates for the fact that,
#'   once the initial estimate marks a point to one side of a scale, there is by
#'   definition a greater room to move towards the other side of the scale than
#'   there is to increase the rating on the chosen side.
#'
#'   The returned tbl has the following additional columns for each advisor
#'   (\code{AdvisedTrial$advisor#...}):
#'
#'   \itemize{
#'   \item{changedOfMind}{whether the chosen side is the same for the initial estimate and final decision}
#'   \item{increase}{how much confidence has increased from initial estimate to final decision}
#'   \item{agrees}{whether the advisor agrees with the initial decision}
#'   \item{woaRaw}{how much the}
#'   }
#' @return \code{AdvisedTrial} with columns appended containing influence
#'   calculations
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% bind_rows across summarise select
#' @export
get_influence_binary <- function(AdvisedTrial) {

}

#' Add variables assessing the correctness, influence etc. of estimates
#' @param trials tbl of initial estimates and final decisions made with
#'  advice in the dots task
#' @return \code{AdvisedTrial} with variables:
#'
#' \itemize{
#' \item{[reponse/advisor#]Correct[Final]}{whether the estimate was correct}
#' \item{estimateIncrease}{Difference between final and initial estimate}
#' \item{adviceInfluence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{adviceInfluenceCapped}{Difference between final and initial estimate signed in the direction of the advice capped to the maximum value which could have been given if confidence had increased and the answer side remained the same.}
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate %>% if_else case_when select starts_with
annotate_responses.trials <- function(trials) {
  trials %>%
    # mark responses
    mutate(
      initialAnswerCorrect = .data$initialAnswer == .data$correctAnswer,
      finalAnswerCorrect = .data$finalAnswer == .data$correctAnswer,
      adviceCorrect = .data$adviceSide == .data$correctAnswer
    ) %>%
    # Calculate influence and woa
    mutate(
      adviceInfluence = case_when(
        initialAnswer == .data$adviceSide & !.data$switch ~
          .data$finalConfidence - .data$initialConfidence,
        initialAnswer != .data$adviceSide & switch ~
          .data$finalConfidence + .data$initialConfidence,
        initialAnswer != .data$adviceSide & !.data$switch ~
          .data$initialConfidence - .data$finalConfidence,
        initialAnswer == .data$adviceSide & .data$switch ~
          -(.data$finalConfidence + .data$initialConfidence)
      ),
      .max = 50 - .data$initialConfidence,
      adviceInfluenceCapped = if_else(abs(.data$adviceInfluence) > .data$.max,
                                      .data$.max * sign(.data$adviceInfluence),
                                      .data$adviceInfluence)
    ) %>%
    select(-starts_with('.'))
}
