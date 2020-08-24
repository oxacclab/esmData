#' Add variables assessing the correctness, influence etc. of estimates
#' @param X tbl of initial estimates and final decisions made with
#'   advice
#'
#' @return \code{X} with variables (*=binary only, ^=continuous only):
#'
#' \itemize{
#' \item{[reponse/advisor#]Correct[Final]}{whether the estimate was correct}
#' \item{[reponse/advisor#]Error[Final]^}{absolute difference between the correct answer and the centre of the estimate}
#' \item{EstimateIncrease}{Difference between final and initial estimate}
#' \item{Influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{InfluenceCapped*}{Influence capped by the maximum potential increase}
#' \item{WOAraw^}{Weight on advice = (final - initial) / (advice - initial)}
#' \item{WOA^}{Winzorized version of WoA Raw to range [0-1]}
#' }
#'
#' @export
annotate_responses <- function(X) {
  changed <- F
  # Detect which kind of data we have using some heuristics
  if ('responseEstimateLeft' %in% names(X)) {
    X <- annotate_responses.AdvisedTrial(X)
    changed <- T
  }
  if ('responseAnswerSide' %in% names(X)) {
    X <- annotate_responses.AdvisedTrial.binary(X)
    changed <- T
  }
  if ('initialAnswer' %in% names(X)) {
      X <- annotate_responses.trials(X)
      changed <- T
  }

  if (!changed)
    warning('Could not determine appropriate annotation function by inspecting names().')
  X
}

#' Add variables assessing the correctness, influence etc. of estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice in the dates task
#'
#' @return \code{AdvisedTrial} with variables:
#'
#' \itemize{
#' \item{[reponse/advisor#]Correct[Final]}{whether the chosen side was correct}
#' \item{[reponse/advisor#]Error[Final]}{absolute difference between the correct answer and the centre of the chosen side}
#' \item{EstimateIncrease}{Difference between final and initial estimate}
#' \item{Influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{WOAraw}{Weight on advice = (final - initial) / (advice - initial)}
#' \item{WOA}{Winzorized version of WoA Raw to range [0-1]}
#' }
annotate_responses.AdvisedTrial <- function(AdvisedTrial) {
  AdvisedTrial <- mark_responses(AdvisedTrial)
  AdvisedTrial <- rate_influence(AdvisedTrial)
  AdvisedTrial
}

#' Add variables assessing the correctness, influence etc. of estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice in the dates task
#'
#' @return \code{AdvisedTrial} with variables:
#'
#' \itemize{
#' \item{[reponse/advisor#]Correct[Final]}{whether the chosen side was correct}
#' \item{EstimateIncrease}{Difference between final and initial estimate}
#' \item{Influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{InfluenceCapped}{Influence capped by the maximum potential increase}
#' }
annotate_responses.AdvisedTrial.binary <- function(AdvisedTrial) {
  AdvisedTrial <- mark_responses.binary(AdvisedTrial)
  AdvisedTrial <- rate_influence.binary(AdvisedTrial)
  AdvisedTrial
}

#' Add variables indicating error, correctness, etc. for estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice
#'
#' @return \code{AdvisedTrial} with correct and error columns for initial estimate, final decision, and any advisory estimates
#' @importFrom dplyr mutate %>% select matches rename_with left_join if_else
#' @importFrom stringr str_replace str_c str_extract str_ends str_to_sentence
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data has_name
mark_responses <- function(AdvisedTrial) {
  if (!has_name(AdvisedTrial, 'responseEstimateLeft')) {return(AdvisedTrial)}
  if (all(is.na(AdvisedTrial$responseEstimateLeft))) {return(AdvisedTrial)}

  # Spread into individual estimates
  tmp <- AdvisedTrial %>%
    # Add in responseEstimateCentre calculations
    mutate(
      responseEstimateCentre = .data$responseEstimateLeft + .data$responseMarkerWidth / 2,
      responseEstimateCentreFinal = .data$responseEstimateLeftFinal + .data$responseMarkerWidthFinal / 2
    ) %>%
    select(matches('^(pid|study|timestamp|response|advisor|correct)')) %>%
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
#'
#' @return \code{AdvisedTrial} with correct columns for initial estimate, final decision, and any advisory estimates
#' @importFrom dplyr mutate %>% select matches rename_with left_join if_else
#' @importFrom stringr str_replace str_c str_extract str_ends str_to_sentence
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data has_name
mark_responses.binary <- function(AdvisedTrial) {
  if (!has_name(AdvisedTrial, 'responseAnswerSide')) {return(AdvisedTrial)}
  if (all(is.na(AdvisedTrial$responseAnswerSide))) {return(AdvisedTrial)}

  # Spread into individual estimates
  tmp <- AdvisedTrial %>%
    select(matches('^(pid|study|timestamp|response|advisor|correct)')) %>%
    rename_with(
      ~ str_replace(., '(advisor[0-9]+)advice(Width)', '\\1Marker\\2'),
      .cols = matches('adviceWidth')
    ) %>%
    pivot_longer(matches('(responseAnswerSide|advisor[0-9]+adviceSide)')) %>%
    mutate(
      prefix = str_extract(.data$name, '(advisor[0-9]+|response)(Answer|advice)'),
      suffix = if_else(str_ends(.data$name, 'Final'), 'Final', '')
    )

  # Mark individual estimates
  tmp <- tmp %>% mutate(correct = .data$value == .data$correctAnswerSide)

  # Restore wide format
  tmp <- tmp %>%
    select(-.data$value, -.data$name) %>%
    rename_with(str_to_sentence, .cols = .data$correct) %>%
    pivot_wider(id_cols = matches('^(pid|study|timestamp)'),
                names_from = c(.data$prefix, .data$suffix),
                names_glue = "{prefix}SideCorrect{suffix}",
                values_from = .data$Correct)

  # Join back onto the main tbl
  left_join(AdvisedTrial, tmp, by = names(tmp)[names(tmp) %in% names(AdvisedTrial)])
}

#' Add variables indicating error, correctness, etc. for estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice
#' @return \code{AdvisedTrial} with additional columns \code{advisor#...} where ... is:
#' \itemize{
#' \item{EstimateIncrease}{Difference between final and initial estimate}
#' \item{Influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{WOAraw}{Weight on advice = (final - initial) / (advice - initial)}
#' \item{WOA}{Winzorized version of WoA Raw to range [0-1]}
#' }
#' @importFrom dplyr rename_with %>% starts_with mutate case_when select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
rate_influence <- function(AdvisedTrial) {
  if (!has_name(AdvisedTrial, 'responseEstimateLeft')) {return(AdvisedTrial)}
  if (all(is.na(AdvisedTrial$responseEstimateLeft))) {return(AdvisedTrial)}

  # Reshape to separate out advisors
  tmp <- AdvisedTrial %>%
    rename_with(~ str_replace(., 'advisor([0-9]+)', 'x\\1'), .cols = matches('advisor[0-9]+$')) %>%
    pivot_longer(
      matches('^advisor[0-9]+'),
      names_to = c('advisor', '.value'),
      names_pattern = '(advisor[0-9]+)(.*)'
    )

  # Calculate summary values by advisor
  tmp <- tmp %>%
    mutate(
      .middle = .data$responseEstimateLeft + .data$responseMarkerWidth / 2,
      .middleFinal = .data$responseEstimateLeftFinal + .data$responseMarkerWidthFinal / 2,
      EstimateIncrease = .data$.middleFinal - .data$.middle,
      Influence = case_when(
        .data$.middle < .data$adviceCentre ~ .data$EstimateIncrease,
        .data$.middle > .data$adviceCentre ~ -.data$EstimateIncrease,
        T ~ NA_real_
      ),
      WOAraw = (.data$.middleFinal - .data$.middle) / (.data$adviceCentre - .data$.middle),
      WOA = case_when(
        .data$WOAraw < 0 ~ 0,
        .data$WOAraw > 1 ~ 1,
        T ~ .data$WOAraw
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

#' Add variables indicating error, correctness, etc. for estimates
#' @param AdvisedTrial tbl of initial estimates and final decisions made with
#'   advice
#' @return \code{AdvisedTrial} with additional columns \code{advisor#...} where ... is:
#' \itemize{
#' \item{EstimateIncrease}{Difference between final and initial estimate}
#' \item{Influence}{Difference between final and initial estimate signed in the direction of the advice}
#' \item{InfluenceCapped}{Influence capped by the maximum potential increase}
#' }
#' @importFrom dplyr rename_with %>% starts_with mutate case_when select matches
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
rate_influence.binary <- function(AdvisedTrial) {
  if (!has_name(AdvisedTrial, 'responseAnswerSide')) {return(AdvisedTrial)}
  if (all(is.na(AdvisedTrial$responseAnswerSide))) {return(AdvisedTrial)}

  # Reshape to separate out advisors
  tmp <- AdvisedTrial %>%
    rename_with(~ str_replace(., 'advisor([0-9]+)', 'x\\1'), .cols = matches('advisor[0-9]+$')) %>%
    pivot_longer(
      matches('^advisor[0-9]+'),
      names_to = c('advisor', '.value'),
      names_pattern = '(advisor[0-9]+)(.*)'
    )

  # Calculate summary values by advisor
  tmp <- tmp %>%
    mutate(
      .max = 100 - .data$responseConfidence,
      .switch = .data$responseAnswerSide != .data$responseAnswerSideFinal,
      .agree = .data$responseAnswerSide == .data$adviceSide,
      EstimateIncrease = if_else(
        .data$.switch,
        -(.data$responseConfidenceFinal + .data$responseConfidence),
        .data$responseConfidenceFinal - .data$responseConfidence
      ),
      Influence = case_when(
        .data$.agree ~ .data$EstimateIncrease,
        !.data$.agree ~ -.data$EstimateIncrease,
        T ~ NA_real_
      ),
      InfluenceCapped = if_else(abs(.data$Influence) > .data$.max,
                                .data$.max * sign(.data$Influence),
                                .data$Influence)
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
    # Calculate influence
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
