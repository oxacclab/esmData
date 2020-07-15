# Load and prepare data ---------------------------------------------------

#' Fetch experiment JSON files from a folder
#' @param folderName path to folder to process
#' @return list of tables of data extracted from JSONs
loadFilesFromFolder <- function(folderName) {

  files <- list.files(folderName)
  participants <- NULL
  trials <- NULL
  advisors <- NULL
  questionnaires <- NULL
  genTrustQ <- NULL
  debrief <- NULL
  debriefRepQuiz <- NULL
  tblNames <- c(
    'advisors', 'questionnaires', 'trials', 'generalisedTrustQuestionnaire',
    'debrief', 'debriefRepQuiz'
    )
  parseLists <- function(x) {
    x <- unlist(x)
    if (!is.null(x) & length(x) > 1)
      t(x) %>% as_tibble()
    else
      x
  }
  for (i in seq(length(files))) {
    fileName <- paste(folderName, files[[i]], sep = '/')
    json <- readChar(fileName, file.info(fileName)$size)
    jsonData <- jsonlite::fromJSON(json, simplifyVector = T, simplifyMatrix = T, simplifyDataFrame = T)

    # store all columns in participants table except the special cases
    # (trials, advisors, and questionnaires (including GTQ) are stored separately)
    p <- tibble::as_tibble(t(jsonData[!names(jsonData) %in% tblNames]))
    p <- dplyr::mutate_all(p, parseLists)
    participants <- dplyr::bind_rows(participants, p)

    # store the trials in the trials table
    trials <- rbind(trials, jsonData$trials)
    advisors <- rbind(advisors, jsonData$advisors)
    questionnaires <- rbind(questionnaires, jsonData$questionnaires)
    if (('generalisedTrustQuestionnaire' %in% names(jsonData)))
      genTrustQ <- rbind(genTrustQ, jsonData$generalisedTrustQuestionnaire)
    debrief <- rbind(debrief, jsonData$debrief)
    debriefRepQuiz <- rbind(debriefRepQuiz, jsonData$debriefRepQuiz)
  }
  return(list(participants = participants,
              trials = trials,
              advisors = advisors,
              questionnaires = questionnaires,
              genTrustQ = genTrustQ,
              debrief = debrief,
              debriefRepQuiz = debriefRepQuiz))
}

# Clean data --------------------------------------------------------------

#' @param results from \link{loadFilesFromFolder}
#' @param replaceWithPID whether to add a pid field instead. If NULL, adds a pid
#'   field if it doesn't already exist.
#' @return results with each participantId field removed
removeParticipantIds <- function(results, replaceWithPID = NULL) {

  pids <- as.data.frame(unique(results[['participants']]$id))

  for (dfName in names(results)) {
    df <- results[[dfName]]
    idColName <- ifelse(dfName == 'participants', 'id', 'participantId')
    if (!(idColName %in% names(df)))
      next()
    addPID <- (replaceWithPID == T || (is.null(replaceWithPID) && !('pid' %in% names(df))))
    if (addPID) {
      pid <- sapply(df[ , idColName], function(x) which(pids == x))
      df <- cbind(pid, df)
    }
    df[ , idColName] <- NULL
    results[[dfName]] <- df
  }
  return(results)
}

#' Add some useful derived variables to a set of results
#' @param results results to augment
#' @return results with utility variables added
trialUtilityVariables <- function(results) {
  trialTypes <- list(catch = 0, force = 1, choice = 2, dual = 3, change = 4)

  # unpack results
  trials <- results$trials      # this explicit import suppresses build warnings
  advisors <- results$advisors  # this explicit import suppresses build warnings
  for (i in 1:length(results))
    assign(names(results)[i], results[i][[1]])

  out <- data.frame(trials$id)

  dualAdvisors <- !is.null(trials$advisor0id)

  # sometimes it helps to see confidence arranged from sure left to sure right (-100 to 100)
  out$initialConfSpan <- ifelse(trials$initialAnswer == 0,trials$initialConfidence*-1,trials$initialConfidence)
  out$finalConfSpan <- ifelse(trials$finalAnswer == 0,trials$finalConfidence*-1,trials$finalConfidence)

  # confidence changes
  out$confidenceShift <- getConfidenceShift(trials) #  amount the confidence changes
  out$confidenceShiftRaw <- getConfidenceShift(trials,T,T) # as above, without symmetry adjustment
  out$switch <- trials$initialAnswer != trials$finalAnswer # whether participant switched response

  # trial correctness
  out$initialCorrect <- trials$initialAnswer == trials$correctAnswer # whether the initial answer is correct
  out$finalCorrect <- trials$finalAnswer == trials$correctAnswer # whether the final answer is correct

  # advisor ids
  out$adviceType <- findAdviceType(trials$advisorId, trials$pid, advisors) # adviceType > trials table
  if (dualAdvisors) {
    out$advisor0type <- findAdviceType(trials$advisor0id, trials$pid, advisors)
    out$advisor1type <- findAdviceType(trials$advisor1id, trials$pid, advisors)
  } else {
    out$advisor0type <- NA
    out$advisor1type <- NA
  }

  # advisor group ids
  out$advisorGroup <- findAdvisorGroup(trials$advisorId, trials$pid, advisors)
  if (dualAdvisors) {
    out$advisor0group <- findAdvisorGroup(trials$advisor0id, trials$pid, advisors)
    out$advisor1group <- findAdvisorGroup(trials$advisor1id, trials$pid, advisors)
  } else {
    out$advisor0group <- NA
    out$advisor1group <- NA
  }

  # advisor influence
  # amount the confidence changes in the direction of the advice
  out$advisorInfluence <- NA
  out$advisor0influence <- NA
  out$advisor1influence <- NA

  # as above, without symmetry adjustment
  out$advisorInfluenceRaw <- NA
  out$advisor0influenceRaw <- NA
  out$advisor1influenceRaw <- NA

  for (tt in unique(trials$type)) {
    # Older data sometimes misses trial type assignment
    if (is.na(tt))
      next()

    m <- trials$type == tt & !is.na(trials$type)

    if (tt == trialTypes$force || tt == trialTypes$choice || tt == trialTypes$change) {
      out$advisorInfluence[m] <- findInfluence(trials$advisorAgrees,
                                               out$confidenceShift)[m]
      out$advisorInfluenceRaw[m] <- findInfluence(trials$advisorAgrees,
                                                  out$confidenceShiftRaw)[m]
    }
    if (tt == trialTypes$dual) {
      out$advisor0influence[m] <- findInfluence(trials$advisor0agrees,
                                                out$confidenceShift)[m]
      out$advisor0influenceRaw[m] <- findInfluence(trials$advisor0agrees,
                                                   out$confidenceShiftRaw)[m]
      out$advisor1influence[m] <- findInfluence(trials$advisor1agrees,
                                                out$confidenceShift)[m]
      out$advisor1influenceRaw[m] <- findInfluence(trials$advisor1agrees,
                                                   out$confidenceShiftRaw)[m]
    }
  }

  if (!is.null(trials$stimulusParent)) {
    # repetition stuff
    out$isRepeat <- !is.na(trials$stimulusParent)
    out$isRepeated <- F
    for (pid in unique(trials$pid)) {
      ts <- trials[trials$pid == pid, ]
      ts$isRepeated <- ts$id %in% ts$stimulusParent
      out$isRepeated[trials$pid == pid] <- ts$isRepeated
    }
  }

  return(out[ ,-1])
}

# Utility variable calculation functions ----------------------------------

#' Find the confidence shift in a given trial
#' @param t trial list
#' @param rawShift whether to report the confidence shift without adjusting for the assymetric scale
#' @param forceRecalculate if true, simply return the appropriate column from t if it exists already
#' @return a vector of confidence shifts for trial list t
getConfidenceShift <- function (t, rawShift = FALSE, forceRecalculate = FALSE) {
  scaleMaximum <- 50
  # shortcut if we already calculated this
  if('confidenceShift' %in% colnames(t) && !forceRecalculate)
    return(t$confidenceShift)
  out <- vector(length=dim(t)[1])
  for (i in seq(length(out))) {
    if (is.na(t$finalConfidence[i])) { # no advisor
      out[i] <- NA
    } else {
      max.shift <- scaleMaximum - t$initialConfidence[i]
      if(t$initialAnswer[i]==t$finalAnswer[i])
        out[i] <- t$finalConfidence[i]-t$initialConfidence[i] # same side
      else
        out[i] <- -1 * (t$finalConfidence[i]+t$initialConfidence[i]) # switched sliders, so went to 0 on the first one
      out[i] <- ifelse((abs(out[i]) > max.shift) & rawShift == F, max.shift*sign(out[i]), out[i])
    }
  }
  return(out)
}

#' Return a property of an Advisor from their ID for a given participant.
#' @param property to return. Should be a column in advisors
#' @param advisorId id of the advisor
#' @param participantId will be joined to advisorId as a data.frame, so must be of the same length
#' @param advisors data frame of advisors
#' @return property of specified advisor
.lookupAdvisorProperty <- function(property, advisorId, participantId, advisors) {
  df <- data.frame(advisorId, participantId, type = NA)
  if(any(!is.na(df$advisorId))) {
    tmp <- df[!is.na(df$advisorId), ]
    tmp$type <- sapply(1:nrow(tmp),
                       function(i) advisors[advisors$pid == tmp$participantId[i]
                                            & advisors$id == tmp$advisorId[i], property])
    for(i in 1:nrow(tmp))
      if(length(unlist(tmp$type[i])) == 0)
        tmp$type[i] <- list(NA)
    df[!is.na(df$advisorId), ] <- tmp
  }

  return(unlist(df$type))
}

#' Return an Advisor's adviceType from their ID for a given participant. Updated version of getAdviceType
#' @param advisorId id of the advisor
#' @param participantId will be joined to advisorId as a data.frame, so must be of the same length
#' @param advisors data frame of advisors
#' @return adviceType of specified advisor
findAdviceType <- function(advisorId, participantId, advisors) {
  return(.lookupAdvisorProperty('adviceType', advisorId, participantId, advisors))
}

#' Return an Advisor's groupId from their ID for a given participant. Updated version of getAdviceType
#' @param advisorId id of the advisor
#' @param participantId will be joined to advisorId as a data.frame, so must be of the same length
#' @param advisors data frame of advisors
#' @return adviceType of specified advisor
findAdvisorGroup <- function(advisorId, participantId, advisors) {
  return(.lookupAdvisorProperty('groupId', advisorId, participantId, advisors))
}

#' Return a vector of influences of advisors. Influence is +confidenceShift
#' where the advisor agrees, and -confidenceShift where the advisor disagrees.
#' @param advisorAgreements logical vector of whether the advisor agreed with the participant
#' @param confidenceShift numerical vector of the extent to which confidence changed in the final decision
#' @details The parameters are all bound together into a dataframe so must all be the same length
#' @return vector of influences of the advisors. NA where advisorId is NA
findInfluence <- function(advisorAgreements, confidenceShift) {
  out <- NA
  out[advisorAgreements == T
      & !is.na(advisorAgreements)] <- confidenceShift[advisorAgreements == T
                                                      & !is.na(advisorAgreements)]
  out[advisorAgreements == F
      & !is.na(advisorAgreements)] <- -1 * confidenceShift[advisorAgreements == F
                                                           & !is.na(advisorAgreements)]
  return(out)
}
