library(curl)
library(jsonlite)
library(utils)
library(rlang)
library(tibble)
library(stringr)
library(dplyr)
library(parallel)
library(sjlabelled)
library(rlang)

# Fetch data from the acclab.psy.ox.ac.uk server

# Fetch server .csv files -------------------------------------------------

#' Fetch the data for a particular file from the server
#' @param fileDetails tbl row describing the file with $url, $study, $version, $table
#' @importFrom curl curl
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>% mutate_if filter bind_rows bind_cols select arrange
#' @importFrom stringr str_extract str_detect
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @return tibble of parsed data
.fetchData <- function(fileDetails) {
  library(curl)
  library(tibble)
  library(stringr)
  library(dplyr)
  library(utils)
  library(rlang)
  if (!all(c('url', 'table') %in% names(fileDetails)))
    stop('fileDetails variable missing required field. (Required fields = url, table.)')

  if (class('tbl') %in% class(fileDetails['url']))
    csv <- fileDetails['url'] %>% read.csv() %>% as_tibble()
  else
    csv <- fileDetails %>% pull('url') %>% read.csv() %>% as_tibble()

  if (str_detect(fileDetails['table'], 'Trial')) {
    if (!all(c('study', 'version') %in% names(fileDetails)))
      stop('fileDetails variable missing required field for raw data patching. (Required fields = url, table, study, version.)')
    # Patch resulting tibble with raw json data
    # Find necessary json files
    rDir <- "https://acclab.psy.ox.ac.uk/~mj221/ESM/data/public/raw/"
    jsons <- NULL
    con <- curl(rDir)
    open(con, "rb")
    while (isIncomplete(con)) {
      buffer <- readLines(con, n = 1)
      if (length(buffer)) {
        f <- str_extract(buffer, paste0('([0-9-_]+_', fileDetails['study'], '_', fileDetails['version'], '_[0-9a-z]{8}.json)'))
        if (!is.na(f) & nchar(f))
          jsons <- c(jsons, paste0(rDir, f))
      }
    }
    close(con)

    # Fetch json files and extract matching Trial info
    d <- NULL
    attn <- unique(csv$isAttentionCheck)

    for (j in jsons) {
      con <- curl(j)
      open(con, "rb")
      txt <- readLines(con)
      json <- jsonlite::fromJSON(txt)
      tmp <- json$trials %>% as_tibble()
      if ('blockType' %in% names(tmp))
        tmp <- tmp %>% filter(.data$blockType == "core")
      tmp <- tmp$data %>%
        as_tibble() %>%
        filter(.data$isAttentionCheck %in% attn) %>%
        mutate_if(is.factor, as.character)
      tmp$pid <- json$id
      d <- bind_rows(d, tmp %>% select(names(tmp)[order(names(tmp))]))
      close(con)
    }

    # Save name order for restoring later
    name_order <- names(csv)
    overwrite <- names(csv)[names(csv) %in% names(d)]

    if (!is.null(overwrite) && length(overwrite) > 0) {
      if (nrow(d) != nrow(csv)) {
        warning('Mismatched row counts for raw (JSON) and processed (CSV) data - skipping raw data overwrite.')
        return(csv)
      }

      csv <- csv %>% arrange(timestampStart, pid)
      d <- d %>% arrange(timestampStart, pid)

      csv <- csv[, !(names(csv) %in% overwrite)]
      csv <- bind_cols(csv, d)
      csv <- csv %>% select(name_order)
    }
  }

  csv
}

# List server files -------------------------------------------------------

#' Fetch raw data from the project server
#' @param cores number of cores to run the data extraction on in parallel
#' @importFrom curl curl
#' @importFrom tibble as_tibble
#' @importFrom purrr set_names
#' @importFrom dplyr %>%
#' @importFrom stringr str_match str_extract
#' @importFrom parallel makeCluster detectCores clusterExport parApply stopCluster
fetchRawData <- function(cores = parallel::detectCores() - 4) {

  rDir <- "https://acclab.psy.ox.ac.uk/~mj221/ESM/data/public/"

  out <- NULL

  con <- curl(rDir)
  open(con, "rb")
  while (isIncomplete(con)) {
    buffer <- readLines(con, n = 1)
    if (length(buffer)) {
      f <- str_extract(buffer, paste0('([\\w-]*_v[0-9\\-]+_[^"]+.csv)'))

      if (!is.na(f) & nchar(f)) {
        v <- str_extract(f, paste0('_v([0-9\\-]+)_[^"]+.csv'))

        out <- c(out, paste0(rDir, f))
      }
    }
  }
  close(con)

  files <- str_match(out, '^.+/([^_]+)_(v[0-9]+-[0-9]+-[0-9]+)_(.+)\\.csv$') %>%
    as_tibble() %>%
    set_names(c('url', 'study', 'version', 'table'))


  x <- files %>% filter(str_detect(table, 'Trial'))
  y <- files %>% filter(!str_detect(table, 'Trial'))

  if (cores > 1) {
    cl <- makeCluster(cores)
    clusterExport(cl, '.fetchData')
    x$data <- parApply(cl, x, 1, .fetchData)
    y$data <- parApply(cl, y, 1, .fetchData)
    stopCluster(cl)
  } else {
    x$data <- apply(x, 1, .fetchData)
    y$data <- apply(y, 1, .fetchData)
  }

    files <- rbind(x, y)

  files
}

#' Export basic data
#' @param files data object to export
#' @param name name the object should appear as when loaded
exportRawData <- function(files, name = 'datequiz.raw') {
  path <- paste0('extdata/', name, '.rda')
  assign(name, files)
  save(list = c(name), file = path)
}

#' Export data complete with tags and labels
#' @param files data object to export
#' @param name name the object should appear as when loaded
exportFullData <- function(files, name = 'datequiz') {
  path <- paste0('data/', name, '.rda')
  print(paste0('Export data to ', path, '.'))
  print('1. Tag data')
  files <- tagData(files)
  print('2. Add labels')
  files <- addLabels(files)
  print('3. Save.')
  assign(name, files)
  save(list = c(name), file = path)
}

#' Add information about studies to the raw data table
#' @param files raw data table
#' @importFrom dplyr %>% case_when mutate
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#' @return files with metadata added
tagData <- function(files) {
  files %>%
    mutate(
      N = map_dbl(.data$data, ~ length(unique(.$pid))),
      description = case_when(
        study == "accuracyDates"  & version == "v0-0-1" ~
          "Binary dates task with high and low accuracy advisors with agreement rates targeted to be roughly similar. This version contained a bug in which feedback was always disabled regardless of experimental condition, and were thus unsuitable for testing the key hypotheses for which the data were collected.",
        study == "accuracyDates"  & version == "v0-0-2"  ~
          "Binary dates task with high and low accuracy advisors with agreement rates targeted to be roughly similar. This version contained a bug in which feedback was always disabled regardless of experimental condition, and were thus unsuitable for testing the key hypotheses for which the data were collected.",
        study == "accuracyDates"  & version == "v0-0-3" ~
          "Binary dates task with high and low accuracy advisors with agreement rates targeted to be roughly similar. Constitutes a complete experiment when combined with version 0.0.4.",
        study == "accuracyDates"  & version == "v0-0-4" ~
          "Binary dates task with high and low accuracy advisors with agreement rates targeted to be roughly similar. Constitutes a complete experiment when combined with version 0.0.3.",
        study == "advisorChoice"  & version == "v0-0-5"  ~
          "Continuous dates task with agreeing versus accurate advisors and a primary outcome of advisor choice in a test block. This dataset did not save advisor choice options, making it difficult to systematically identify trials on which a choice was offered.",
        study == "advisorChoice"  & version == "v0-0-6" ~
          "Continuous dates task with agreeing versus accurate advisors and a primary outcome of advisor choice in a test block.",
        study == "advisorChoice"  & version == "v0-0-7" ~
          "Continuous dates task with agreeing versus accurate advisors and a primary outcome of advisor choice in a test block.",
        study == "agreementDates" & version == "v0-0-1" ~
          "Binary dates task with high and low agreement advisors with accuracy rates targeted to be roughly similar. This version contained a bug in which feedback was always disabled regardless of experimental condition, and were thus unsuitable for testing the key hypotheses for which the data were collected.",
        study == "agreementDates" & version == "v0-0-2" ~
          "Binary dates task with high and low agreement advisors with accuracy rates targeted to be roughly similar. This version contained a bug in which feedback was always disabled regardless of experimental condition, and were thus unsuitable for testing the key hypotheses for which the data were collected.",
        study == "agreementDates" & version == "v0-0-3" ~
          "Binary dates task with high and low agreement advisors with accuracy targeted to be roughly similar. Constitutes a complete experiment when combined with version 0.0.4.",
        study == "agreementDates" & version == "v0-0-4" ~
          "Binary dates task with high and low agreement advisors with accuracy targeted to be roughly similar. Constitutes a complete experiment when combined with version 0.0.3.",
        study == "calibrationKnowledge" & version == "v0-0-16" ~
          "Binary dates task with advice from familiar (high and low confidence, equal resolution) advisors, but where the exact identity of the advisor is sometimes unknown. Advice-taking is the primary outcome measure of interest. Development and testing version.",
        study == "calibrationKnowledge" & version == "v0-0-18" ~
          "Binary dates task with advice from familiar (high and low confidence, equal resolution) advisors, but where the exact identity of the advisor is sometimes unknown. Advice-taking is the primary outcome measure of interest.",
        study == "calibrationKnowledge" & version == "v0-0-20" ~
          "Binary dates task with advice from familiar (high and low confidence, equal resolution) advisors, but where the exact identity of the advisor is sometimes unknown. Advice-taking is the primary outcome measure of interest. In this version the difference between the advisors was made much clearer and participants were explicitly taught about the differences in confidence.",
        study == "calibrationKnowledge" & version == "v0-1-3" ~
          "Binary dates task with advice from familiar (high and low confidence, equal resolution) advisors, but where the exact identity of the advisor is sometimes unknown. Advice-taking is the primary outcome measure of interest. In this version the difference between the advisors was made much clearer and participants were explicitly taught about the differences in confidence. Furthermore, the experience of advice was constrained to be extremely similar between participants.",
        study == "ConfidenceExploration"  & version == "v0-0-1" ~
          "Binary dates task with advice from either a single advisor or a group of advisors (equally accurate and confident). Advice-taking is the primary outcome measure of interest. The confidence with which advice was given was overwritten by an existing property in this version.",
        study == "confidenceExploration"  & version == "v0-0-4"  ~
          "Binary dates task with advice from either a single advisor or a group of advisors (equally accurate and confident). Advice-taking is the primary outcome measure of interest. The variable correctAnswerSide is mis-calculated in this version and should be explicitly checked.",
        study == "confidenceExploration" & version == "v0-0-10" ~
          "Binary dates task with advice from either a single advisor or a group of advisors (equally accurate and confident). Advice-taking is the primary outcome measure of interest.",
        study == "datesStudy" & version == "v0-0-8" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Multiple advisors/trial.",
        study == "datesStudy" & version == "v0-0-9" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Multiple advisors/trial. Slightly tweaked agreeing advice fallback.",
        study == "datesStudy" & version == "v0-0-10" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial.",
        study == "datesStudy" & version == "v0-0-11" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial. 2s feedback duration and minor tweaks.",
        study == "datesStudy" & version == "v0-0-12" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial. Less variation in advice around the target value (correct/participant's answer).",
        study == "datesStudy" & version == "v0-0-13" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial.",
        study == "datesStudy" & version == "v0-0-14" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial. Advisors now indexed by position not id.",
        study == "datesStudy" & version == "v0-0-15" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial.",
        study == "datesStudy" & version == "v0-0-16" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial.",
        study == "datesStudy" & version == "v0-0-17" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial.",
        study == "datesStudy" & version == "v0-0-18" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial. Advisors now have identicons. Practice phase (without advice) added. No Safari browsers.",
        study == "datesStudy" & version == "v0-0-20" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/trial. No Safari, no Apple.",
        study == "datesStudy" & version == "v0-0-21" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/block.",
        study == "datesStudy" & version == "v0-0-22" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/block. Automatic attention check rejections, participant metadata tags.",
        study == "datesStudy" & version == "v0-1-22" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/block.",
        study == "datesStudy" & version == "v1-0-0" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/block.",
        study == "datesStudy" & version == "v1-0-1" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/block.",
        study == "datesStudy" & version == "v1-1-0" ~
          "Continuous dates task with advice from agreeing and accurate advisors, and with feedback manipulated between participants. Single advisor/block.",
        study == "datesStudyDemo" & version == "v1-1-1" ~
          "Continuous dates task demo.",
        study == "directBenevolence"  & version == "v1-2-0" ~
          "Continuous dates task with similar advisors (misleading one gives one misleading trial). Participants told one advisor will help while the other will sometimes mislead.",
        study == "directBenevolence"  & version == "v1-3-0" ~
          "Continuous dates task with similar advisors (misleading one gives one misleading trial). Participants told one advisor will help while the other will sometimes mislead.",
        study == "directBenevolence"  & version == "v2-0-0" ~
          "Continuous dates task with similar advisors (misleading one gives one misleading trial). Participants told one advisor will help while the other will sometimes mislead. Clearer manipulation where participants must acknowledge advisor properties.",
        study == "directBenevolence"  & version == "v2-1-0" ~
          "Continuous dates task with equivalent advisors. Participants told one advisor will help while the other may sometimes mislead. Clearer manipulation where participants must acknowledge advisor properties.",
        study == "directBenevolence"  & version == "v3-0-0" ~
          "Continuous dates task with equivalent advisors. Participants told one advisor will help while the other may sometimes mislead. Clearer manipulation where participants must acknowledge advisor properties. Participants are asked to rate the misleadingness of each question prior to their final response.",
        study == "directBenevolence"  & version == "v3-0-1" ~
          "Continuous dates task with equivalent advisors. Participants told one advisor will help while the other may sometimes mislead. Clearer manipulation where participants must acknowledge advisor properties. Participants are asked to rate the misleadingness of each question prior to their final response.",
        study == "directBenevolenceContexts"  & version == "v0-0-1" ~
          "Continuous dates task with equivalent advisors. Participants told one group of advisors will help while the other may sometimes mislead. Clearer manipulation where participants must acknowledge advisor properties. Participants are asked to rate the misleadingness of each question prior to their final response.",
        study == "markerUse"  & version == "v0-0-1" ~
          "Continuous dates task with no markers on the timeline.",
        study == "markerUse"  & version == "v0-1-1" ~
          "Continuous dates task.",
        study == "markerUse"  & version == "v0-2-1" ~
          "Continuous dates task.",
        study == "markerUse"  & version == "v0-3-1" ~
          "Continuous dates task.",
        study == "markerUse"  & version == "v0-4-1" ~
          "Continuous dates task.",
        study == "markerUse"  & version == "v0-5-1" ~
          "Continuous dates task.",
        study == "minGroups"  & version == "v0-0-1"  ~
          "Continuous dates task with equivalent advisors represented either as in-group or out-group members. Participant group assignment is by animated coin-flip. Group assignment not clearly recorded.",
        study == "minGroups"  & version == "v1-0-0" ~
          "Continuous dates task with equivalent advisors represented either as in-group or out-group members. Participant group assignment is by animated coin-flip. Group assignment not clearly recorded.",
        study == "minGroups"  & version == "v1-0-1" ~
          "Continuous dates task with equivalent advisors represented either as in-group or out-group members. Participant group assignment is by animated coin-flip.",
        study == "minGroups"  & version == "v1-2-0" ~
          "Continuous dates task with equivalent advisors represented either as in-group or out-group members. Participant group assignment is by animated coin-flip. Advisors use normally distributed advice calibrated to average past participant ability. Highlighted advisor group membership as a salient property.",
        study == "minGroups"  & version == "v2-0-0" ~
          "Continuous dates task with equivalent advisors represented either as in-group or out-group members. Participant group assignment is by animated coin-flip. Participant must acknoweldge advisor group membership dialogue.",
        study == "minGroups"  & version == "v2-0-1" ~
          "Continuous dates task with equivalent advisors represented either as in-group or out-group members. Participant group assignment is by animated coin-flip. Participant must acknoweldge advisor group membership dialogue. Reversed in/out group colours to check weird colour-preference result from previous experiment.",
        study == "withConf" & version == "v0-0-1" ~
          "Continuous dates task.",
        study == "withConf" & version == "v0-0-2" ~
          "Continuous dates task.",
        T ~ NA_character_
      ),
      preregistration = case_when(
        study == 'datesStudy' & version == 'v1-0-1' ~ 'https://osf.io/fgmdw',
        study == 'minGroups' & version == 'v1-0-1' ~ 'https://osf.io/28ktf',
        study == 'minGroups' & version == 'v1-2-0' ~ 'https://osf.io/6hvg5',
        study == 'directBenevolence' & version == 'v3-0-1' ~ 'https://osf.io/tu3ev',
        study == 'directBenevolenceContexts' & version == 'v0-0-1' ~ 'https://osf.io/qjey5',
        study == 'confidenceEstimation' & version == 'v0-0-10' ~ 'https://osf.io/9a84t',
        study == 'advisorChoice' & version == 'v0-0-7' ~ 'https://osf.io/nwmx5',
        study == 'accuracyDates' & version %in% c('v0-0-3', 'v0-0-4') ~ 'https://osf.io/5xpvq',
        study == 'agreementDates' & version %in% c('v0-0-3', 'v0-0-4') ~ 'https://osf.io/8d7vg',
        T ~ NA_character_
      ),
      analysis_url = case_when(
        study == 'accuracyDates' & version %in% c('v0-0-3', 'v0-0-4') ~
          'analysis/ACv2/accuracyDates_v0.0.1.html',
        study == 'agreementDates' & version %in% c('v0-0-3', 'v0-0-4') ~
          'analysis/ACv2/agreementDates_v0.0.1.html',
        study == 'advisorChoice' & version == 'v0-0-6' ~
          'analysis/ACv2/advisorChoice_v0.0.6.html',
        study == 'advisorChoice' & version == 'v0-0-7' ~
          'analysis/ACv2/advisorChoice_v0.0.7.html',
        study == "calibrationKnowledge" & version == "v0-0-18" ~
          'analysis/ACv2/calibrationKnowledge_v0.0.2.html',
        study == "calibrationKnowledge" & version == "v0-1-3" ~
          'analysis/ACv2/calibrationKnowledge_v0.1.3.html',
        study == 'confidenceEstimation' & version == 'v0-0-10' ~
          'analysis/ACv2/confidenceEstimation_v0.0.10.html',
        study == 'datesStudy' & version == 'v0-0-10' ~
          'analysis/ACv2/coreAnalysis_v0.0.10.html',
        study == 'datesStudy' & version == 'v0-0-20' ~
          'analysis/ACv2/coreAnalysis_v0.0.20.html',
        study == 'datesStudy' & version == 'v0-0-21' ~
          'analysis/ACv2/coreAnalysis_v0.0.21.html',
        study == 'datesStudy' & version == 'v0-0-22' ~
          'analysis/ACv2/coreAnalysis_v0.0.22.html',
        study == 'directBenevolence' & version == 'v1-2-0' ~
          'analysis/ACv2/directBenevolence_v1.2.0.html',
        study == 'directBenevolence' & version == 'v1-3-0' ~
          'analysis/ACv2/directBenevolence_v1.3.0.html',
        study == 'directBenevolence' & version == 'v2-0-0' ~
          'analysis/ACv2/directBenevolence_v2.0.0.html',
        study == 'directBenevolence' & version == 'v2-1-0' ~
          'analysis/ACv2/directBenevolence_v2.1.0.html',
        study == 'directBenevolence' & version == 'v3-0-0' ~
          'analysis/ACv2/directBenevolence_v3.0.0.html',
        study == 'directBenevolence' & version == 'v3-0-1' ~
          'analysis/ACv2/directBenevolence_v3.0.1.html',
        study == 'directBenevolenceContexts' & version == 'v0-0-1' ~
          'analysis/ACv2/directBenevolenceContexts_v0.0.1.html',
        study == 'markerUse' & str_detect(version, '^v0-[013-5]-1$') ~
          'analysis/ACv2/markerUse.html',
        study == 'minGroups' & version == 'v1-0-0' ~
          'analysis/ACv2/minGroups_v1.0.0.html',
        study == 'minGroups' & version == 'v1-0-1' ~
          'analysis/ACv2/minGroups_v1.0.1.html',
        study == 'minGroups' & version == 'v1-2-0' ~
          'analysis/ACv2/minGroups_v1.2.0.html',
        study == 'minGroups' & version == 'v2-0-0' ~
          'analysis/ACv2/minGroups_v2.0.0.html',
        study == 'minGroups' & version == 'v2-0-1' ~
          'analysis/ACv2/minGroups_v2.0.1.html',
        study == 'withConf' & version %in% c('v0-0-1', 'v0-0-2') ~
          'analysis/ACv2/withConfidence_coreAnalysis_v0.0.1.html',
        T ~ NA_character_
      ),
      experiment_url = case_when(
        study == "accuracyDates" ~ '?PROLIFIC_PID=esmDataTest&study=ACBin&v=acc.html',
        study == "advisorChoice" ~ '?PROLIFIC_PID=esmDataTest&study=ACc2&v=ac.html',
        study == "agreementDates" ~ '?PROLIFIC_PID=esmDataTest&study=ACBin&v=agr.html',
        study == "calibrationKnowledge" ~ '?PROLIFIC_PID=esmDataTest&study=ACBin&v=ck.html',
        study == "ConfidenceExploration" ~ '?PROLIFIC_PID=esmDataTest&study=ACBin&v=ce.html',
        study == "datesStudy" ~ '?PROLIFIC_PID=esmDataTest&study=ACv2&v=index.html',
        study == "directBenevolence" ~ '?PROLIFIC_PID=esmDataTest&study=ACv2&v=db.html',
        study == "directBenevolenceContexts" ~ '?PROLIFIC_PID=esmDataTest&study=ACv2&v=dbc.html',
        study == "markerUse" ~ '?PROLIFIC_PID=esmDataTest&study=ACv2&v=index.html',
        study == "minGroups" ~ '?PROLIFIC_PID=esmDataTest&study=ACv2&v=mg.html',
        study == "withConf" ~ '?PROLIFIC_PID=esmDataTest&study=ACv2&v=wc.html',
        T ~ NA_character_
      ),
      completeExperiment = case_when(
        study == 'datesStudy' & version == 'v1-0-1' ~ T,
        study == 'minGroups' & version %in% c('v1-0-1', 'v1-2-0') ~ T,
        study == 'directBenevolence' & version == 'v3-0-1' ~ T,
        study == 'directBenevolenceContexts' & version == 'v0-0-1' ~ T,
        study == 'confidenceEstimation' & version == 'v0-0-10' ~ T,
        study == 'advisorChoice' & version %in% c('v0-0-6', 'v0-0-7') ~ T,
        study == "calibrationKnowledge" & version %in% c('v0-0-18', 'v0-1-3') ~ T,
        study == 'confidenceEstimation' & version == 'v0-0-10' ~ T,
        study == 'datesStudy' & version %in% c('v0-0-10', 'v0-0-16', 'v0-0-20', 'v1-0-0', 'v1-0-1') ~ T,
        study == 'directBenevolence' ~ T,
        study == 'minGroups' & version %in% c('v1-0-1', 'v1-2-0', 'v2-0-0', 'v2-0-1') ~ T,
        T ~ F
      ),
      partialExperiment = case_when(
        study == 'accuracyDates' & version %in% c('v0-0-1', 'v0-0-2') ~ T,
        study == 'accuracyDates' & version %in% c('v0-0-3', 'v0-0-4') ~ T,
        study == 'agreementDates' & version %in% c('v0-0-1', 'v0-0-2') ~ T,
        study == 'agreementDates' & version %in% c('v0-0-3', 'v0-0-4') ~ T,
        study == 'markerUse' & str_detect(version, '^v0-[013-5]-1$') ~ T,
        study == 'withConf' & version %in% c('v0-0-1', 'v0-0-2') ~ T,
        T ~ F
      ),
      replication = case_when(
        study == 'advisorChoice' & version == 'v0-0-7' ~ T,
        study == 'directBenevolence' & version %in% c('v1-3-0') ~ T,
        study == 'directBenevolence' & version %in% c('v3-0-1') ~ T,
        T ~ F
      ),
      manipulationOK = case_when(
        study == 'accuracyDates' & version %in% c('v0-0-3', 'v0-0-4') ~ T,
        study == 'agreementDates' & version %in% c('v0-0-3', 'v0-0-4') ~ T,
        study == "calibrationKnowledge" & version %in% c('v0-0-18', 'v0-1-3') ~ T,
        study == 'confidenceEstimation' & version %in% c('v0-0-1', 'v0-0-4', 'v0-0-10') ~ T,
        study == "datesStudy" & (str_detect(version, '^v0-0-[12][0-8]$') | version %in% c('v1-0-1', 'v1-1-0')) ~ T,
        study == 'directBenevolence' ~ T,
        study == 'markerUse' & str_detect(version, '^v0-[0-5]-1$') ~ T,
        T ~ F
      )
    )
}

#' Fetch dictionary files from the local webserver
#' @importFrom dplyr mutate %>% select
#' @importFrom purrr map
#' @importFrom rlang .data
#' @return list of dictionaries in JSON format
getDictionaries.dates <- function() {
  dicts <- tibble(name = c(
    'AdvisedTrial',
    'AdvisedTrialWithConf',
    'advisors',
    'debrief-advisors',
    'debrief-form',
    'demographics-form',
    'metadata',
    'practiceAdvisedTrial',
    'practiceTrial',
    'study-details',
    'Trial'
    ))
  dicts %>%
    mutate(
      url = paste0('http://localhost/ExploringSocialMetacognition/data/public/dictionary_', .data$name, '.csv'),
      csv = map(
        url,
        ~ read.csv(., header = F) %>%
          as_tibble()
      ),
      json = map(.data$csv, dictToJSON)
    ) %>% select(.data$name, .data$json)
}

#' Convert a dictionary CSV file into a metadata JSON file
#' @param dict dictionary.csv to convert
#' @importFrom dplyr mutate %>% select
#' @importFrom tibble has_name
#' @importFrom rlang .data
#' @importFrom jsonlite toJSON
dictToJSON <- function(dict) {
  # Correct names if they're missing
  if (is.null(colnames(dict)) | has_name(dict, 'V1')) {
    if (ncol(dict) == 2)
      names(dict) <- c('name', 'description')
    else if (ncol(dict) == 3)
      names(dict) <- c('name', 'value', 'description')
    else
      stop(paste0('Expected dictionary to have 2 or 3 columns, not ', ncol(dict), '.'))
  }
  if (!has_name(dict, 'value'))
    dict <- dict %>%
      mutate(value = '*') %>%
      select(.data$name, .data$value, .data$description)

  jsonlite::toJSON(dict)
}

#' Save local copy of dictionaries
#' @param dicts tbl of dictionaries with columns name and json
#' @param prefix string to include before the dictionary name
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv
#' @return NULL (invisible)
saveLocalDictionaries <- function(dicts, prefix = '') {
  if (prefix != '')
    prefix <- paste0(prefix, '-')
  apply(
    dicts,
    1,
    function(x)
      write.csv(
        jsonlite::fromJSON(x$json),
        paste0('inst/extdata/dictionary_', prefix, x$name, '.csv'),
        row.names = F)
  )
  invisible(NULL)
}

#' Add the labels to files' data fields from the associated dictionary files
#' @param files list of files to add labels to
#' @param dictionaryPrefix prefix to ensure the correct dictionary is found
#' @return files with labels added
addLabels <- function(files, dictionaryPrefix = '') {
  #' @importFrom stringr str_match str_replace_all
  #' @importFrom dplyr mutate %>%
  #' @importFrom rlang .data
  .getLabel <- function(dict, v) {
    # Advisor variables are tagged with advisor number
    m <- str_match(v, 'advisor([0-9])+')
    if (!is.na(m[, 1])) {
      n <- m[, 2]
      dict <- dict %>%
        mutate(
          name = str_replace_all(.data$name,
                                 'advisor\\[0-9\\]\\+',
                                 paste0('advisor', n)),
          description = str_replace_all(.data$description,
                                        'advisor( ?)\\[0-9\\]\\+',
                                        paste0('advisor\\1', n))
          )
    }
    if (v %in% dict$name)
      dict$description[dict$name == v]
    else
      NULL
  }
  #' @importFrom sjlabelled set_label
  #' @importFrom stringr str_detect
  #' @importFrom utils read.csv
  .addLabels <- function(f) {
    if (dictionaryPrefix != '' & !str_detect(dictionaryPrefix, '-$'))
      dictionaryPrefix <- paste0(dictionaryPrefix, '-')
    dictPath <- paste0('inst/extdata/dictionary_', dictionaryPrefix, f$table, '.csv')
    if (!file.exists(dictPath)) {
      warning(paste0('No dictionary file found for ', f$table, ', missing ', dictPath, '.'))
      return(f$data)
    }
    dict <- read.csv(dictPath)
    for (n in names(f$data)) {
      d <- .getLabel(dict, n)
      if (!is.null(d))
        f$data[, n] <- set_label(f$data[, n], d)
      else
        print(paste0(f$table, '.', n))
        # warning(paste0('Variable "', n, '" has no dictionary entry in ', dictPath, '.'))
    }
    f$data
  }
  files$data <- apply(files, 1, .addLabels)
  files
}
