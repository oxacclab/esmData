# Fetch data from the local filesystem

# List server files -------------------------------------------------------

fetchRawData.dots <- function() {
  source('R/ESM_core.R')
  require(purrr)
  require(dplyr)
  require(tibble)

  path <- "G:/Documents/University/Google Drive/Project Documents/AdvisorChoice/results"
  folders <- list.dirs(path, recursive = T)
  folders <- folders[grepl("processed$", folders)]

  tmp <- NULL

  for (folderName in folders) {
    print(paste('Extracting', length(list.files(folderName)),
                'files from', folderName))

    if (!length(list.files(folderName)))
      next()
    results <- loadFilesFromFolder(folderName)
    results <- removeParticipantIds(results)

    results$trials <- bind_cols(results$trials, trialUtilityVariables(results))

    df <- tibble(
      folderName = folderName,
      table = names(results),
      data = map(table, ~ results[[.]] %>% as_tibble())
    )

    # drop empty
    df <- df %>% mutate(n = map_dbl(data, length)) %>% filter(n > 0) %>% select(-n)

    tmp <- rbind(tmp, df)
  }

  # split by study and version
  tmp <- tmp %>%
    mutate(
      study = str_match(folderName, '/AdvisorChoice/results/([^/]+)/')[, 2],
      version = str_match(folderName, '/AdvisorChoice/results/[^/]+/([^/]+)')[, 2],
      version = if_else(version %in% c('processed', 'raw'), study, version),
      date = str_match(version, '^([0-9]{4}-[0-9]{2}-[0-9]{2})'),
      date = if_else(is.na(date[, 1]), NA_character_, date[, 2]),
      version = str_remove(version, '^[0-9]{4}-[0-9]{2}-[0-9]{2}\\s*')
    )

  tmp
}

exportRawData.dots <- function(files, name = 'dotstask.raw') {
  require(stringr)
  path <- paste0('extdata/', str_replace(name, '\\.', '-'), '.Rdata')
  assign(name, files)
  save(list = c(name), file = path)
}

exportFullData.dots <- function(files, name = 'dotstask.full') {
  require(stringr)
  path <- paste0('data/', str_replace(name, '\\.', '-'), '.Rdata')
  print(paste0('Export data to ', path, '.'))
  print('1. Tag data')
  files <- tagData.dots(files)
  print('2. Add labels')
  files <- addLabels.dots(files)
  print('3. Save.')
  assign(name, files)
  save(list = c(name), file = path)
}

#' Add information about studies to the raw data table
#' @param files raw data table
#' @return files with metadata added
tagData.dots <- function(files) {
  require(dplyr)
  require(purrr)
  require(stringr)

  files$N <- sapply(
    files$data,
    function(x)
      ifelse('pid' %in% names(x),
             length(unique(x$pid)),
             NA_real_)
    )

  files %>%
    mutate(
      description = case_when(
        study == "Accuracy vs agreement" & version == "Pilot data" ~
          "Dots task with high agreement vs high accuracy advisors.",
        study == "Accuracy vs agreement" & version == "v1 Mixed design" ~
          "Dots task with high agreement vs high accuracy advisors. Advisors mixed during familiarization blocks.",
        study == "Accuracy vs agreement" & version == "v2 Serial design" ~
          "Dots task with high agreement vs high accuracy advisors. Advisors presented individually during familiarization blocks.",
        study == "Accuracy" & version == "Bad agreement rates" ~
          "Dots task with high vs low accuracy advisors (controlled for agreement). Agreement rates mistakenly calculated as accuracy rates instead. Agreement bug: 'agree' advice is actually correct answer.",
        study == "Accuracy" & version == "Agreement bug" ~
          "Dots task with high vs low accuracy advisors (controlled for agreement). Agreement bug: 'agree' advice is actually correct answer.",
        study == "Accuracy" & version == "Staircase bug" ~
          "Dots task with high vs low accuracy advisors (controlled for agreement). Participant accuracy exceeded expected values (74 vs 71%) due to a staircase bug.",
        study == "Accuracy" & version == "60 practice trials" ~
          "Dots task with high vs low accuracy advisors (controlled for agreement). 60 practice trials for accuracy convergence to target values.",
        study == "Accuracy" & version == "120 practice trials" ~
          "Dots task with high vs low accuracy advisors (controlled for agreement). 120 practice trials sufficed for accuracy convergence to target values.",
        study == "Agreement" & version == "Agreement" ~
          "Dots task with high vs low agreement advisors (controlled for accuracy).",
        study == "DotTask staircase check" & version == "74percent accuracy" ~
          "Dots task with no advice. Attempt to debug staircase issues.",
        study == "DotTask staircase check" & version == "72percent accuracy" ~
          "Dots task with no advice. Attempt to debug staircase issues.",
        study == "MetaCog" & version == "2 - Bad initial exposure" ~
          "Dots task with a manipulation failure - advisors not differentiated in the force-choice block. Agreement bug: 'agree' advice is actually correct answer.",
        study == "MetaCog" & version == "2b - Wrong agreement" ~
          "Dots task with misapplied advisor agreement - advisors 'agree' responses were in fact the correct response.",
        study == "MetaCog" & version == "2c Fixed" ~
          "Dots task with agree-in-confidence (bias sharing) and agree-in-uncertainty (anti-bias) advisors.",
        T ~ NA_character_
      ),
      preregistration = case_when(
        study == "Accuracy vs agreement" & version == "v1 Mixed design" ~
          "https://osf.io/5z2fp",
        study == "Accuracy vs agreement" & version == "v2 Serial design" ~
          "https://osf.io/f3k4x",
        study == "MetaCog" & version == "2c Fixed" ~
          "https://osf.io/h6yb5",
        study == "Accuracy" & version == "Agreement bug" ~
          "https://osf.io/3n5ek",
        study == "Accuracy" & version == "120 practice trials" ~
          "https://osf.io/u5hgj",
        T ~ NA_character_
      ),
      analysis_url = case_when(
        study == "Accuracy vs agreement" & version == "v2 Serial design" ~
          "analysis/acc-vs-agr-dots.html",
        study == "MetaCog" & version == "2c Fixed" ~
          "analysis/advisorChoiceMetaCog.html",
        study == "Accuracy" & version == "Agreement bug" ~
          "analysis/advisorChoiceAgreement_ManualExclusions.html",
        study == "Accuracy" & version == "120 practice trials" ~
          "analysis/advisorChoiceAccuracy_ManualExclusions.html",
        T ~ NA_character_
      ),
      completeExperiment = case_when(
        study == "Accuracy vs agreement" & version == "v1 Mixed design" ~ T,
        study == "Accuracy vs agreement" & version == "v2 Serial design" ~ T,
        study == "Accuracy" & version == "Bad agreement rates" ~ T,
        study == "Accuracy" & version == "Agreement bug" ~ T,
        study == "Accuracy" & version == "120 practice trials" ~ T,
        study == "Agreement" & version == "Agreement" ~ T,
        study == "MetaCog" & version == "2 - Bad initial exposure" ~ T,
        study == "MetaCog" & version == "2b - Wrong agreement" ~ T,
        study == "MetaCog" & version == "2c Fixed" ~ T,
        T ~ F
      ),
      partialExperiment = case_when(
        T ~ F
      ),
      replication = case_when(
        T ~ F
      ),
      manipulationOK = case_when(
        study == "Accuracy vs agreement" & version == "Pilot data" ~ T,
        study == "Accuracy vs agreement" & version == "v1 Mixed design" ~ T,
        study == "Accuracy vs agreement" & version == "v2 Serial design" ~ T,
        study == "Accuracy" & version == "Staircase bug" ~ T,
        study == "Accuracy" & version == "60 practice trials" ~ T,
        study == "Accuracy" & version == "120 practice trials" ~ T,
        study == "Agreement" & version == "Agreement" ~ T,
        study == "DotTask staircase check" & version == "74percent accuracy" ~ T,
        study == "DotTask staircase check" & version == "72percent accuracy" ~ T,
        study == "MetaCog" & version == "2 - Bad initial exposure" ~ F,
        study == "MetaCog" & version == "2b - Wrong agreement" ~ F,
        study == "MetaCog" & version == "2c Fixed" ~ T,
        T ~ F
      )
    )
}

getDictionaries.dots <- function() {
  require(jsonlite)
  dicts <- tibble(name = c('advisors', 'debrief', 'genTrustQ', 'participants', 'questionnaires', 'trials'))
  dicts %>%
    mutate(
      url = paste0('http://localhost/ExploringSocialMetacognition/analysis/dictionary_', name, '.csv'),
      csv = map(
        url,
        ~ read.csv(., header = F) %>%
          as_tibble()
      ),
      json = map(csv, dictToJSON)
    ) %>% select(name, json)
}

addLabels.dots <- function(files, dictionaryPrefix = 'dots') {
  addLabels(files, dictionaryPrefix = dictionaryPrefix)
}

saveLocalDictionaries.dots <- function() saveLocalDictionaries(getDictionaries.dots(), prefix = 'dots')

load('extdata/dotstask-raw.Rdata')
load('data/dotstask-full.Rdata')
