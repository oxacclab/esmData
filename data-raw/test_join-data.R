context("Load raw data")
library(esmData)
library(tidyverse)
source('data-raw/getRawDotsTaskData.R')

test_that("fetchRawData fragment loads data", {
  logfile <- 'I:/TMP/r-test_datequiz-raw.log'
  if (file.exists(logfile)) unlink(logfile)

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
    as_tibble(.name_repair = 'minimal') %>%
    set_names(c('url', 'study', 'version', 'table'))


  x <- files %>% filter(str_detect(table, '^practiceAdvisedTrial$'))
  # x$data <- apply(x, 1, .fetchData)

  cl <- makeCluster(detectCores() - 2, outfile = logfile)
  clusterExport(cl, c('.fetchData', 'enforceTypes'))
  x$data <- parApply(cl, x, 1, .fetchData)
  stopCluster(cl)

  z <- NULL
  for (d in x$data) z <- bind_rows(z, d)

  for (d in x$data) print(typeof(d$correctAnswer))
})

test_that("fetchRawData.dots loads data", {
  source('data-raw/ESM_core.R')

  path <- "G:/Documents/University/Google Drive/Project Documents/AdvisorChoice/results"
  folders <- list.dirs(path, recursive = T)
  folders <- folders[grepl("processed$", folders)]

  folders <- folders[str_detect(folders, 'Accuracy')]

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
      url = folderName,
      table = names(results),
      data = map(table, ~ results[[.x]] %>%
                   as_tibble() %>%
                   enforceTypes(paste0('dots-', .x)))
    )

    # drop empty
    df <- df %>%
      mutate(n = map_dbl(.data$data, length)) %>%
      filter(.data$n > 0) %>%
      select(-.data$n)

    tmp <- rbind(tmp, df)
  }

  z <- NULL
  for (d in tmp %>% filter(table == 'participants') %>% .$data) z <- bind_rows(z, d)
})
