#' Expand nested tables into their own tibbles in the environment
#' @param project 'dotstask' or 'datequiz'
#' @param f filter function like \code{function(x) dplyr::filter(x, ...)}
#'   specifying filter conditions
#' @param envir environment to write to
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% bind_rows across summarise select everything
#' @importFrom rlang has_name .data
#' @export
select_experiment <- function(project, f = function(x) dplyr::filter(x), envir = .GlobalEnv) {
  .fEnv <- new.env()
  tada(project, package = 'esmData', envir = .fEnv)

  assign('D', get(project, envir = .fEnv))

  D <- f(D)

  # Produce summary table
  assign(
    project,
    D %>%
      select(-url, -data) %>%
      summarise(across(-table), tables = paste0(table, collapse = ';')) %>%
      unique(),
    envir = envir
  )

  for (i in 1:nrow(D)) {
    n <- D$table[i]
    if (n %in% ls(envir = envir)) {
      # Try to merge dataframes
      tryCatch({
        x <- D$data[[i]]
        # Ensure study id and version are recorded
        if (!has_name(x, 'studyId'))
          x$studyId <- D$study[i]
        if (!has_name(x, 'studyVersion'))
          x$studyVersion <- D$version[i]
        x <- select(
          x,
          matches('^p?id$'),
          .data$studyId,
          .data$studyVersion,
          everything()
        )
        assign(n, bind_rows(get(n, envir = envir), x), envir = envir)
      }, error = function(e) {
        warning(paste0('Unable to automatically join rows for "',
                       n, '" (', D$study[i], ' ', D$version[i], ').\n',
                       'bind_rows() error was: ', e))
        assign(n, rbind(get(n, envir = envir), D$data[[i]]), envir = envir)
      })
    } else {
      assign(n, D$data[[i]], envir = envir)
    }
  }
}
