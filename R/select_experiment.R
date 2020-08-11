#' Expand nested tables into their own tibbles in the environment
#' @param project 'dotstask' or 'datequiz'
#' @param f filter function like \code{function(x) dplyr::filter(x, ...)}
#'   specifying filter conditions
#' @param envir environment to write to
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr %>% bind_rows across summarise select
#' @export
select_experiment <- function(project, f = function(x) dplyr::filter(x), envir = .GlobalEnv) {
  tada(project, package = 'esmData')

  assign('D', get(project))

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
        assign(n, bind_rows(get(n, envir = envir), D$data[[i]]), envir = envir)
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
