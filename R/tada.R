#' Wrapper around utils::data() which tibblifies the resulting list objects
#' @param list character vector of sources to load
#' @param envir environment to write to
#' @param overwrite whether to overwrite existing objects in \code{envir}
#' @importFrom utils data
#' @importFrom tibble as_tibble tibble
#' @inheritDotParams utils::data
#' @return tibble report of variables loaded and their statuses (skipped,
#'   tibblified, or imported)
#' @export
tada <- function(list, envir = .GlobalEnv, overwrite = T, ...) {
  report <- NULL
  tmpEnv <- new.env()
  data(list = list, envir = tmpEnv, overwrite = T, ...)

  for (n in ls(envir = tmpEnv)) {
    o <- get(n, envir = tmpEnv)
    if (!overwrite & n %in% ls(envir)) {
      report <- rbind(report, tibble(name = n, status = 'skipped'))
      next()
    }
    if (is.data.frame(o)) {
      report <- rbind(report, tibble(name = n, status = 'tibblified'))
      o <- as_tibble(o)
    } else {
      report <- rbind(report, tibble(name = n, status = 'imported'))
    }
    assign(n, o, envir = envir)
  }

  report
}
