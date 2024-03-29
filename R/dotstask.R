#' Data from the Exploring Social Metacognition Dots Task
#'
#' Data collected from a series of behavioural psychological experiments in
#' which participants view two boxes containing around 200 dots each and make a
#' decision as to whether the left or the right box contains more dots. On most
#' trials, participants receive advice from an advisor and make a final
#' decision. Both the initial and final decision are made on a horizontal bar
#' with the extremes representing certainty that the box on that side had the
#' most dots, and the middle region representing relative uncertainty.
#'
#' This dataset contains many experiments on this theme, as well as metadata
#' which provides basic information about each individual study.
#'
#' @format A tibble with 80 rows and 14 variables: \describe{
#'   \item{folderName}{the original source of the data - should eventually point to the OSF repository for the data}
#'   \item{table}{the kind of data held}
#'   \item{data}{tibble containing the actual data. Columns have a label attr giving further details}
#'   \item{study}{name of the study for in which the data were collected}
#'   \item{version}{version of the study in which the data were collected}
#'   \item{date}{date the study data were collected, if known}
#'   \item{N}{number of participants in the data}
#'   \item{description}{short description of the study}
#'   \item{preregistration}{URL of the study preregistration document, if available}
#'   \item{url}{URL of the study analysis document relative to the web project root directory}
#'   \item{completeExperiment}{whether the data constitute a complete experiment}
#'   \item{partialExperiment}{whether the data constitute a complete experiment when combined with appropriate other data (e.g. for studies split across versions)}
#'   \item{replication}{whether the study is an exact replication of a previous study in the dataset}
#'   \item{manipulationOK}{whether the study manipulation was successful (in form rather than function - ineffective but accurately delivered manipulations are 'OK')}
#'   }
#' @source \url{https://osf.io/rm49v/}
"dotstask"
