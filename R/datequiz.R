#' Data from the Exploring Social Metacognition Dates Task
#'
#' Data collected from a series of behavioural psychological experiments in
#' which participants made estimates about when events occurred and then updated
#' those estimates based on advice.
#'
#' The task has two versions, a continuous (timeline) version and a binary
#' version. In the timeline version, participants place markers of various
#' widths on a timeline stretching from 1890 to 2010 to identify when a series
#' of events took place. Events are presented one at a time, and for each event
#' participants drop one of up to three markers of different widths (thinner
#' markers are worth more points) on the timeline to indicate their initial
#' estimate of when the event occurred, before seeing a marker placed by an
#' advisor and getting the opportunity to alter their original marker's position
#' in making their final decision.
#'
#' In the binary version of the task, participants have two vertical bars, one
#' on the left of the screen labelled 'before', and one on the right labelled
#' 'after'. Participants see an event at the top of the screen alongside an
#' 'anchor date'. Participants then decide which bar to select for their answer,
#' 'before' or 'after', and click a vertical position on the bar to represent
#' their confidence in the answer (the higher up the bar they click, the more
#' confidence they report). Participants then see either binary advice (which
#' identifies one or other bar as the advisor's estimate) or, in other
#' experiments, graduated advice (which indicates a height on a bar to show the
#' advisor's confidence in their advice). Participants then select a location on
#' one of the bars which represents their final decision.
#'
#' This dataset contains many experiments on this theme, as well as metadata
#' which provides basic information about each individual study. A common
#' manipulation is the presence or absence of feedback during the familiarity
#' phase in which participants are introduced to their advisors.
#'
#' @format A tibble with 478 rows and 14 variables: \describe{
#'   \item{url}{URL of the study
#'   analysis document relative to the web project root directory}
#'   \item{study}{name of the study for in which the
#'   data were collected}
#'   \item{version}{version of the study in which the data
#'   were collected}
#'   \item{table}{the kind of data held}
#'   \item{data}{tibble containing the actual data. Columns have a label attr
#'   giving further details}
#'   \item{N}{number of participants in the data}
#'   \item{description}{short
#'   description of the study}
#'   \item{preregistration}{URL of the study
#'   preregistration document, if available}
#'   \item{experiment_url}{URL of the experiment relative to the web project root directory}
#'   \item{completeExperiment}{whether the data constitute a complete
#'   experiment}
#'   \item{partialExperiment}{whether the data constitute a complete
#'   experiment when combined with appropriate other data (e.g. for studies
#'   split across versions)}
#'   \item{replication}{whether the study is an exact
#'   replication of a previous study in the dataset}
#'   \item{manipulationOK}{whether the study manipulation was successful (in
#'   form rather than function - ineffective but accurately delivered
#'   manipulations are 'OK')}
#'   }
#' @source \url{https://acclab.psy.ox.ac.uk/~mj221/ESM/data/public/}
"datequiz"
