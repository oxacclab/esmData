#' Lab data from the Exploring Social Metacognition Dots Task
#'
#' @note This dataset cannot be loaded using the
#' \code{\link{select_experiment}} function. It should instead be loaded using
#' \code{\link{data}}.
#'
#' Data collected from a lab-based behavioural psychological experiment in
#' which participants view two boxes containing around 200 dots each and make a
#' decision as to whether the left or the right box contains more dots. On most
#' trials, participants receive advice from an advisor and make a final
#' decision. Both the initial and final decision are made on a horizontal bar
#' with the extremes representing certainty that the box on that side had the
#' most dots, and the middle region representing relative uncertainty.
#'
#' @format A list with four items: \describe{
#'   \item{advisors}{A tibble with 100 rows and 6 columns:
#'     \describe{
#'       \item{participantId}{Identifier for the participant}
#'       \item{id}{Identifier for the advisor}
#'       \item{advice.type}{Code for the advice offered by the advisor.
#'         \enumerate{
#'           \item neutral
#'           \item Agree in Confidence (Bias sharing)
#'           \item Agree in Uncertainty (Anti-bias)
#'         }}
#'       \item{portrait}{Portrait number for the advisor}
#'       \item{voice}{Voice number for the advisor}
#'       \item{name}{Name assigned to the advisor}
#'     }}
#'   \item{participants}{A tibble with 25 rows and 17 columns:
#'     \describe{
#'       \item{participantId}{Identifier for the participant}
#'       \item{gender}{Participant's self-reported gender}
#'       \item{age}{Participant's self-reported age}
#'       \item{restarted}{Whether the experiment was resumed from earlier
#'       failure}
#'       \item{screen}{Screen used for display}
#'       \item{computer}{Operating system code}
#'       \item{os}{Operating system description}
#'       \item{debug}{Whether the study was run in debug mode}
#'       \item{short.mode}{Whether the study was run in short mode}
#'       \item{stimulus.response.interval, ...}{Experimental settings (see
#'       MATLAB experiment code for their functions)}
#'     }}
#'   \item{questionnaires}{A tibble with 800 rows and 11 columns:
#'     \describe{
#'       \item{answer}{Answer given on a sliding scale}
#'       \item{questionNumber}{Number of the question}
#'       \item{initialPosition}{Initial answer position of the slider}
#'       \item{presentationOrder}{Question's position in the presentation
#'       sequence at that time point}
#'       \item{hasChanged}{Whether the participant moved the slider}
#'       \item{responseTime}{Seconds taken for the answer to be submitted}
#'       \item{onsetTime}{Time at which the question was displayed to the
#'       participant}
#'       \item{participantId}{Identifier for the participant}
#'       \item{timePoint}{Which stage of the experiment at which this set of
#'       questions occurs}
#'       \item{questionId}{Identifier for the question text}
#'       \item{advisorId}{Identifier for the advisor who is the subject of the
#'       question}
#'     }}
#'   \item{trials}{A tibble with 9,075 rows and 50 columns:
#'     \describe{
#'       \item{participantId}{Identifier for the participant}
#'       \item{id}{trial identifier}
#'       \item{block}{experimental block the trial belongs to}
#'       \item{break}{whether there is a break after the trial}
#'       \item{instr}{whether there are instructions accompanying the trial}
#'       \item{questionnaire}{whether there is a questionnaire after the trial}
#'       \item{feedback}{whether the trial is accompanied with feedback (tone
#'       on error)}
#'       \item{practice}{whether the trial is labelled as a practice trial}
#'       \item{obsacc}{whether the advisor is correct in their advice}
#'       \item{agree}{whether the advisor agrees with the participant}
#'       \item{step}{whether the confidence expressed by the participant in
#'       their initial decision is in the bottom 30\% (-1), middle 40\% (0), or
#'       top 30\% (1) of initial confidence ratings given over the last 2 blocks}
#'       \item{advisorId}{identification number of advisor}
#'       \item{choice}{list of the advisors offered as a choice on the trial in
#'       position [top, bottom]}
#'       \item{advisorPolitics}{not used}
#'       \item{advisorPoliticsQ}{not used}
#'       \item{taskType}{not used}
#'       \item{cj1}{Participant's initial response}
#'       \item{cj2}{Participant's final response}
#'       \item{cor}{whether participant's (final or initial if no final
#'       response is taken) answer is correct}
#'       \item{cor1}{whether participant's initial answer is correct}
#'       \item{cor2}{whether participant's final answer is correct}
#'       \item{choiceDecision}{whether the participant selected the top (1) or
#'       bottom (2) advisor}
#'       \item{choiceTime}{time taken to choose the advisor (s)}
#'       \item{estim_obsacc}{Participant's estimate of the accuracy of the
#'       advisors [first advisor, last advisor]}
#'       \item{int1}{Whether the initial response chose the left (1) or right
#'       (2) side}
#'       \item{int2}{Whether the final response chose the left (1) or right
#'       (2) side}
#'       \item{offset_speech}{timestamp of advisor's speech ending}
#'       \item{offsetstim}{timestamp of stimulus offset}
#'       \item{onsetstim}{timestamp of stimulus onset}
#'       \item{time_responseStart1}{timestamp of initial response opening}
#'       \item{time_responseStart2}{timestamp of final response opening}
#'       \item{time_response1}{timestamp of initial response}
#'       \item{time_response2}{timestamp of final response}
#'       \item{time_startTrial}{timestamp of trial start}
#'       \item{time_endTrial}{timestamp of trial end}
#'       \item{tmissed_offset1}{delay between target offset and actual offset
#'       (s) for initial decision}
#'       \item{tmissed_onset1}{delay between target onset and actual onset (s)
#'       for initial decision}
#'       \item{qanswers}{Questionnaire answers - struct (unpacked in
#'       \code{questionnaires} dataframe)}
#'       \item{wherelarger}{whether the box with the most dots is on the left
#'       (1) or right (2)}
#'       \item{wheredots}{2x400 array of the possible dot positions for the
#'       left [1,] and right [2,] boxes. 1s represent dots while 0s represent
#'       blank spaces}
#'       \item{dotdifference}{the number of dots subtracted from the less
#'       numerous box and added to the more numerous box}
#'       \item{time_starttrial}{timestamp of the trial start}
#'       \item{endTime}{timestamp of the trial end}
#'       \item{advice_order}{whether the advisor's "I think" precedes or
#'       follows the direction endorsement}
#'       \item{whichspeech}{Not used}
#'       \item{onsetobsspeech}{timestamp of advice speech onset}
#'       \item{onsetobs}{timestamp of advisor portrait display onset}
#'       \item{tmissed_onset2}{delay between target onset and actual onset (s)
#'       for final decision}
#'       \item{offsetobs}{timestamp of advisor portrait display offset}
#'       \item{tmissed_offset2}{delay between target offset and actual offset
#'       (s) for final decision}
#'     }}
#'   }
#' @source \url{https://osf.io/vgcnb/} via some data wrangling to convert from
#' MATLAB to R
"dotstask_lab"
