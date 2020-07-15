# Procedural code ---------------------------------------------------------

source('data-raw/getRawDateQuizData.R')

saveLocalDictionaries(getDictionaries.dates())

datequiz <- fetchRawData()
datequiz <- tagData(datequiz)
datequiz <- addLabels(datequiz)

usethis::use_data(datequiz, overwrite = TRUE)
