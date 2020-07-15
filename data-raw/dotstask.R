# Procedural code ---------------------------------------------------------

source('data-raw/getRawDotsTaskData.R')

saveLocalDictionaries.dots()

dotstask <- fetchRawData.dots()
dotstask <- tagData.dots(dotstask)
dotstask <- addLabels.dots(dotstask)

usethis::use_data(dotstask, overwrite = TRUE)
