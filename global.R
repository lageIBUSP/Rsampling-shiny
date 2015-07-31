##################################################
###       MULTILINGUAL VARIABLES AND FUNCTION  ###
#################################################
ifelse(exists("language"),lg <- language, lg <- "en")
lang <- unique(read.csv('lang.csv', sep = ';', stringsAsFactors = FALSE, fileEncoding="UTF-8"))
tr <- function(text, language, D = lang) {
    for (i in 1:length(text)) {
      id <- which(D$value %in% text[i])
      if (length(id) == 0) {
        id <- which(D$en %in% text[i])
        if (length(id) == 1) {
          text[i] <- ifelse(language == "pt", D$pt[id], D$en[id])
        } else if (length(id) > 1) {
          print('Mulit-variable matched!')
        }
      } else if (length(id) == 1) {
        text[i] <- ifelse(language == "pt", D$pt[id], D$en[id])
      } else if (length(id) > 1) {
        print('Mulit-variable matched!')
      }
    }
    return(text)
}
