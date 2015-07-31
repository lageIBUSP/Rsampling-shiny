##################################################
###       MULTILINGUAL VARIABLES AND FUNCTION  ###
#################################################
# The default language is "en". To use the translated version, set the language such as
# > language <- "pt" 
# before calling the app
language <- ifelse(exists("language"), language, "en")
dictionary <- read.csv('lang.csv', sep = ';', stringsAsFactors = FALSE, fileEncoding="UTF-8", header=TRUE)
# If the language is not recognized, does not attempt to run the app. Stop immediately:
if (! language %in% colnames(dictionary)[-1])
  stop(paste0("Language is not recognized: ", language, "\n"))
## Main translation function. Uses "language" and "dictionary" as globals, defined above
# in this file. 
tr <- function(text) {
    for (i in 1:length(text)) {
      id <- which(dictionary$value %in% text[i])
      # Trying again: 
      if (length(id) == 0) 
        id <- which(dictionary$en %in% text[i])
      # If still no matches, prints a warning:
      if (length(id) == 0) 
        cat(paste('No matches for text:', text[i], '\n'))
      if (length(id) == 1) {
        text[i] <- ifelse(language == "pt", dictionary$pt[id], dictionary$en[id])
      } else if (length(id) > 1) {
        cat(paste('More than one match for text:', text[i], '\n'))
      }
    }
    return(text)
}
