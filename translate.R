

update.dictionary = function(inputFile="ui.R", dictionaryFile = "tempDict.csv"){
  text=scan(inputFile, what="character", sep="\n", encoding="unknown")
  pat="\\(tr\\(([^\\)])+\\)"
  s=grep(pattern = pat,x=text,value = TRUE)
  s=gsub(pattern="  ",replacement = "",x = s)
  s=gsub(pattern="  ",replacement = "",x = s)
  s=unlist(strsplit(s,"tr\\("))
  s=gsub(pattern="h[0-9]\\(|p\\(|helpText\\(| strong\\(|tabPanel\\(",replacement =" ",x = s) 
  s=unlist(s)
  s= sub(",lg\\).*","",s)
  s= sub(", lg\\).*","",s)
  s=unique(s[s!=""])
  s=unique(s[s!=" "])
  s=paste(s,s,NA,sep=";")
  write.table(x = s,file = "test.txt",row.names = FALSE,quote = FALSE,col.names = TRUE,sep=";")
  #colnames(s) = c("value","en","pt")
 
  text2=scan("lang.csv", what="character", sep="\n", encoding="unknown")

  s <- read.table(textConnection(s), sep = ";");
  text2 <- read.table(textConnection(text2), sep = ";");
  text2=text2[-1,]

  q=rbind(text2,s)
  colnames (q)= c("value","en","pt")

  q=q[unique(q$value),]
  
  write.table(x = q,file = dictionaryFile,row.names = FALSE,quote = FALSE,col.names = TRUE,sep=";")
}
update.dictionary()
