processAlpha = function(line){
  library("stringr")
  tolower(line) #converting the line to all lower cases
  return (str_count(line, letters))
}

processFile = function(filepath){
  #obtained from here: https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-by-line
  doc = file(filepath, "r")
  alpha = letters
  runningTotal <- data.frame(alpha)

  while(TRUE){
    line = readLines(doc, n = 1)
    if( length(line) == 0){
      break #We reached the end of the file
    }
    runningTotal <- cbind(runningTotal, processAlpha(line)) #I know this is not efficient, I don't know any other way to do it.
  }
  runningTotal$sum <- rowSums(runningTotal[,c(-1)]) #summing up all of the rows, ignoring the first column
  totalCounts = runningTotal$sum # renaming the column header
  finalDf <- data.frame(alpha, totalCounts)

  close(doc)
  return(finalDf)
}

