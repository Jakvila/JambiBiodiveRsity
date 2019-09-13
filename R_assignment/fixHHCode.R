fixHHcode = function(rawVector, targetIDLength = 6){
  rawVector = as.character(rawVector)
  rawVector = gsub(",", ".", rawVector)
  rawVector = gsub(";", ".", rawVector)
  rawVector[nchar(rawVector) == (targetIDLength - 1)] = paste(rawVector[nchar(rawVector) == (targetIDLength - 1)], "0", sep = "")

  return(rawVector)
}
