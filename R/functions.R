library(R.matlab)

readMatfile <- function(matfile, folder = project.extdata) {
  matfileLong <- paste(folder, matfile, sep = "/")
  ret <- readMat(matfileLong, fixNames = TRUE)
  print(str(ret))
  return(ret)
}

savetoRda <- function(..., file, folder = project.data) {
  rdafileLong <- paste(folder, file, sep = "/")
  save(..., file = rdafileLong)
}
