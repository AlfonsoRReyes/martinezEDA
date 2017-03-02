library(R.matlab)

readMatfile <- function(matfile, folder = project.extdata, verbose = FALSE) {
  matfileLong <- paste(folder, matfile, sep = "/")
  ret <- readMat(matfileLong, fixNames = TRUE)
  if (verbose) print(str(ret))
  return(ret)
}

savetoRda <- function(..., file, folder = project.data) {
  rdafileLong <- paste(folder, file, sep = "/")
  save(..., file = rdafileLong)
}
