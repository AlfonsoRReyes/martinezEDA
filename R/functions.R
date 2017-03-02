library(R.matlab)
library(RcppOctave)

readMatfile <- function(matfile, folder = project.extdata, verbose = FALSE) {
  matfileLong <- paste(folder, matfile, sep = "/")
  ret <- readMat(matfileLong, fixNames = FALSE)
  if (verbose) print(str(ret))
  return(ret)
}

savetoRda <- function(..., file, folder = project.data) {
  rdafileLong <- paste(folder, file, sep = "/")
  save(..., file = rdafileLong)
}

#' load .mat file in Octave
#' and get basic info
o.loader <- OctaveFunction("
function [struct] = readMatfile(mfile)
  load(mfile)
  struct = whos();
end
")

#' get information of .mat file objects
#' as a data frame
matInfo <- function(matFile) {
  ld <- o.loader(matFile)
  sz <- paste(sapply(ld$size, `[[`, 1), sapply(ld$size, `[[`, 2), sep = "x")
  df <- data.frame(name = ld$name, size = sz, bytes = ld$bytes, class = ld$class)
  return(df)
}
