library(R.matlab)
library(RcppOctave)

readMatfile <- function(matfile, folder = project.extdata, verbose = FALSE) {
  matfileLong <- paste(folder, matfile, sep = "/")
  # print(matfileLong)
  ret <- readMat(matfileLong, fixNames = FALSE)
  if (verbose) print(str(ret))
  return(ret)
}




#' get information of .mat file objects
#' as a data frame
matInfo <- function(matfile) {
  matfilepath <- paste(project.extdata, matfile, sep = "/")
  ld <- o.loader(matfilepath)
  if (typeof(ld$size) == 'list') {
    sz <- paste(sapply(ld$size, `[[`, 1), sapply(ld$size, `[[`, 2), sep = "x")
  } else {
    sz <- paste(sapply(ld$size, `[[`, 1), collapse = "x")
  }
  # sz <- paste(sapply(ld$size, `[[`, 1), sapply(ld$size, `[[`, 2), sep = "x")
  df <- data.frame(name = ld$name,
                   size = sz,
                   bytes = ld$bytes,
                   class = ld$class,
                   stringsAsFactors = FALSE)
  df <- df[order(df[, 1]), ]
  rownames(df) <- NULL
  return(df)
}




savetoRda <- function(..., file, folder = project.data) {
  rdafileLong <- paste(folder, file, sep = "/")
  save(..., file = rdafileLong)
}


o.loader <- OctaveFunction("
function [struct] = readMatfile(mfile)
  load(mfile)
  clear mfile;
  struct = whos();
end
")



#' split a 1-D matrix in a 2-D matrix by means of the # columns
split.matrix <- function(mat, cols) {
  rows <- length(mat) / cols
  df.list <- split(as.data.frame(mat, stringsAsFactors = FALSE),
                   rep(1:cols, each = rows))
  merged <- do.call("cbind", df.list)           # join the data frames in list
  names(merged) <- as.matrix(merged[1, ])       # extract variable names
  merged <- merged[-1, ]                        # remove row 1
  rownames(merged) <- 1:nrow(merged)                           # reset row numbers
  merged <- as.matrix(merged)
  return(merged)
}


process.matlab.object <- function(mf, matList, rdaFile) {
  # create new environment
  e <- new.env()

  toSave <- NULL
  rdf <- data.frame()

  for (item in mf$name) {
    # cat(item, "\t")

    kls <- as.character(mf[mf$name == item, "class"]) # get object class
    size <- as.character(mf[mf$name == item,][1, 'size'])
    dim <- as.integer(unlist(strsplit(size, "x")))

    if (kls == "cell") {
      assign(item, as.matrix(unlist(matList[[item]])))  # unlist if cell

      if ( (dim[2] >=2) & (dim[1] >= 2) ) {
        # stop("more than one column ...")
        cols <- min(dim[2], dim[1])
        # cat("\tcell cols:", cols, "\t")
        cell <- split.matrix(get(item), cols)
        assign(item, cell)
      }
    } else if (kls == "char") {
        assign(item, as.character(unlist(matList[[item]])))
    } else if (kls == "double") {
        assign(item, matList[[item]])
    } else if (kls == "struct") {
        # cat("struct <--")
        # assign(item, unlist(matList[[item]]))
        assign(item, matList[[item]][,,1])
    } else {
        assign(item, matList[[item]])

    }
    # get the object from the string
    obj <- get(item)
    dims <- ifelse(kls == "char", "NULL", paste(dim(obj), collapse = "x"))
    len <- ifelse(kls == "char", nchar(obj), length(obj))
    kls <- class(obj)
    to <- typeof(obj)
    stir <- capture.output(str(obj))[1]

    item.props <- list(Rname = item, dims = dims, length = len, Rclass = kls,
                       typeof = to, value = stir)
    item.whole <- list(values = obj, name = item, properties = item.props)

    ## return(item.whole)
    ##################################################
    ## whole <- process.matlab.object(item)
    whole <- item.whole
    assign(whole$name, whole$values)
    row <- whole$properties
    # cat(row$Rclass, row$typeof)
    row.df <- data.frame(row, stringsAsFactors = FALSE)
    rdf <- rbind(row.df, rdf)
    # cat("\n")
    toSave <- c(toSave, whole$name)
  }
  # save the R objects to .rda file
  savetoRda(list = toSave, file = rdaFile, envir = parent.env(e))

  #rBlock <- list(rdf = rdf, toSave = toSave)
  return(rdf)
}


#' main function that convert .mat files to .rda files
#'
mat2rda <- function(matfile) {

  # provide the Matlab file name
  matfilepath <- paste(project.extdata, matfile, sep = "/")

  # stop if Matlab file does not exist
  if (!file.exists(matfilepath)) stop("file does not exist ...")

  # get info from the Matlab file using RcppOctave
  mf <- matInfo(matfile)
  print(mf)

  # read the Matlab file from R
  matList <- readMatfile(matfile)   # load the Matlab file results to a list
  # print(names(matList))

  # iterate through objects in data frame returned from RcppOctave
  rdaFile <- paste(unlist(strsplit(matfile, "\\."))[1], "rda",sep = ".")

  # process the matlab file objects to convert them to R
  rdf <- process.matlab.object(mf, matList, rdaFile)

  # get a data frame with information of converted R objects
  #rdf <- rObject$rdf

  # R objects to save in the .rda file
  #toSave <- rObject$toSave

  rdf <- rdf[order(rdf[, 1]), ]     # sort data frame
  rownames(rdf) <- 1:nrow(rdf)      # reset the row names
  print(rdf)                        # show data frame of objects in Matlab and R



}
