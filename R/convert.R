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


process.matlab.object <- function(item) {
  kls <- as.character(mf[mf$name == item, "class"]) # get object class
  size <- as.character(mf[mf$name == item,][1, 'size'])
  dim <- as.integer(unlist(strsplit(size, "x")))

  if (kls == "cell") {
    assign(item, as.matrix(unlist(matList[[item]])))  # unlist if cell

    if ( (dim[2] >=2) & (dim[1] >= 2) ) {
      # stop("more than one column ...")
      cols <- min(dim[2], dim[1])
      cat("\tcell cols:", cols, "\t")
      cell <- split.matrix(get(item), cols)
      assign(item, cell)
    }

  } else if (kls == "char") {
    assign(item, as.character(unlist(matList[[item]])))
  } else if (kls == "double") {
    assign(item, matList[[item]])
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
  return(item.whole)
}

# geneinfo.tmp <- as.matrix(unlist(matList$geneinfo))
# res <- split.matrix(geneinfo.tmp, 2)
# dim(res)
# class(res)


# main
rdaFile <- paste(unlist(strsplit(matfile, "\\."))[1], "rda",sep = ".")
toSave <- NULL
rdf <- data.frame()

for (item in mf$name) {
  # cat(item, class(item), "\n")
  cat(item, "\t")
  #print(process.matlab.object(item)$Rname)

  # row <- process.matlab.object(item)
  whole <- process.matlab.object(item)
  assign(whole$name, whole$values)
  row <- whole$properties
  cat(row$Rclass, row$typeof)
  row.df <- data.frame(row, stringsAsFactors = FALSE)
  rdf <- rbind(row.df, rdf)
  cat("\n")
  toSave <- c(toSave, whole$name)
}
rdf <- rdf[order(rdf[, 1]), ]
rownames(rdf) <- 1:nrow(rdf)
print(rdf)
savetoRda(list = toSave, file = rdaFile)
