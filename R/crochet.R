#' crochet (first line)
#'
#' crochet (details here)
#' one
#' two
#' some more
#'
#' @param rnw the rnw file
#' @param df a data frame
#' @param folder for the results (i.e feedback files)
#' @param exp1 expression to be evaluated within the data frame
#' @param exp2 expression to be evaluated within the function's environment
#' @param fnv File name variable (a variable in data frame, df, to use as names of the resulting pdf files)
#' @param subset subset
#' @param ... passed to knit2pdf
#' @return nothing useful, used for side fx
#' @export crochet
crochet <- function(rnw, df, folder, fnv, exp1 = NULL, exp2 = NULL, subset, III = NULL, ...){
  .D <- df # maybe there will be a possibility to use ct (control table) to transform df
  if(!missing(subset)) {
    s <- substitute(subset)
    .D <- .D[eval(s, .D),]
  }
  if(!is.null(exp1)) eval(subsitute(exp1))
  PR <- "%"
  # if(missing(fnv)) fnv <- as.name(iii) else fnv <- substitute(fnv)
  if(file.exists(folder) & !file.info(folder)$isdir) stop("Can't create directory ", folder, " - a file with that name exists.")
  if(!file.exists(folder)) dir.create(folder)
  if(!is.null(exp2)) eval(substitute(exp2), envir=environment())

  if(is.null(III)) III <- seq_len(NROW(.D))
  for(iii in III){
    SLICE <- Diii <- .D[iii,]
    # for(jjj in names(SLICE)) if(is.na(SLICE[[jjj]])) SLICE[[jjj]] <- ""
    SLICE <- as.environment(SLICE)
    parent.env(SLICE) <- environment()
    knit2pdf(rnw, envir = SLICE, ...)
    if(missing(folder)) folder <- "."
    fnv2 <- if(missing(fnv)) iii else eval(substitute(fnv), SLICE)
    renameto <- paste0(folder, "/", fnv2, ".pdf")
    pdfname <- sub("\\.Rnw$", "\\.pdf", rnw)
    file.rename(pdfname, renameto)
  }

}

