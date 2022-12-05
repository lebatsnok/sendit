#' Crochet-txt
#'
#' Crochet-txt
#'
#'
#'
#' @param txt text
#' @param df data frame
#' @param exp1 expression 1
#' @param exp2 expression 2
#' @param subset subset
#' @param name name
#' @param attrs NULL
#' @param PS **
#' @param na.destroy Print empty string instead of NA
#' @param browse FALSE
#' @export crochet_txt
crochet_txt <- function(txt, df, exp1=NULL, exp2=NULL, subset, name=NULL, attrs = NULL, PS = "**", na.destroy=FALSE, browse=FALSE){
  D <- df # maybe there will be a possibility to use ct (control table) to transform df
  if(!missing(subset)) {
    s <- substitute(subset)
    D <- D[eval(s, D),,drop=FALSE]
  }

  OUT <- list()
  for(iii in seq_len(NROW(D))){
    TXT <- txt
    SLICE <- D[iii,,drop=FALSE]
    if(na.destroy) for(jjj in names(SLICE)) if(is.na(SLICE[[jjj]])) SLICE[[jjj]] <- ""
    SLICE <- as.environment(SLICE)
    parent.env(SLICE) <- environment()
    LS <- ls(SLICE)
    LSa <- paste0(PS, LS, PS)
    for(jjj in seq_along(LS)){
      value <- SLICE[[ LS[jjj] ]]
      label <- LSa[jjj]
      TXT <- gsub(label, value, TXT, fixed=TRUE)
    }

    if(!is.null(attrs)){
      for(AAA in attrs){
        attr(TXT, AAA) <- SLICE[[AAA]]
      }
    }
    if(browse) browser()
    nimi <- if(is.null(name)) iii else SLICE[[name]]
    if(as.character(nimi)!="") OUT[[nimi]] <- TXT
  }

  OUT
}
