#' Mboxplot
#'
#' @param x - a variable
#' @param Q  (boolean) show quantiles by colors
#' @param TXT - text at the left side
#' @param BOX - box type "O" or "="
#' @param AT - passed to axis(1)
#' @param jit - jit points? (T / F)
#' @param XLIM - limits for x axis
#' @param MAR  - margins (left one is large for text)
#' @param MTEXT_CEX - cex for label
#' @param browse
#'
#' @return a data frame
#' @export
mboxplot <- function(x, Q = TRUE, TXT = "", BOX = "=", AT = NULL, jit = FALSE, XLIM = NULL, MAR = c(2,8,0,0), MTEXT_CEX = 1, browse=FALSE){
  if(is.null(XLIM)) XLIM <- c(floor(min(x, na.rm=TRUE)), ceiling(max(x, na.rm=TRUE)))
  par(mar = MAR)
  plot(NA, ylim = c(0,1), xlim=XLIM, axes=FALSE, xlab="", ylab="")
  if(jit) x<-jitter(x)
  y <- runif(length(x))
  mtext(TXT, side=2, adj=1, las=2, cex=MTEXT_CEX)
  if(Q){
    quant <- quantile(x, c(0.125, .25, .75, .875), na.rm=TRUE)
    innercol<-"deepskyblue"; outercol<-"light blue"
    rect(quant[1], 0, quant[4], 1, col=outercol, border=outercol)
    rect(quant[2], 0, quant[3], 1, col=innercol, border=innercol)
  }
  if(!is.null(AT)) axis(1, at=AT) else axis(1)
  if(BOX=="=") abline(h=c(0,1), col="grey", lwd=2)
  if(BOX=="o") box()
  points(x, y, pch=".", cex=5)

  if(browse) browser()
  invisible(data.frame(x=x,y=y))
}
