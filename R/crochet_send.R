#' Crochet the messages and send e-mails with attachments.
#'
#' Crochet the messages and send e-mails with attachments
#'
#' A nice way to store your credentials for the DETAILS argument is in a file
#' accessible only to yourself: preferably in another folder (e.g. '~/cred'),
#' not your R working directory . Set appropriate access rights to that folder.
#' This way you will not accidentally copy your password and send it to someone
#' over e-mail or post it to github or #' facebook. The file with credential should
#' be in dcf format (\link{read.dcf}), having three lines: server, username, and
#' password; tags and values separated by colon.
#' The function only works with attachments.
#'
#' @param D a data frame
#' @param MSG message to be pre-treated by crochet_txt (e.g. adding personalized information to standard text)
#' @param TO character (name of the variable in D with e-mails)
#' @param ATT attachment: character vector with 3 components (these will be pasted together, the first typically refers to folder and should in this case end in "/"; the second refers to a variable name in data frame D; the third is a file name extension ) e.g. c("feedback/", "ID", "pdf) becomes feedback/001.pdf for the first case
#' @param SUB subject line for e-mail
#' @param FAKEMAIL an email address that will be used instead of the original address (for trying out if everything works correctly before sending it to the real addressees)
#' @param SUBSET a numeric vector (row numbers in D)
#' @param FUN function to be used for sending mail (defaults to send_email; one can use custom functions that can take the required parameters; using "list" for FUN is another way of trying it out before sending it to real addressees)
#' @param DETAILS a DCF filename (character(1), e.g., "~/secrets/mycredentials.dcf") or a list with three named components: server, username, password e.g. list(server="xxx.yyy.zz:123", username="myself@blah.zz", password="topsecret***"). A dcf file is an ordinary plain text file, so on windows, it might have a txt extension, and on linux, it is ok to have no extension.
#' @param browse get inside the function before end
#' @details could use something like crochet_send(data, "Hi **name**", ATT=c("ts/", "ID", "pdf"), SUB = "Subject line", FAKEMAIL = "my.own@gmail.com", DETAILS = list(server="xxx.yyy.zzz:123", username="myself", password="topsecret123") || "~/my/cred/mycredentials"), ,
#'
#'
#' @return a matrix with 3 columns: (1) e-mail address, (2) attachment, (3) result: "sent" or "not sent"
#' @export crochet_send
crochet_send <- function(D, MSG, TO, ATT = c("folder/", "ID", "pdf"), SUB = "Subject line",
                         FAKEMAIL = NULL, SUBSET = TRUE,
                         FUN = send_email, DETAILS , browse=FALSE){
  if(is.character(DETAILS)) DETAILS <- as.list(read.dcf(DETAILS)[1,])
  if(is.list(DETAILS)) {
    PWDD <- DETAILS$password
    USRD <- DETAILS$username
    SRVR <- DETAILS$server
  } else stop("need server details and username to send something")
  D <- D[SUBSET,]
  MSGS <- crochet_txt(MSG, D)
  DTO <- D[[TO]]
  if(!is.null(FAKEMAIL)) D[[TO]] <- FAKEMAIL
  TOS <- as.character(D[[TO]])
  any12send2 <- !is.na(DTO) & !is.na(D[[ATT[2]]]) & DTO!=""
  ATTS <- paste0(ATT[1], D[[ATT[2]]], ".", ATT[3])
  RES <- list()
  for(iii in seq_along(TOS)){
    if(any12send2[iii]) {
      REZ <- try(FUN(to = list(TOS[iii]), from=list(USRD), message=MSGS[iii],
                     attachment = ATTS[iii], subject = SUB, username = USRD, password = PWDD,
                     server = SRVR, confirmBeforeSend = FALSE,
                     wait = 5))
    } else REZ <- "not sent"
    if(inherits(REZ, "try-error")) REZ <- "not sent"
    if(!identical("not sent", REZ)) REZ <- "sent"
    REZ <- c(TOS[iii], ATTS[iii], REZ)
    RES[[iii]] <- REZ
  }
  RES <- do.call(rbind, RES)
  rownames(RES) <- DTO
  if(browse) browser()
  RES
}
