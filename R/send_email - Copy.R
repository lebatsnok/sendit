#' Send e-mail using rJython
#'
#' Send e-mail using rJython
#'
#'
#'
#' @param to recipients name
#' @param from sender's name
#' @param subject subject
#' @param message message
#' @param attachment a file name
#' @param username user name to log in to the SMTP server
#' @param password password to log in to the SMTP server
#' @param server server (defaults to gmail SMTP server)
#' @param confirmBeforeSend ask before sending? defaults to true
#' @param wait Seconds to wait before sending next email
#' @export send_email
send_email <- function(to, from, subject, message, attachment=NULL, username, password,
                       server="smtp.gmail.com:587", confirmBeforeSend=TRUE, wait = 0){
  ### http://r.789695.n4.nabble.com/Email-out-of-R-code-td3530671.html
  ### from http://stackoverflow.com/questions/7802360/email-an-attachment-in-r-with-gmail
  ### (minimally edited)
  ### to: a list object of length 1. Using list("Recipient" ="recip@somewhere.net")       will     send the message to the address but
  ### the name will appear instead of the address.
  ### from: a list object of length 1. Same behavior as 'to'
  ### subject: Character(1) giving the subject line.
  ### message: Character(1) giving the body of the message
  ### attachment: Character(1) giving the location of the attachment
  ### username: character(1) giving the username. If missing and you are using Windows, R     will prompt you for the username.
  ### password: character(1) giving the password. If missing and you are using Windows, R     will prompt you for the password.
  ### server: character(1) giving the smtp server.
  ### confirmBeforeSend: Logical. If True, a dialog box appears seeking confirmation         before sending the e-mail. This is to
  ### prevent me to send multiple updates to a collaborator while I am working interactively.
  start <- Sys.time()
  if (!is.list(to) | !is.list(from)) stop("'to' and 'from' must be lists")
  if (length(from) > 1) stop("'from' must have length 1")
  if (length(to) >  1) stop("'send.email' currently only supports one recipient e-mail     address")
  if (length(attachment) > 1) stop("'send.email' can currently send only one attachment")
  if (length(message) > 1){
    warning("'message' must be of length 1")
    message <- paste(message, collapse="\\n\\n")
  }
  if (is.null(names(to))) names(to) <- to
  if (is.null(names(from))) names(from) <- from
  if (!is.null(attachment)) if (!file.exists(attachment)) stop(paste("'",
                                                                     attachment, "' does not exist!", sep=""))

  if (missing(username)) username <- winDialogString("Please enter your
                                                     e-mail username", "")
  if (missing(password)) password <- winDialogString("Please enter your
                                                     e-mail password", "")

  # require(rJython)
  rJython <- list( exec = py_run_string)

  rJython$exec("import smtplib")
  rJython$exec("import os")
  rJython$exec("from email.mime.multipart import MIMEMultipart")
  rJython$exec("from email.mime.base import MIMEBase")
  rJython$exec("from email.mime.text import MIMEText")
  rJython$exec("from email.utils import COMMASPACE, formatdate")
  rJython$exec("from email import encoders")
  rJython$exec("import email.utils")

  mail<-c(
    #Email settings
    paste("fromaddr = '", from, "'", sep=""),
    paste("toaddrs = '", to, "'", sep=""),
    "msg = MIMEMultipart()",
    paste("msg.attach(MIMEText('''", message, "'''))", sep=""),
    paste("msg['From'] = email.utils.formataddr(('", names(from), "',
          fromaddr))", sep=""),
    paste("msg['To'] = email.utils.formataddr(('", names(to), "',
          toaddrs))", sep=""),
    paste("msg['Subject'] = '", subject, "'", sep=""))

  if (!is.null(attachment)){
    mail <- c(mail,
              paste("f = '", attachment, "'", sep=""),
              "part=MIMEBase('application', 'octet-stream')",
              "part.set_payload(open(f, 'rb').read())",
              "encoders.encode_base64(part)",
              "part.add_header('Content-Disposition', 'attachment; filename=\"%s\"' % os.path.basename(f))",
              "msg.attach(part)")
  }

  #SMTP server credentials
  mail <- c(mail,
            paste("username = '", username, "'", sep=""),
            paste("password = '", password, "'", sep=""),

            #Set SMTP server and send email, e.g., google mail SMTP server
            paste("server = smtplib.SMTP('", server, "')", sep=""),
            "server.ehlo()",
            "server.starttls()",
            "server.ehlo()",
            "server.login(username,password)",
            "server.sendmail(fromaddr, toaddrs, msg.as_string())",
            "server.quit()")

  message.details <-
    paste("To: ", names(to), " (", unlist(to), ")", "\n",
          "From: ", names(from), " (", unlist(from), ")",
          "\n",
          "Using server: ", server, "\n",
          "Subject: ", subject, "\n",
          "With Attachments: ", attachment, "\n",
          "And the message:\n", message, "\n", sep="")

  if (confirmBeforeSend)
    SEND <- winDialog("yesnocancel", paste("Are you sure you want to send
                                           this e-mail to ", unlist(to), "?", sep=""))
    else SEND <- "YES"

    if (SEND %in% "YES"){
       tmp <- tempfile()
       writeLines(mail, tmp)
       py_run_file(tmp)
       cat(message.details)
       }
     else cat("E-mail Delivery was Canceled by the User")
     if(is.numeric(wait) & !is.na(wait)){
       now <- Sys.time()
       tdiff <- difftime(now, start, units="secs")
       if(tdiff < wait) {
         towait <- wait - tdiff
         cat("Waiting", towait, " s\n")
         Sys.sleep(towait)
       }
     }
     invisible(mail)
}

