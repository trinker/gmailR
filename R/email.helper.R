email.helper <-
function(to, from, subject, username, password, server, message, attachment,
  confirmBeforeSend){
#   if (!is.null(attachment)) {
#     if (attachment == FALSE) attachment <- NULL
#   }
  if (!is.list(to) | !is.list(from)) stop("'to' and 'from' must be lists")
  if (length(from) > 1) stop("'from' must have length 1")
  if (length(to) > 1) stop("'send.email' currently only supports one recipient e-mail address")
  # if (length(attachment) > 1) stop("'send.email' can currently send only one attachment")
  if (length(message) > 1){
    stop("'message' must be of length 1")
    message <- paste(message, collapse="\\n\\n")
  }
  if (is.null(names(to))) names(to) <- to
  if (is.null(names(from))) names(from) <- from
  if (!is.null(attachment)) {
      for(att in attachment) {
          if (!file.exists(att)) stop(paste("'", att, "' does not exist!", sep=""))
      }
  }
  if (missing(username)) username <- winDialogString("Please enter your e-mail username", "")
  if (missing(password)) password <- winDialogString("Please enter your e-mail password", "")
  rJython <- rJython()
  rJython$exec("import smtplib")
  rJython$exec("import os")
  rJython$exec("from email.MIMEMultipart import MIMEMultipart")
  rJython$exec("from email.MIMEBase import MIMEBase")
  rJython$exec("from email.MIMEText import MIMEText")
  rJython$exec("from email.Utils import COMMASPACE, formatdate")
  rJython$exec("from email import Encoders")
  rJython$exec("import email.utils")
  mail<-c(
  #Email settings
  paste("fromaddr = '", from, "'", sep=""),
  paste("toaddrs  = '", to, "'", sep=""),
  "msg = MIMEMultipart()",
  paste("msg.attach(MIMEText('", message, "','plain','utf-8'))", sep=""),
  paste("msg['From'] = email.utils.formataddr(('", names(from), "', fromaddr))", sep=""),
  paste("msg['To'] = email.utils.formataddr(('", names(to), "', toaddrs))", sep=""),
  paste("msg['Subject'] = '", subject, "'", sep=""))
  if (!is.null(attachment)){
      for(att in attachment) {
        mail <- c(mail,
          paste("f = '", att, "'", sep=""),
         "part=MIMEBase('application', 'octet-stream')",
         "part.set_payload(open(f, 'rb').read())",
         "Encoders.encode_base64(part)",
         "part.add_header('Content-Disposition', 'attachment; filename=\"%s\"' % os.path.basename(f))",
         "msg.attach(part)")
      }
  }

  mail <- c(mail,
    paste("username = '", username, "'", sep=""),
    paste("password = '", password, "'", sep=""),
    paste("server = smtplib.SMTP('", server, "')", sep=""),
    "server.ehlo()",
    "server.starttls()",
    "server.ehlo()",
    "server.login(username,password)",
    "server.sendmail(fromaddr, toaddrs, msg.as_string())",
    "server.quit()")

  message.details <-
    paste("To:               ", names(to), " (", unlist(to), ")", "\n",
          "From:             ", names(from), " (", unlist(from), ")", "\n",
          "Using server:     ", server, "\n",
          "Subject:          ", subject, "\n",
          "With Attachments: ", attachment, "\n",
          "And the message:\n", message, "\n", sep="")

  if (confirmBeforeSend)
   SEND <- winDialog("yesnocancel", paste("Are you sure you want to send this e-mail to ", unlist(to), "?", sep=""))
   else SEND <- "YES"

  if (SEND %in% "YES"){
    jython.exec(rJython,mail)
    cat(message.details)
  }
  else cat("E-mail Delivery was Canceled by the User")
}
