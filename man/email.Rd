\name{email}
\alias{email}
\title{
Send gmail with Attachments in R
}
\description{
Enables a user to send an email or text in R.  Useful to alert the user that a simulation is complete.
}
\usage{
email(to, subject = "R message", message = "EOM", attachment = NULL, server = "smtp.gmail.com:587", from = FROM, username = UNAME, password = PWORD, confirmBeforeSend = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{to}{
recipients
}
  \item{subject}{
subject line: defaults to R message
}
  \item{message}{
optional message: defaults to EOM
}
  \item{attachment}{
The path to an attachment (suggest results): can only send one attachment currently
}
  \item{server}{
email server
}
  \item{from}{
who the email is from: reads from a saved director if left blank
}
  \item{username}{
the name you log into emails with: reads from a saved director if left blank
}
  \item{password}{
email password:  reads from a saved director if left blank
}
  \item{confirmBeforeSend}{
logical.  If TRUE will ask for permission before sending
}
}
\references{
Daniel Malter via: http://r.789695.n4.nabble.com/Email-out-of-R-code-td3530671.html
}
\examples{
#blank for now
}

