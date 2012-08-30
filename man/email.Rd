\name{email}
\alias{email}
\title{
Send gmail with Attachments in R
}
\description{
Enables a user to send an email or text in R.  Useful to alert the user that a simulation is complete.
}
\usage{
email(to, password, subject="R message", message="EOM", from=NULL, 
    attachment=NULL, server="smtp.gmail.com:587", username=NULL, 
    confirmBeforeSend=FALSE, clear.username = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{to}{
recipients
}
  \item{password}{
email password:  reads from a saved director if left blank
}
  \item{subject}{
subject line: defaults to R message
}
  \item{message}{
optional message: defaults to EOM
}
  \item{from}{
who the email is from: reads from a saved directory if NULL
}
  \item{attachment}{
The path to an attachment (suggest results): can only send one attachment currently
}
  \item{server}{
email server
}
  \item{username}{
the name you log into emails with: reads from a saved directory if NULL
}
  \item{confirmBeforeSend}{
logical.  If TRUE will ask for permission before sending
}
  \item{clear.username}{
logical.  If TRUE will allow the user to reset the username stored in the package
}
}
\references{
Daniel Malter via: http://r.789695.n4.nabble.com/Email-out-of-R-code-td3530671.html
}
\examples{
#blank for now
}

