\name{gmail}
\alias{gmail}
\title{Email With Attachments}
\usage{
  gmail(to, password, subject = "R message",
    message = "EOM", from = NULL, attachment = NULL,
    server = "smtp.gmail.com:587", username = NULL,
    confirmBeforeSend = FALSE, clear.username = FALSE)
}
\arguments{
  \item{to}{The recipient's email address.}

  \item{password}{Yor password.}

  \item{subject}{The message (default is \code{R
  message}).}

  \item{message}{The subject line (default is \code{EOM}).}

  \item{from}{Your email address (gmailR will try to store
  this information for you).}

  \item{attachment}{Path to an attachment you wish to
  include.}

  \item{server}{email server.}

  \item{username}{Your email address (gmailR will try to
  store this information for you).}

  \item{confirmBeforeSend}{logical.  If TRUE gmail will
  confirm before sending.}

  \item{clear.username}{logical.  If TRUE clears the stored
  username.}
}
\description{
  Send emails (including gmails) with attachments from
  within R.
}
\examples{
\dontrun{
gmail(to=c("bob@gmail.com", "janr@hotmail.com"), password = "password",
    attachment="path/to/file.pdf")
gmail(to=cell2email(5555555555, "sprint"), password = "password")
}
}
\author{
  Tyler Rinker<tyler.rinker@gmail.com> and Daniel Malter
}
\references{
  \url{http://r.789695.n4.nabble.com/Email-out-of-R-code-td3530671.html}
}

