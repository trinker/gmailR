\name{cell2email}
\alias{cell2email}
\title{Generate an Email from a Cell Phone Number}
\usage{
  cell2email(cell.number, carrier = NULL, omit.old = TRUE)
}
\arguments{
  \item{cell.number}{The 10 digits number of the cell
  you're trying to send a text to.}

  \item{carrier}{The name of the carrier.  If NULL
  interactive selection of a carrier is used.}

  \item{omit.old}{logical.  If TRUE removes all the old
  Canadian carriers from the list.}
}
\description{
  Generates an email adress based on cell number nad
  carrier.
}
\examples{
cell2email("555-555-5555")
}

