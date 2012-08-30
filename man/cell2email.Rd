\name{cell2email}
\alias{cell2email}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a Cell Phone Number to an Email Address
}
\description{
Convert a cell number to an email address. 
}
\usage{
cell2email(cell.number, carrier, omit.old = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cell.number}{
10 digit cell phone number
}
  \item{carrier}{
Cell phone carrier
}
  \item{omit.old}{
logical.  Should the old_us_can of the last column of cell.ext data set be used.
}
}
\examples{
cell2email(5555555555, "Sprint")
cell2email(5555555555, "seprint")
}
