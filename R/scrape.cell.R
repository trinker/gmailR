scrape.cell <- function() {
    library(RCurl)
    library(XML)
    url   <- "http://www.emailtextmessages.com/"
    doc   <- htmlTreeParse(url, useInternalNodes=TRUE)
    x     <- sapply(getNodeSet(doc, "//li") [1:217], xmlValue)
    y     <- sapply(getNodeSet(doc, "//h3"), xmlValue)
    z     <- rep(c("us_can", "inter", "old_us_can"), 
              c(diff(c(0, 72, 152, 217))))
    a <- data.frame(carrier = y, ext = x, loc = z)
    a <- a[substring(a$ext, 1, nchar("10digit")) == "10digit", ]
    a$ext <- as.character(a$ext)
    b <- strsplit(a$ext, "@")
    a$ext <- gsub("^\\s+|\\s+$", "", sapply(b, function(x) x[length(x)]))
    a <- a[!duplicated(a[, 1:2]), ]
    rownames(a) <- NULL
    return(a)
}