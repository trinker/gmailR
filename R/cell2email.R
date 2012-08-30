cell2email <-
function(cell.number, carrier, omit.old = TRUE) {
    if(nchar(as.character(cell.number)) != 10) {
        stop("cell.number must have 10 digits")
    }
    x <- cell.ext
    if (omit.old) {
        x <- x[x[, 3] != "old_us_can", ]
    }
    y <- tolower(x[, 1]) %in% tolower(carrier)
    if (sum(y) == 0) {
        cat("\n","Warning: Carrier Not Found!","\n")
        cat("\n","Choose Carrier","\n")
        c2 <- menu(unique(x[, 1]))   
        y <- tolower(x[, 1]) %in% tolower(x[c2, 1])
    }
    if (sum(y) > 1) {
        z <- x[y, ]
        cat("\n","Choose Carrier","\n")
        choice <- menu(paste(z[, 1], z[, 2], z[, 3], sep = "     "))   
    } else {
        choice <- which(y)
    }
    paste0(cell.number, "@", x[choice, 2])   
}