#' Generate an Email from a Cell Phone Number
#' 
#' Generates an email adress based on cell number nad carrier.
#' 
#' @param cell.number The 10 digits number of the cell you're trying to send a 
#' text to.
#' @param carrier The name of the carrier.  If NULL interactive selection of a 
#' carrier is used.
#' @param omit.old logical.  If TRUE removes all the old Canadian carriers from 
#' the list.
#' @param phone.ext A dictionary of email add ons to extend a phone number 
#' from a particular carrier into an email address.
#' @export
#' @examples
#' \dontrun{
#' cell2email("555-555-5555")
#' }
cell2email <-
function(cell.number, carrier = NULL, omit.old = TRUE, phone.ext = gmailR::cell.ext) {

    cell.number <- gsub("[^\\d]+", "", cell.number, perl=TRUE)

    if(nchar(as.character(cell.number)) != 10) {
        stop("cell.number must have 10 digits")
    }

    if (omit.old) {
        phone.ext <- phone.ext[phone.ext[, 3] != "old_us_can", ]
    }
    if (!is.null(carrier)){
        y <- tolower(phone.ext[, 1]) %in% tolower(carrier)
        if (sum(y) == 0){
            message("\n","Warning: Carrier Not Found!","\n")
            carrier <- NULL
        }
    }
    if (is.null(carrier)) {
        cat("\n","Choose Carrier","\n")
        c2 <- menu(unique(phone.ext[, 1]))  
        y <- phone.ext[, 1] %in% as.character(unique(phone.ext[, 1]))[c2]
    } 

    if (sum(y) > 1) {
        z <- phone.ext[y, ]
        cat("\n","Choose Carrier","\n")
        choice <- menu(paste(z[, 1], z[, 2], z[, 3], sep = "     "))   
    } else {
        choice <- which(y)
    }
    paste0(cell.number, "@", phone.ext[choice, 2])   
}