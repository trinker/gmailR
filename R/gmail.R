#' Email With Attachments
#'
#' Send emails (including gmails) with attachments from within R.
#'
#' @param to The recipient's email address.
#' @param password Yor password.
#' @param subject The message (default is \code{R message}).
#' @param message The subject line (default is \code{EOM}).
#' @param from Your email address (gmailR will try to store this information for you).
#' @param attachment Path to an attachment you wish to include.
#' @param server email server.
#' @param username Your email address (gmailR will try to store this information for you).
#' @param confirmBeforeSend logical.  If TRUE gmail will confirm before sending.
#' @param clear.username logical.  If TRUE clears the stored username.
#' @author Tyler Rinker<tyler.rinker@@gmail.com> and Daniel Malter
#' @references \url{http://r.789695.n4.nabble.com/Email-out-of-R-code-td3530671.html}
#' @export
#' @import rJython rJava rjson
#' @examples
#' \dontrun{
#' gmail(to=c("bob@@gmail.com", "janr@@hotmail.com"), password = "password", 
#'     attachment="path/to/file.pdf")
#' gmail(to=cell2email(5555555555, "sprint"), password = "password")
#' }
gmail <-
function(to, password, subject="R message", message="EOM", from=NULL, 
    attachment=NULL, server="smtp.gmail.com:587", username=NULL, 
    confirmBeforeSend=FALSE, clear.username = FALSE){
    data(UNAME)
    cells <- sapply(strsplit(to, "@"), function(x) x[2]) %in% cell.ext[, 2]
    loc <- paste0(find.package("gmailR"), "/data")
    if ((is.null(username) & is.na(UNAME) & !exists(".UNAME", 
        envir = .GlobalEnv)) | clear.username) {
        cat("\n","Enter gmail Username","\n")
        UNAME <- scan(n=1,what = character(0), quiet=T)
        .UNAME <<- UNAME 
        unlink(paste0(loc, "/UNAME.rda"), recursive = TRUE, 
            force = FALSE)
        save(UNAME, file = paste0(loc, "/UNAME.rda"))
    }     
    if (exists(".UNAME", envir = .GlobalEnv)) {
        if (is.null(username)) {
            username <- .UNAME
        } else {
            username <- UNAME
        }   
    }
    if (is.null(username)) {
        username <- UNAME
    }
    if (is.null(from)) {
        from <- username
    } 
    if (!is.null(attachment) && 
        (length(unlist(strsplit(attachment, "\\", fixed=TRUE))) == 1 &
        length(unlist(strsplit(attachment, "/", fixed=TRUE))) == 1)) {
        attachment <- paste0(getwd(), "/", attachment)
    }
    atts <- rep(attachment, length(to))
    atts <- lapply(atts, c)
    atts[cells] <- FALSE
    lapply(seq_along(to),
        function (i){
            email.helper(to=list(to[i]), 
                from = list(from),
                subject = subject,
                message = message, 
                attachment = atts[[i]],
                username = username, 
                password = password, 
                server = server, 
                confirmBeforeSend = confirmBeforeSend)
        }
    )
}