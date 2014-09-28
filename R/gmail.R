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
#' @import txtutils
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
        cells <- sapply(strsplit(to, "@"), function(x) x[2]) %in% cell.ext[, 2]
        loc <- paste0(find.package("gmailR"), "/data")
        if (is.null(from)) {
            from <- username
        }

        #     if (!is.null(attachment) &&
        #         (length(strsplit(unlist(attachment), "\\", fixed=TRUE))) == 1 &&
        #         length(strsplit(unlist(attachment), "/", fixed=TRUE)) == 1)) {
        #         attachment <- paste0(getwd(), "/", attachment)
        #     }


        if (is.vector(attachment)) {
            attachment = list(attachment)
        }

        if(!is.null(attachment)) {
            attachment = lapply(attachment, addPwd)
        }


        nRcpt = length(to)
        nAttach = length(attachment)
        if(nAttach == 1 && nRcpt > 1) {
            attachment = rep(attachment, nRcpt)
        }
        nAttach = length(attachment)
        if(nAttach != nRcpt) {
            stop("Number of attachments not equal to number of recepients!")
        }

        atts = attachment
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


addPwd = function(files = character())  {
    if(is.list(files)) files = unlist(files)
    idx = grepl("/", files)
    idx = which(!idx)
    if(length(idx) > 0) {
        message("Some files are relative to PWD, need to prepend PWD..")
        wd = getwd()
        for(i in idx) {
            files[i] = file.path(wd, files[i])
        }
    }

    idx = grepl("[*~]", files)
    idx = which(idx)
    if(length(idx) > 0) {
        newFiles = list()
        message("It looks like you used globbing, I will expand them..")
        for(i in 1:length(files)) {
            if(i %in% idx) {
                newItem = Sys.glob(files[i])
                newFiles[[length(newFiles) + 1]] = newItem
            }
            else {
                newFiles[[length(newFiles) + 1]] = files[i]
            }
        }
        files= unlist(newFiles)
    }
    files
}

# require(txtutils)
# files = c("/tmp/*.txt", "~/Downloads/PhD-dag 2014_programme_PhD Day_def.pdf", "/Users/kaiyin/.autoGmailrc", "tmp.txt")
# addPwd(files)
