email <-
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
    if (is.null(from)) {
        from <- username
    } 
    if (!is.null(attachment) & 
        (length(unlist(strsplit(attachment, "\\", fixed=TRUE))) == 1|
        length(unlist(strsplit(attachment, "/", fixed=TRUE))) == 1)) {
        attachment <- paste0(getwd(), "/", attachment)
    }
    atts <- rep(attachment, length(to))
    atts <- lapply(atts, c)
    atts[cells] <- FALSE
    lapply(seq_along(to),
        function (i){
            email.helper(to=list(to(i)), 
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
