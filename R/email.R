email <-
function(to, password, subject="R message", message="EOM", from=NULL, 
    attachment=NULL, server="smtp.gmail.com:587", username=UNAME, 
    confirmBeforeSend=FALSE, clear.username = FALSE){
    data(UNAME)
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
    if (is.null(from)) {
        from <- username
    } 
    lapply(to,
        function (x){
            email.helper(to=list(x), 
                from = list(from),
                subject = subject,
                message = message, 
                attachment = attachment,
                username = username, 
                password = password, 
                server = server, 
                confirmBeforeSend = confirmBeforeSend)
        }
    )
}
