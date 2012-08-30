email <-
function(to, subject="R message", message="EOM", attachment=NULL,  
    server="smtp.gmail.com:587", from=FROM, username=UNAME, password=PWORD, 
    confirmBeforeSend=FALSE){
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
