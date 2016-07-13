#' Guess at username
#' 
#' Returns the username for the current logged in user in lowercase. 
#' This works for both windows and unix
#' 
#' @export
guess_user <- function(){
  if(.Platform$OS.type=="windows")
    return(tolower(Sys.getenv("USERNAME")))
  else
    return(tolower(Sys.getenv("USER")))
}