#' @rdname get_credentials
#' @export
get_credentials_via_tcltk <- function(prompt="", username=guess_user()) {
  require(tcltk)
  prompt <- paste0(prompt, " (user='", username, "')")
  tt <- tktoplevel()
  Password <- tclVar("")
  entry.Password <- tkentry(tt,width="20",textvariable=Password,show="*")
  tkgrid(tklabel(tt, text=prompt))
  tkgrid(entry.Password)
  OnOK <- function()
  {
    tkdestroy(tt)
    Password <<- tclvalue(Password)
  }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkbind(entry.Password, "<Return>",OnOK)
  tkgrid(OK.but)
  tkraise(tt)
  tkfocus(tt) 
  tkwait.window(tt)
  return(list(username=username, password=Password))
}

#' @export
#' @rdname get_credentialing_options
is_tcltk_credentialing_available <- function() {
  if (Sys.getenv("Display")!="" && ("tcltk" %in% installed.packages()[,1]))
    return(TRUE)
  
  return(FALSE)
}

#' @export
#' @rdname get_credentialing_options
#' @section tcltk:
#' Credentialing via \code{tcltk} is available when \code{R} is
#' run with an attached display. This method is recommended as an option
#' when both \code{shiny} and \code{rstudioapi} approaches are unavailable.
is_tcltk_credentialing_recommended <- function() {
  if (is_tcltk_credentialing_available() &&
      !is_shiny_credentialing_recommended() && 
      !is_rstudioapi_credentialing_recommended())
    return(TRUE)
  return(FALSE)
}