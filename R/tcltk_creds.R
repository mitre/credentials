#' @rdname get_credentials
#' @export
get_credentials_via_tcltk <- function(prompt="", username=guess_user(), toggle_cache=TRUE) {
  check_interactive()
  
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
#' @rdname list_credentialing_options
is_tcltk_credentialing_available <- function() {
  if (has_gui() && ("tcltk" %in% installed.packages()[,1]) &&  
      capabilities("tcltk") && !is_rstudio_server())
    return(TRUE)
  
  return(FALSE)
}

#' @export
#' @rdname list_credentialing_options
#' @section tcltk:
#' Credentialing via \code{tcltk} is available when \code{R} \code{tcltk} is available. 
#' This means that \code{R} was compiled with \code{tcltk} (see \code{?capabilities})
#' and the \code{tcltk} package is installed. One additional special check is that R
#' is not running under RStudio Server. This special case is needed because RStudio Server
#' can pass the first checks, but \code{tcltk} fails to pop up a window on top of the
#' web browser serving out RStudio in that case. Checking for a display is not sufficient
#' because R GUI reports no display but is in principle able to use \code{tcltk}. This method 
#' is recommended as an option when both \code{shiny} and \code{rstudioapi} approaches are 
#' unavailable.
is_tcltk_credentialing_recommended <- function() {
  if (is_tcltk_credentialing_available() &&
      !is_shiny_credentialing_recommended() && 
      !is_rstudioapi_credentialing_recommended())
    return(TRUE)
  return(FALSE)
}