#' @rdname get_credentials
#' @export
get_credentials_via_readline <- function(prompt="", username=guess_user(), plain_text=FALSE, 
                                         toggle_cache=TRUE) {
  check_interactive()
  
  cat(paste0(prompt, "\n"))
  cat(paste0("User name: ", username, "\n"))
  cat("Password: ")
  if (!plain_text)
    system("stty -echo")
  
  pw <- readline()
  
  if (!plain_text)
    system("stty echo")
  
  return(list(username=username, password=pw))
}

#' @export
#' @rdname list_credentialing_options
is_readline_credentialing_available <- function() {
  if (get_gui()=="X11" && get_display()=="")
    return(TRUE)
  
  return(FALSE)
}

#' @export
#' @rdname list_credentialing_options
#' @section readline:
#' Credentialing via \code{readline} is available when \code{R} is
#' run on a terminal (i.e., command line \code{R}). It is not recommended
#' when running using a GUI because it depends on system commands to 
#' turn off echoing of typed text - which is how it prevents passwords
#' from showing up in the console in plain text. This is a method of 
#' last resort and is only recommended when the GUI-based options are
#' not available.
#' 
#' In the case that a user is running R with a GUI and no other credentialing
#' options are available, then as a last resort the \code{readline} approach
#' can be used with a \code{plain_text} option. This at least prevents a 
#' user's password from persisting in a command history (e.g., if the password
#' were entered when calling a function) but it is exposed in plain text on the
#' console. If this happens a warning message will be printed explaining how
#' a user might go about installing a package to enable veiled password entry.
is_readline_credentialing_recommended <- function() {
  if (is_readline_credentialing_available() &&
      !is_shiny_credentialing_recommended() && 
      !is_rstudioapi_credentialing_recommended() &&
      !is_tcltk_credentialing_recommended())
    return(TRUE)
  return(FALSE)
}