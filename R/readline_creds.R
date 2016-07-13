#' @rdname get_credentials
#' @export
get_credentials_via_readline <- function(prompt="", username="mpollock") {
  cat(paste0(prompt, "\n"))
  cat(paste0("User name: ", username, "\n"))
  cat("Password: ")
  system("stty -echo")
  pw <- readline()
  system("stty echo")
  return(list(username=username, password=pw))
}

#' @export
#' @rdname get_credentialing_options
is_readline_credentialing_available <- function() {
  if (.Platform$GUI=="X11" && Sys.getenv("Display")=="")
    return(TRUE)
  
  return(FALSE)
}

#' @export
#' @rdname get_credentialing_options
#' @section readline:
#' Credentialing via \code{readline} is available when \code{R} is
#' run on a terminal (i.e., command line \code{R}). It is not available
#' when running using a GUI because it dependson system commands to 
#' turn off echoing of typed text - which is how it prevents passwords
#' from showing up in the console in plain text. This is a method of 
#' last resort and is only recommended when the GUI-based options are
#' not available.
is_readline_credentialing_recommended <- function() {
  if (is_readline_credentialing_recommended() &&
      !is_shiny_credentialing_recommended() && 
      !is_rstudioapi_credentialing_recommended() &&
      !is_tcltk_credentialing_recommended())
    return(TRUE)
  return(FALSE)
}