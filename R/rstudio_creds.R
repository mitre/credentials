#' @rdname get_credentials
#' @export
get_credentials_via_rstudioapi <- function(prompt="", username=guess_user()) {
  prompt <- paste0(prompt, " (user='", username, "')")
  pw <- rstudioapi::askForPassword(prompt)
  return(list(username=username, password=pw))
}

#' @export
#' @rdname get_credentialing_options
is_rstudioapi_credentialing_available <- function() {
  # I actually think the 1st or 2nd+3rd criteria are sufficient, but just in case
  if (.Platform$GUI=="RStudio" && ("rstudioapi" %in% installed.packages()[,1]) &&
      rstudioapi::isAvailable())
    return(TRUE)

  return(FALSE)
}

#' @export
#' @rdname get_credentialing_options
#' @section rstudioapi:
#' Credentialing via the \code{rstudioapi} package is available when \code{R} is
#' run using the RStudio IDE and not otherwise. It is the preferred approach if
#' it is available and the dependencies required for \code{shiny} credentialing
#' are not installed.
is_rstudioapi_credentialing_recommended <- function() {
  if (is_rstudioapi_credentialing_available() && 
      !is_shiny_credentialing_available())
    return(TRUE)
  return(FALSE)
}