#' Get credentials
#' 
#' This function provides several options for ways in which user credentials (i.e., a
#' user name and password) may be collected in R. The intent here is for use cases where
#' some sort of authentiation is necessary within R, such as database connection credentials.
#' 
#' @export
get_credentials <- function(prompt="", username=guess_user(), toggle_cache=TRUE, method) {
}

#' Determine how credentials can be asked for
#' 
#' This function interrogates the R process to determine what valid modes of asking for credentials
#' might be. For example the shiny app will work with R GUI and RStudio, but is somewhat cumbersome
#' (and maybe entirely impractical) for command line R (especially when operating over SSH).
#' 
#' @export
get_credentialing_options <- function(preferences=c("shiny", "rstudio", "readlines", "tcltk")) {
  preferences <- tolower(preferences)
  opts <- data.frame(option=tolower(preferences),
                     available=FALSE)
}

#' @export
#' @rdname get_credentialing_options
get_recommended_credentialer <- function() {
  if (is_shiny_credentialing_recommended())
    return("shiny")
  if (is_rstudioapi_credentialing_recommended())
    return("rstudioapi")
  if (is_tcltk_credentialing_recommended())
    return("tcltk")
  if (is_readline_credentialing_recommended())
    return("readline")
  return("none")
}