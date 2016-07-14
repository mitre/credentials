#' Get credentials
#' 
#' This function provides several options for ways in which user credentials (i.e., a
#' user name and password) may be collected in R. The intent here is for use cases where
#' some sort of authentiation is necessary within R, such as database connection credentials.
#' 
#' Generally \code{get_credentials} is a better choice for packages to use than one of the
#' specific credential getters. The order in which options are considered is controllable
#' via the \code{preferences} argument, but there are safeguards adapt to the urser's system.
#' 
#' @param prompt Character. A prompt justifying why a user ought to provide a password, or
#'        perhaps indicating what the password is needed for. 
#' @param username Character. The username that will be used in conjunction with the provided
#'        password
#' @param toggle_cache Logical. Used only in the \code{shiny} option, it enables packages to
#'        be friendly about whether users would like to have their passwords cached to avoid reentry
#'        (e.g., if the same system is going to be connected several times) or if they would prefer
#'        to be asked each time. 
#' @param plain_text Logical. Available only for the \code{readline} option only, this allows a password
#'        to be entered in plain text on the console. This is generally a bad and avoidable option.
#'        It exists as a method of last resort for those who don't care about security.
#' @inheritParams list_credentialing_options
#' @export
get_credentials <- function(prompt="", username=guess_user(), toggle_cache=TRUE, 
                            preferences=get_recommended_credentialer()) {
  
  credentialer <- get_first_available_credentialer(preferences=preferences)
  if (credentialer=="none") {
    Warning(generate_no_options_msg())
    response <- readline("Do you want to use a plain text password entry? [y/n] ")
    if (grepl("^y", tolower(response)))
      return(get_credentials_via_readline(prompt=prompt, username=username, plain_text=TRUE))
  }
  
  credentials_func <- paste0("get_credentials_via_", credentialer)
  return(do.call(credentials_func, args=list(prompt=prompt, username=username, toggle_cache=toggle_cache)))
}

#' Determine how Credentials can be Gathered
#' 
#' These functions interrogate the R process to determine what valid modes of asking for credentials
#' might be. For example the shiny app will work with R GUI and RStudio, but is somewhat cumbersome
#' (and maybe entirely impractical) for command line R (especially when operating over SSH).
#' 
#' @param preferences Character. An ordered list of preferred credentialing options. The first
#'        option on the list that is functional on the user's system will be used. Note that in the
#'        event that none of the preferred options is available on a user's system, then all 
#'        available options (per \code{\link{list_credentialing_options}}) will be tried even though
#'        they may not appear on the preferences list.
#' @export
list_credentialing_options <- function() {
  return(c("shiny", "rstudioapi", "readline", "tcltk"))
}

#' @export
#' @rdname list_credentialing_options
get_recommended_credentialer <- function() {
  credentialers <- list_credentialing_options()
  
  for (credr in credentialers) {
    func <- paste0("is_", credr, "_credentialing_recommended")
    if (do.call(func, arg=list()))
      return(credr)
  }
  
  return("none")
}

#' @export
#' @rdname list_credentialing_options
get_first_available_credentialer <- function(preferences) {
  default_preferences <- list_credentialing_options()
  if (missing(preferences)) {
    preferences <- default_preferences
  } else {
    # always append any omitted preferences to the end in case preferred 
    # credentialing options are unavailable
    preferences <- c(preferences, 
                     default_preferences[!(default_preferences %in% preferences)])
  }
    
  for (pref in preferences) {
    func <- paste0("is_", pref, "_credentialing_available")
    if (!exists(func, mode="function")) {
      warning(paste0("'", pref, "' is an unrecognized credentialing option."))
      next()
    }
    if (do.call(func, arg=list()))
      return(pref)
  }
  
  return("none")
}

generate_no_options_msg <- function() {
  paste0("None of the credentialing options are workable on your system. This is easily resolved ",
         "by intsalling the 'shiny' and 'miniUI' packages if working with RStudio or the R GUI ",
         "or by installing the 'tcltk' package (and possibly recompiling R with tcltk if necessary) ",
         "if working with either R GUI or RStudio desktop version. If you see this message while ",
         "using R on the command line please report a bug since the readline option ought to work ",
         "in that case.")
}

