#' @rdname get_credentials
#' @export
get_credentials_via_shiny <- function(prompt="", username=guess_user(), toggle_cache=TRUE) {
  require(shiny)
  require(miniUI)
  
  ui <- miniPage(
    gadgetTitleBar(""),
    miniContentPanel(
      titlePanel(prompt),
      textInput("username", "User name", value=username),
      passwordInput("password", "Password"),
      if (toggle_cache)
        checkboxInput("cache_password", "cache password (less secure)")
    )
  )
  
  server <- function(input, output, session) {
    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      stopApp(list(
        username = isolate(input$username), 
        password = isolate(input$password),
        cache_pw = ifelse(toggle_cache, isolate(input$cache_password), FALSE)
      ))
    })
    
    observeEvent(input$cancel, {
      stopApp(list(
        username = isolate(input$username), 
        password = NULL,
        cache_pw = NULL
      ))
    })
  }
  
  viewer <- paneViewer(300)
  ans <- runGadget(ui, server, viewer=viewer)
  if (is.null(ans[["password"]]))
    stop("Unable to complete request without valid credentials")
  if (!toggle_cache)
    ans["cache_pw"] <- NULL
  return(ans)
}

#' @export
#' @rdname get_credentialing_options
is_shiny_credentialing_available <- function() {
  
  # need to be using some GUI
  if (!(.Platform$GUI %in% c("Rgui", "RStudio"))) {
    return(FALSE)
  }
  
  # need dependent package available
  if (!("shiny" %in% installed.packages()[,1])) {
    message("Shiny-based credentialing could be available if the 'shiny' package were installed.")
    return(FALSE)
  }
  
  if (!("miniUI" %in% installed.packages()[,1])) {
    message("Shiny-based credentialing could be available if the 'miniUI' package were installed.")
    return(FALSE)
  }
  
  return(TRUE)
}

#' @export
#' @rdname get_credentialing_options
#' @section Shiny:
#' Shiny credentialing depends on both the \code{shiny} and \code{miniUI} packages being available.
#' It also requires that \code{R} is being run using a GUI (e.g., not on the command line). Shiny
#' is designated as the recommended option when R is run via RStudio and the necessary dependencies
#' are available. While this may seem odd given that there is an \code{rstudioapi} credentialing
#' option, the \code{shiny} option is superior in that it is the only option to provide an optional
#' cache password checkbox and is also the only option where the user can modify the user name input
#' after the function is called.
is_shiny_credentialing_recommended <- function() {
  if (is_shiny_credentialing_available() && .Platform$GUI=="RStudio")
    return(TRUE)
  return(FALSE)
}