# NOTE: these helper functions exist for two reasons. The first and more important is so that
#       the package can be tested mocking environments (it is easy to mock get_gui, it is not
#       easy to mock .Platform$GUI). The second is for code readability. 
get_gui <- function() {
  return(.Platform$GUI)
}

has_gui <- function() {
  return(get_gui()!="X11")
}

get_display <- function() {
  return(Sys.getenv("Display"))
}

rstudioapi_is_available <- function() {
  return(("rstudioapi" %in% installed.packages()[,1]) &&
           rstudioapi::isAvailable())
}

is_rstudio_desktop <- function() {
  if (!rstudioapi_is_available())
    return(FALSE)
  
  if (rstudioapi::versionInfo()$mode=="desktop")
    return(TRUE)
  
  return(FALSE)
}

is_rstudio_server <- function() {
  if (!rstudioapi_is_available())
    return(FALSE)
  
  if (rstudioapi::versionInfo()$mode=="server")
    return(TRUE)
  
  return(FALSE)
}

check_interactive <- function() {
  if (!interactive())
    stop("Cannot ask for credentials in a non-interactive R session!")
}