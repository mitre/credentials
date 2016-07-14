test_that("Recommendation on RStudio Desktop with a display and packages shiny, miniUI, rstudioapi, tcltk installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("RStudio")},
    is_rstudio_server = function() {return(FALSE)},
    is_rstudio_desktop = function() {return(TRUE)},
    get_display = function() {return(":0")},
    installed.packages = function() {return(matrix(c("shiny", "miniUI", "rstudioapi", "tcltk"), ncol=1))},
    rstudioapi_is_available = function() {return(TRUE)},
    
    expect_true(is_shiny_credentialing_recommended()),
    expect_false(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_false(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on RStudio Server with packages shiny, miniUI, rstudioapi, tcltk installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("RStudio")},
    is_rstudio_server = function() {return(TRUE)},
    is_rstudio_desktop = function() {return(FALSE)},
    get_display = function() {return(":0")},
    installed.packages = function() {return(matrix(c("shiny", "miniUI", "rstudioapi", "tcltk"), ncol=1))},
    rstudioapi_is_available = function() {return(TRUE)},
    
    expect_true(is_shiny_credentialing_recommended()),
    expect_false(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_false(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on R GUI with packages shiny, miniUI, rstudioapi, tcltk installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("Rgui")},
    is_rstudio_server = function() {return(FALSE)},
    is_rstudio_desktop = function() {return(FALSE)},
    get_display = function() {return("")},
    installed.packages = function() {return(matrix(c("shiny", "miniUI", "rstudioapi", "tcltk"), ncol=1))},
    rstudioapi_is_available = function() {return(FALSE)},
    
    expect_false(is_shiny_credentialing_recommended()),
    expect_false(is_rstudioapi_credentialing_recommended()),
    expect_true(is_tcltk_credentialing_recommended()),
    expect_false(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on X11 with packages shiny, miniUI, rstudioapi, tcltk installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("X11")},
    is_rstudio_server = function() {return(FALSE)},
    is_rstudio_desktop = function() {return(FALSE)},
    get_display = function() {return("")},
    installed.packages = function() {return(matrix(c("shiny", "miniUI", "rstudioapi", "tcltk"), ncol=1))},
    rstudioapi_is_available = function() {return(FALSE)},
    
    expect_false(is_shiny_credentialing_recommended()),
    expect_false(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_true(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on RStudio Desktop with a display and package rstudioapi installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("RStudio")},
    is_rstudio_server = function() {return(FALSE)},
    is_rstudio_desktop = function() {return(TRUE)},
    get_display = function() {return(":0")},
    installed.packages = function() {return(matrix("rstudioapi", ncol=1))},
    rstudioapi_is_available = function() {return(TRUE)},
    
    expect_false(is_shiny_credentialing_recommended()),
    expect_true(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_false(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on RStudio Server with package rstudioapi installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("RStudio")},
    is_rstudio_server = function() {return(TRUE)},
    is_rstudio_desktop = function() {return(FALSE)},
    get_display = function() {return(":0")},
    installed.packages = function() {return(matrix("rstudioapi", ncol=1))},
    rstudioapi_is_available = function() {return(TRUE)},
    
    expect_false(is_shiny_credentialing_recommended()),
    expect_true(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_false(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on R GUI with package rstudioapi installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("Rgui")},
    is_rstudio_server = function() {return(FALSE)},
    is_rstudio_desktop = function() {return(FALSE)},
    get_display = function() {return("")},
    installed.packages = function() {return(matrix("rstudioapi", ncol=1))},
    rstudioapi_is_available = function() {return(FALSE)},
    
    expect_false(is_shiny_credentialing_recommended()),
    expect_false(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_false(is_readline_credentialing_recommended())
  )
})

test_that("Recommendation on X11 with package rstudioapi installed", {
  with_mock(
    check_interactive = function() {return()},
    get_gui = function() {return("X11")},
    is_rstudio_server = function() {return(FALSE)},
    is_rstudio_desktop = function() {return(FALSE)},
    get_display = function() {return("")},
    installed.packages = function() {return(matrix("rstudioapi", ncol=1))},
    rstudioapi_is_available = function() {return(FALSE)},
    
    expect_false(is_shiny_credentialing_recommended()),
    expect_false(is_rstudioapi_credentialing_recommended()),
    expect_false(is_tcltk_credentialing_recommended()),
    expect_true(is_readline_credentialing_recommended())
  )
})