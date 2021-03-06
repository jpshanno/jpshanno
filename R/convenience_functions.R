#' Detach and reload this package
#'
#' @return
#' @export
#'
#' @examples
reload_jpshanno <-
  function(){
    detach(package:jpshanno)
    library(jpshanno)
  }

#' Copy the supplied .Rprofile to your home directory
#'
#' @param overwrite Logical indicating if an exisits .Rprofile should be
#'   overwritten
#' @param view.only Logical indicating if .Rprofile should be printed in the
#'   console instead of copied
#'
#' @return
#' @export
#'
#' @examples
set_rprofile <-
  function(overwrite = TRUE,
           view.only = FALSE){

    r_profile <-
      system.file(".Rprofile",
                  package = "jpshanno")

    if(view.only){
      cat(readLines(r_profile), sep = "\n")
    }

    new_path <-
      file.path(Sys.getenv("HOME"), ".Rprofile")

    if(file.exists(new_path) & !overwrite){
      stop(new_path, " already exists.")
    }

    copy_cmd <-
      ifelse(Sys.info()[["sysname"]] == "Windows",
             glue::glue("COPY /Y {r_profile} {new_path"),
             glue::glue("cp {r_profile} {new_path} 2> /dev/null"))

    copy_result <-
      system(copy_cmd)

    if(copy_result != "0"){
      stop("Copying .Rprofile failed with exit status ", copy_result, "\n  ",
           copy_cmd,
           "\n  And exit status ",
           copy_result,
           call. = FALSE)
    } else {
      source(new_path,
             echo = FALSE)
    }
  }

#' Load liberation fonts
#'
#' This loads the font files included in the package
#'
#' @export
#'
load_liberation_fonts <-
  function(){
    sans_reg <-
      system.file("fonts", "LiberationSans-Regular.ttf", package = "jpshanno")

    sans_bold <-
      system.file("fonts", "LiberationSans-Bold.ttf", package = "jpshanno")

    sans_italic <-
      system.file("fonts", "LiberationSans-Italic.ttf", package = "jpshanno")

    narrow_reg <-
      system.file("fonts", "LiberationSansNarrow-Regular.ttf", package = "jpshanno")

    narrow_bold <-
      system.file("fonts", "LiberationSansNarrow-Bold.ttf", package = "jpshanno")

    narrow_italic <-
      system.file("fonts", "LiberationSansNarrow-Italic.ttf", package = "jpshanno")

  sysfonts::font_add(family = "Liberation",
                     regular = narrow_reg,
                     bold = narrow_bold,
                     italic = narrow_italic)
  sysfonts::font_add(family = "liberation-sans",
                     regular = sans_reg,
                     bold = sans_bold,
                     italic = sans_italic)

  showtext::showtext_auto()
}

#' Print all the rows of a tibble
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
print_all <-
  function(data){
    print(data,
          n = nrow(data))

    invisible(data)
  }


#' Set black ash study site treatment period
#'
#' @param x Time series of sample data/times
#' @param study One of "pws", "eco", "planting"
#'
#' @return A factor with levels "Pre-treatment" and "Post-treatment"
#' @export
#' @examples
set_treatment_period <-
  function(x, study = "pws"){
    match.arg(study, c("pws", "eco", "planting"))

    treatment_date <-
      dplyr::if_else(study == "pws",
             lubridate::ymd_hms("2015-03-31 00:00:00"),
             lubridate::ymd_hms("2014-03-31 00:00:00"))

    factor(ifelse(x < treatment_date,
                   "Pre-treatment",
                   "Post-treatment"),
           levels = c("Pre-treatment", "Post-treatment"),
           labels = c("Pre-treatment", "Post-treatment"))
  }


#' Combine Standard Errors
#'
#' @param mu A vector of two means
#' @param se A vector of two standard errors
#' @param n A vector of two group sizes
#'
#' @return
#' @export
#'
#' @examples
combine_se <-
  function(mu, se, n){
    # From Baker & Nissim. 1963. Expressions for Combining Standard Errors of
    # Two Groups and for Sequential Standard Error. Nature

    stopifnot(length(mu) == 2 & length(se) == 2 & length(n) == 2)

    n1 <- n[1]
    n2 <- n[2]
    N <- sum(n)
    mu1 <- mu[1]
    mu2 <- mu[2]
    se1 <- se[1]
    se2 <- se[2]

    scalar <- 1/(N*(N-1))
    scaled_se1 <- n1*(n1-1)*se1^2
    scaled_se2 <- n2*(n2-1)*se2^2
    scaled_mean <- ((n1*n2) / N)*(mu1-mu2)^2

    SE_sq <- scalar * sum(scaled_se1, scaled_se2, scaled_mean)

    sqrt(SE_sq)

  }
