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
    sans_file <-
      system.file("fonts", "LiberationSans-Regular.ttf", package = "jpshanno")

    narrow_file <-
      system.file("fonts", "LiberationSansNarrow-Regular.ttf", package = "jpshanno")

  sysfonts::font_add(family = "Liberation-Narrow", narrow_file)
  sysfonts::font_add(family = "Liberation-Sans", regular = sans_file)
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
