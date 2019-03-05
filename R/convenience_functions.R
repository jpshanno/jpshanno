#' Title
#'
#' @return
#' @export
#'
#' @examples
reload_jpshanno <-
  function(){
    detach(package:jpshanno, unload = TRUE)
    library(jpshanno)
  }

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
split_cfi_id <-
  function(x){
    split_x <-
      regmatches(x, regexec("([a-z]{3})([0-9]{3})([0-9]{3})([0-9]{3})([0-9]{4})([0-9]{6})", x))[[1]][-1]

    purrr::map_chr(split_x, str_remove, "^0+") %>%
      purrr::set_names(c("source", "state_code", "unit_code",
                         "county_code", "invyr", "plot"))
  }

#' Title
#'
#' @param data
#' @param x
#'
#' @return
#' @export
#'
#' @examples
extract_cfi_id <-
  function(data, x){
    x <-
      rlang::enquo(x)

    data %>%
      tidyr::extract(!!x,
                     into = c("source", "state_code", "unit_code",
                              "county_code", "invyr", "plot"),
                     regex = "([a-z]{3})([0-9]{3})([0-9]{3})([0-9]{3})([0-9]{4})([0-9]{6})",
                     convert = TRUE)

  }

write_gdb <-
  function(obj, gdb, layer.name = NULL){

    if(is.null(layer.name)){
      layer.name <-
        deparse(substitute(obj))
    }

    gdb <-
      normalizePath(gdb)

    temp_file <-
      tempfile(fileext = ".gpkg")

    sf::write_sf(obj,
                 temp_file)

    ogr <-
      dplyr::if_else(Sys.info()[["sysname"]] == "Windows",
                     "C:/OSGeo4W64/bin/ogr2ogr",
                     "ogr2ogr")

    system_call <-
      glue::glue('{ogr} -f "FileGDB" -update "{gdb}" "{temp_file}" -nln "{layer.name}"')

    system(system_call)

    unlink(temp_file)
  }
