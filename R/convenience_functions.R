reload_jpshanno <-
  function(){

    detach(package:jpshanno, unload = TRUE)
    library(jpshanno)
  }

split_cfi_id <-
  function(x){
    split_x <-
      regmatches(x, regexec("([a-z]{3})([0-9]{3})([0-9]{3})([0-9]{3})([0-9]{4})([0-9]{6})", x))[[1]][-1]

    map_chr(split_x, str_remove, "^0+")
  }

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
