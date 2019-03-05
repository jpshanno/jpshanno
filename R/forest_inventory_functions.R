#' Calculate Modified importance value
#'
#' Calculates a modified importance value using relative dominance (basal area)
#' and relative density (relative prevalence) rescaled to fall between 0 and 1.
#'
#' Importance values were originally described in Curtis and MacIntosh (1951).
#' An Upland Forest Continuum in the Prarie-Forest Border Region of Wisconsin.
#'
#' @param basal.area a numeric vector of basal areas
#' @param prevalence a numeric vector of prevalence
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#'
importance_value <-
  function(basal.area, prevalence){

    stopifnot(is.numeric(basal.area),
              is.numeric(prevalence))

    relative_ba <-
      basal.area / sum(basal.area, na.rm = TRUE)

    relative_prevalence <-
      prevalence / sum(prevalence, na.rm = TRUE)


    0.5 * (relative_ba + relative_prevalence)
  }
