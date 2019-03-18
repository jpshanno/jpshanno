#' Fix Shimadzu transcription errors
#'
#' @param .data
#' @param sourceFile.string
#' @param site
#' @param sampleLocation
#' @param sampleDate
#' @param sampleTime
#' @param error.column
#' @param replacement.value
#'
#' @return
#' @export
#'
correct_shimadzu <-
  function(.data,
           sourceFile.string,
           site,
           sampleLocation,
           sampleDate,
           sampleTime,
           error.column,
           replacement.value){

    sampleTypes <- c(
      sampleLocation = "sampleType",
      FL = "Streamwater",
      MW1 = "Peat Porewater",
      MW2 = "Peat Porewater",
      MW3 = "Peat Porewater",
      OF = "Overland Flow",
      OF3 = "Overland Flow",
      SN = "Snow",
      SW1 = "Surface Water",
      SW2 = "Surface Water",
      SW3 = "Surface Water")


    site = stringr::str_pad(deparse(substitute(site)),
                            3,
                            "left",
                            0)
    sampleLocation = deparse(substitute(sampleLocation))
    sampleDate = stringr::str_replace_all(deparse(substitute(sampleDate)), "\\s", "")
    sampleTime = stringr::str_replace_all(deparse(substitute(sampleTime)), "\\s", "")
    error.column = deparse(substitute(error.column))
    replacement.value = deparse(substitute(replacement.value))

    # deparse and quo both strip leading zeros from months, days, hourse, and minutes
    sampleDate = stringr::str_replace_all(sampleDate,
                                          c("-([0-9]{1})-" = "-0\\1-",
                                            "-([0-9]{1})$" = "-0\\1"))
    sampleTime = stringr::str_replace_all(sampleTime,
                                          c("^([0-9]{1}):" = "0\\1:",
                                            ":([0-9]{1})$" = ":0\\1"))

    replacement.value =
      dplyr::case_when(
        error.column == "site" ~ stringr::str_pad(replacement.value,
                                                  3,
                                                  "left",
                                                  0),
        error.column == "sampleDate" ~ stringr::str_replace_all(stringr::str_replace_all(replacement.value, "\\s", ""),
                                                                c("-([0-9]{1})-" = "-0\\1-",
                                                                  "-([0-9]{1})$" = "-0\\1")),
        error.column == "sampleTime" ~ stringr::str_replace_all(stringr::str_replace_all(replacement.value, "\\s", ""),
                                                                c("^([0-9]{1}):" = "0\\1:",
                                                                  ":([0-9]{1})$" = ":0\\1")),
        TRUE ~ replacement.value)

    selectedSample <-
      which(.data$originalReport == sourceFile.string &
              .data$site == site &
              .data$sampleLocation == sampleLocation &
              .data$sampleDate == sampleDate &
              .data$sampleTime == sampleTime)

    if(nrow(.data[selectedSample, ]) == 0) stop("No sample found.")

    .data[selectedSample, error.column] <- replacement.value

    if(error.column %in% c("site", "sampleLocation")){
      .data[selectedSample, "sampleID"] <- paste(.data$site[selectedSample],
                                                 .data$sampleLocation[selectedSample],
                                                 sep = "-")
    }

    if(error.column == "sampleLocation"){
      .data[selectedSample, "sampleType"] <-
        sampleTypes[[.data[["sampleLocation"]][selectedSample]]]
    }

    return(.data)}


#' Validate sample sites and locations
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples

validate_shimadzu <-
  function(.data){
    validSampleLocations <-
      sort(c("FL", "MW1", "MW2", "MW3", "OF", "OF3", "SN", "SW1", "SW2", "SW3"))

    validSampleSites <-
      sort(c("053", "113"))


    if(any(ymd(.data$sampleDate) > today())){
      stop("\nNo output .dataset was be created because some samples have sample dates in the future. Please run invalid_samples('date') to view samples with errors.")}
    if(!all(unique(.data$sampleLocation) %in% validSampleLocations)){
      stop("\nNo output .dataset was be created because some samples have invalid sample locations listed. Please run invalid_samples('location') to view samples with errors.")}
    if(!all(unique(.data$site) %in% validSampleSites)){
      stop("\nNo output dataset was be created because some samples have invalid sites listed. Please run invalid_samples('site') to view samples with errors.")}
    return(.data)
  }


#' Return Invalid Samples
#'
#' @param errorSource
#'
#' @return
#' @export
#'
#' @examples
invalid_samples <-
  function(.data,
           errorSource = c("date", "location", "site")){

    errorSource <- match.arg(errorSource)

    validSampleLocations <-
      sort(c("FL", "MW1", "MW2", "MW3", "OF", "OF3", "SN", "SW1", "SW2", "SW3"))

    validSampleSites <-
      sort(c("053", "113"))

    badSamples <-
      dplyr::case_when(
        errorSource == "date" ~ ymd(.data$sampleDate) > lubridate::today(),
        errorSource == "site" ~ !(.data$site %in% validSampleSites),
        errorSource == "location" ~ !(.data$sampleLocation %in% validSampleLocations))
    return(filter(.data, badSamples))}
