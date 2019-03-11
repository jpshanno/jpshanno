#' Maintain a list of previously processed files within a directory
#'
#' @param path A character vector of a directory containing files
#' @param pattern A pattern to match within the file names
#' @param qaqc.file The destination file of the data after QAQC/processing
#'
#' @return
#' @export
#'
#' @examples
checkpoint_directory <-
  function(path,
           pattern = NULL,
           qaqc.file = NULL){

    stopifnot(is.character(path),
              is.null(pattern) | is.character(pattern),
              fs::dir_exists(path))

    if(!is.null(qaqc.file)){
      stopifnot(is.character(qaqc.file),
                fs::dir_exists(fs::path_dir(qaqc.file)))
    }

    raw_directory <-
      fs::path_norm(path)

    raw_files <-
      fs::dir_ls(raw_directory,
                 glob = pattern)

    checkpoint_file <-
      fs::path(raw_directory,
               "__processed_files.txt")

    if(fs::file_exists(checkpoint_file)){

      existing_qaqc <-
        get_qaqc_file(checkpoint_file)

      if(!is.null(qaqc.file) && qaqc.file != existing_qaqc){
        stop("The checkpoint file ", checkpoint_file, " has the QAQC file listed as\n",
             existing_qaqc,
             "\n which does not match the supplied file\n",
             qaqc.file)
      }

      processed_files <-
        readr::read_lines(checkpoint_file,
                          skip = 2)

      raw_files <-
        raw_files[!match(fs::path_file(raw_files),
                         fs::path_file(processed_files),
                         nomatch = 0)]

    } else {

      if(is.null(qaqc.file)){
        stop("qaqc.file must be supplied if ", checkpoint_file, " does not already exist.'")
      }

      write_lines(c("# Files that have been processed and incorporated into QAQC data.",
                    paste0("# ", qaqc.file)),
                  checkpoint_file)
    }

    return(raw_files)
  }

#' Append data to an existing QAQC file and create a back-up
#'
#' @param data The data to write
#' @param input.files The files used in processing
#' @param ignore.names Logical indicating whether or not to check if the names
#'   of the existing data and new data should match
#'
#' @return
#' @export
#'
#' @examples
write_qaqc <-
  function(data,
           input.files,
           ignore.names = FALSE){

    stopifnot(is.data.frame(data),
              is.character(input.files),
              "input_source" %in% names(data))

    input_directory <-
      fs::path_common(input.files)

    checkpoint_file <-
      fs::path(input_directory,
               "__processed_files.txt")


    if(!fs::file_exists(checkpoint_file)){
      stop("There is no checkpoint file, '__processed_files.txt' in ",
           source.directory)
    }

    qaqc_file <-
      get_qaqc_file(checkpoint_file)

    if(fs::file_exists(qaqc_file)){
      qaqc_data <-
        suppressMessages(readr::read_csv(qaqc_file,
                                         guess_max = 100000,
                                         locale = readr::locale(tz = "EST")))

      if(!identical(names(qaqc_data), names(data)) & !ignore.names){
        # Check on set operations for automatic check
        stop("Column names for QAQC do not mach column names for data")
      }

      fs::file_copy(qaqc_file,
                    fs::path_ext_set(qaqc_file, "bak"),
                    overwrite = TRUE)

      if(nrow(qaqc_data) != 0){
        data <-
          map_dfc(data, as.character)

        qaqc_data <-
          map_dfc(qaqc_data, as.character)

        data <-
          dplyr::anti_join(data,
                           qaqc_data,
                           by = intersect(names(qaqc_data), names(data)))
      }

      if(nrow(data) > 0){

        data <-
          dplyr::bind_rows(data,
                           qaqc_data)

        write.csv(data,
                  qaqc_file,
                  row.names = FALSE)}
    } else {
      write.csv(data,
                qaqc_file,
                row.names = FALSE)
    }

    readr::write_lines(fs::path_file(input.files),
                       checkpoint_file,
                       append = TRUE)

    message("QAQC data were written to ", qaqc_file, "\n",
            "  ", checkpoint_file, " was updated with the processed file names\n")
  }

## REMOVE QAQC DATA FUNCTION
# Deletes __procesed_files.txt and removes data from those files from QAQC

#' Remove a directory of processed data
#'
#' This will remove the __processed_files.txt file and delete the data from the
#' QAQC file specified in __processed_files.txt
#'
#' @param path The directory to 'unprocess'
#'
#' @return
#' @export
#'
#' @examples
unprocess_directory <-
  function(path){
    stopifnot(is.character(path),
              fs::dir_exists(path),
              fs::path_ext(path) != "csv")

    # Read in processed file list from checkpont file
    checkpoint_file <-
      fs::path(path,
               "__processed_files.txt")

    processed_files <-
      readr::read_lines(checkpoint_file,
                        skip = 2)

    qaqc_file <-
      get_qaqc_file(checkpoint_file)

    if(!fs::file_exists(qaqc_file)){
      stop("The QAQC file (", qaqc_file, ") listed in ", checkpoint_file, " does not exist")
    }

    # Read in QAQC data & remove observations

    fs::file_copy(qaqc_file,
                  fs::path_ext_set(qaqc_file, "bak"),
                  overwrite = TRUE)

    qaqc <-
      suppressMessages(readr::read_csv(qaqc_file,
                                       guess = 100000)) %>%
      filter(!(input_source %in% processed_files))

    # Save updated QAQC

    if(nrow(qaqc) > 0){
      write.csv(qaqc,
                qaqc_file,
                row.names = FALSE)
    } else {
      fs::file_delete(qaqc_file)
    }

    # Delete checkpoint file
    fs::file_delete(checkpoint_file)
  }

#' Extract the qaqc filename from __procesed_files.txt
#'
#' @param x The path to __processed_files.txt
#'
#' @return
#' @export
#'
#' @examples
get_qaqc_file <-
  function(x){
    readr::read_lines(x,
                      skip = 1,
                      n_max = 1) %>%
      stringr::str_remove("# ")
  }
