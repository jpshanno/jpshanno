# Adapted from the ideas in these answers:
# https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window

#' Fetch a table from an Access database from 64-bit R.
#'
#' @param db.path
#' @param table.name
#'
#' @return
#' @export
#'
#' @examples
r32_fetch_table <- function(db.path,
                            table.name) {

  stopifnot(is.character(db.path),
            length(db.path) == 1)

  rscript32 <-
    file.path(R.home("bin/i386"),
              "Rscript")

  if(!any(grepl("Rscript",
               list.files(dirname(rscript32))))){
    stop('r32_fetch_table requires a 32-bit version of R to be installed with
         your default 64 bit R installation.')
  }

  temp_file <-
    normalizePath(file.path(tempdir(),
                            'data_file.rds'),
                  winslash = "/",
                  mustWork = FALSE)

  script <-
    glue::glue(
      "db <- DBI::dbConnect(odbc::odbc(),
                          Driver = 'Microsoft Access Driver (*.mdb, *.accdb)',
                          Dbq = '{db.path}')
      table <- DBI::dbReadTable(db,
                      '{table.name}')
      DBI::dbDisconnect(db)
      readr::write_rds(x = table,
                       path = '{temp_file}')")

  command <-
    glue::glue("{rscript32} -e \
                \"{script}\"")

  res <-
    system(command)

  if(res != 0){
    print(command)
    stop("The above system call failed with exit code ", res)
  }

  df <-
    readr::read_rds(temp_file) %>%
    tibble::as_tibble()

  unlink(temp_file)

  return(df)
}


#' List tables in a 32-bit Access database from 64-bit R
#'
#' @param db.path
#'
#' @return
#' @export
#'
#' @examples
r32_list_tables <- function(db.path) {

  stopifnot(is.character(db.path),
            length(db.path) == 1)

  rscript32 <-
    file.path(R.home("bin/i386"),
              "Rscript")

  if(!any(grepl("Rscript",
                list.files(dirname(rscript32))))){
    stop('r32_fetch_table requires a 32-bit version of R to be installed with
         your default 64 bit R installation.')
  }

  script <-
    glue::glue(
      "db <- DBI::dbConnect(odbc::odbc(),
                          Driver = 'Microsoft Access Driver (*.mdb, *.accdb)',
                          Dbq = '{db.path}')
      tables <- DBI::dbListTables(db)
      DBI::dbDisconnect(db)
      cat(tables,
          sep = '\n')")

  command <-
    paste(rscript32,
          '-e "',
          script,
          '"')

  tables <-
    system(command,
           intern = TRUE)

  return(tables)
}


#' Send a query to and fetch results from a 32-bit Access database from 64-bit R
#'
#' @param db.path
#'
#' @return
#' @export
#'
#' @examples
r32_query <- function(db.path,
                      sql.string) {

  stopifnot(is.character(db.path),
              is.character(sql.string),
            length(db.path) == 1)
  rscript32 <-
    file.path(R.home("bin/i386"),
              "Rscript")

  if(!any(grepl("Rscript",
                list.files(dirname(rscript32))))){
    stop('r32_fetch_table requires a 32-bit version of R to be installed with
         your default 64 bit R installation.')
  }

  temp_file <-
    normalizePath(file.path(tempdir(),
                            'data_file.rds'),
                  winslash = "/",
                  mustWork = FALSE)

  script <-
    glue::glue(
      "db <- DBI::dbConnect(odbc::odbc(),
                          Driver = 'Microsoft Access Driver (*.mdb, *.accdb)',
                          Dbq = '{db.path}')
      result <- DBI::dbGetQuery(db,
                                '{sql.string}')
      DBI::dbDisconnect(db)
      readr::write_rds(result,
                       '{temp_file}')")

  command <-
    paste(rscript32,
          '-e "',
          script,
          '"')

  system(command)

  df <-
    readr::read_rds(temp_file) %>%
    tibble::as_tibble()

  unlink(temp_file)

  return(df)
}



#' Run an expression in 32-bit R
#'
#' @param expr
#' @param result
#'
#' @return
#' @export
#'
#' @examples
r32_run <-
  function(expr,
           result = "result"){

    stopifnot(is.character(result),
              is.expression(expr),
              length(expr) == 1)
    rscript32 <-
      file.path(R.home("bin/i386"),
                "Rscript")

    if(!any(grepl("Rscript",
                  list.files(dirname(rscript32))))){
      stop('r32_fetch_table requires a 32-bit version of R to be installed with
         your default 64 bit R installation.')
    }

    temp_file <-
      normalizePath(file.path(tempdir(),
                              'data_file.rds'),
                    winslash = "/",
                    mustWork = FALSE)

    expr <-
      deparse(expr)

    expr <-
      expr[-c(1, length(expr))]

    expr <-
      paste(expr,
            collapse = "\n")

    expr <-
      gsub('\"', "'", expr)

    command <-
      glue::glue("{rscript32} -e \
                \"{expr}
                readr::write_rds({result},
                               '{temp_file}')\"")

    res <-
      system(command)

    if(res != 0){
      print(command)
      stop("The above system call failed with exit code ", res)
    }

    df <-
      readr::read_rds(temp_file)

    unlink(temp_file)

    return(df)
  }

#' Source a file using a 32-bit R process
#'
#' @param source.file
#'
#' @return
#' @export
#'
#' @examples
r32_source <-
  function(source.file){

    stopifnot(is.character(source.file),
              length(source.file) == 1)

    rscript32 <-
      file.path(R.home("bin/i386"),
                "Rscript")

    if(!any(grepl("Rscript",
                  list.files(dirname(rscript32))))){
      stop('r32_fetch_table requires a 32-bit version of R to be installed with
         your default 64 bit R installation.')
    }

    command <-
      glue::glue("{rscript32} -e \
                  {source.file}")

    res <-
      system(command)

    if(res != 0){
      print(command)
      stop("The above system call failed with exit code ", res)
    }

    return(df)
  }


