# These are somewhat inefficient because they collect the data for each function
# call, but for my purposes that's okay. It allows me to keep remote data as
# remote and local data as local. Alternative you could copy the local data each
# time as that is likely to the the smaller dataset.

#' Title
#'
#' @param local
#' @param remote
#' @param column
#'
#' @return
#' @export
#'
#' @examples
check_categorical <-
  function(local,
           remote,
           column){

    column <-
      rlang::enquo(column)

    local <-
      dplyr::select(local, !!column)

    remote <-
      dplyr::select(remote, !!column) %>%
      collect()

    dplyr::anti_join(local,
                     remote,
                     by = rlang::quo_text(column)) %>%
      dplyr::distinct()
  }

#' Title
#'
#' @param local
#' @param remote
#' @param column
#'
#' @return
#' @export
#'
#' @examples
check_numeric <-
  function(local,
           remote,
           column){

    column <-
      rlang::enquo(column)

    local <-
      dplyr::select(local, !!column)

    remote <-
      dplyr::select(remote, !!column) %>%
      collect()

    combined <-
      dplyr::bind_rows(local = local,
                       remote = remote,
                       .id = "source")

    ggplot2::ggplot(data = combined,
                    ggplot2::aes(y = !!column,
                                 x = source)) +
      ggplot2::geom_violin()
  }


#' Title
#'
#' @param local
#' @param remote
#' @param verbose
#' @param ignore.structure
#'
#' @return
#' @export
#'
#' @examples
insert_data <-
  function(local,
           remote,
           verbose = TRUE,
           ignore.structure = FALSE){

    stopifnot(is.logical(verbose),
              is.logical(ignore.structure))


    # Get connection information and remote table name
    con <-
      remote[[1]]$con

    tbl_name <-
      as.character(remote[[2]]$x)

    # Check for spatial data and transform to match database
    is_spatial <-
      "sf" %in% class(local)

    if(is_spatial){

      crs_local <-
        sf::st_crs(local)$epsg

      crs_db <-
        DBI::dbGetQuery(con,
                        paste0("SELECT srid FROM geometry_columns WHERE f_table_name = '",
                               dplyr::sql(tbl_name), "';"))[["srid"]]

      if(crs_local != crs_db){
        local <-
          sf::st_transform(local, crs_db)
      }

      geom_col <-
        attr(local, "sf_column")

      local <-
        local %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::mutate(geometry = sf::st_as_binary(local[[geom_col]],
                                                  EWKB = TRUE,
                                                  hex = TRUE))

      class(local[["geometry"]]) <- "pq_geometry"
    }


    # Get table data structures
    local_table_structure <-
      vapply(local, class, character(1))

    remote_table_structure <-
      vapply(collect(head(remote)), class, character(1))

    identical_tables <-
      identical(local_table_structure,
                remote_table_structure)

    if(!ignore.structure & !identical_tables){
      # message("Local Table Structure:")
      # print(vapply(local, class, character(1)))
      # message("Remote Table Structure:")
      # print(vapply(collect(head(remote)), class, character(1)))
      stop("\nLocal and remote data do not match\n",
           "Local Table Structure:\n",
           paste(capture.output(vapply(local, class, character(1))), collapse = "\n"),
           "\nRemote Table Structure:\n",
           paste(capture.output(vapply(collect(head(remote)), class, character(1))), collapse = "\n"),
           call. = FALSE)
    }

    # postgreSQL is storing double as float8, but dbWriteTable uploads as float8
    # so we have to get the column types from the database, even if the R types
    # match as tested above
    res <-
      DBI::dbSendQuery(map_db, paste("SELECT * FROM", dplyr::sql(tbl_name), "LIMIT 1"))

    # On completion or error clear the results of the query
    on.exit(DBI::dbClearResult(res))

    column_info <-
      DBI::dbColumnInfo(res)

    data_types <-
      column_info[[".typname"]]

    data_types <-
      setNames(data_types,
               column_info[["name"]])

    DBI::dbClearResult(res)

    # Copy local data to remote source. It's best to be pretty certain you are
    # not copying duplicate rows, or rows already in the table as this is the
    # biggest bottleneck to the function
    DBI::dbWriteTable(conn = con,
                      name = "temp",
                      value = local,
                      field.types = data_types,
                      temporary = TRUE)

    # On completion or error remove the temporary remote copy of local
    on.exit(DBI::dbRemoveTable(conn = con,
                               name = "temp"))

    # Create SQL
    # Use and except statement instead of an anti-join. We are looking to append
    # new rows to the existing table, which is a job for UNION because we verify
    # that the two tables have identical structures. Using UNION would require
    # overwriting the existing remote table. Using EXCEPT let's us remove any
    # local records that are already in remote, and then INSERT the results into
    # remote without overwriting.

    except_sql <-
      paste('( SELECT * FROM temp EXCEPT SELECT * FROM', dplyr::sql(tbl_name), ');')

    # Check SQL
    # DBI::dbGetQuery(except_sql) %>%
    #   as_tibble()

    insert_sql <-
      paste('INSERT INTO', dplyr::sql(tbl_name), except_sql)

    # Execute SQL and store number of rows affect, which equals the number of
    # rows inserted
    n_inserted <-
      DBI::dbExecute(map_db,
                     insert_sql)

    if(verbose){ message(n_inserted, " rows added to ", tbl_name) }
  }
