#' Title
#'
#' @param idw_power
#'
#' @return
#' @export
#'
#' @examples
calculate_weights <-
  function(idw_power = 3){
    subplot_centers <-
      map(list(c(0, 0),
               c(0, 120),
               c(120*sin(pi/3), -120 * cos(pi/3)),
               c(-120*sin(pi/3), -120 * cos(pi/3))),
          matrix,
          nrow = 1)

    weights <-
      map(subplot_centers,
          function(coords){

            x <- coords[1]
            y <- coords[2]

            bbox_x <- bbox_grid[["x"]]
            bbox_y <- bbox_grid[["y"]]

            distances <-
              sqrt((bbox_x - x)^2 + (bbox_y - y)^2)

            distances <-
              ifelse(distances <= 24,
                     0,
                     distances - 24)

            weights <-
              1 / distances^idw_power

            ifelse(is.infinite(weights) | weights > 1,
                   1,
                   weights)
          }
      ) %>%
      reduce(cbind)

    colnames(weights) <-
      paste0("subplot_", 1:4)

    weights
  }

#' Title
#'
#' @param measure
#' @param subgroup
#' @param idw_power
#'
#' @return
#' @export
#'
#' @examples
interpolate_plot <-
  function(measure, subgroup, idw_power = 2){

    weights <-
      calculate_weights(idw_power)

    col_name <-
      unique(subgroup)

    list(matrix((weights %*% measure) / rowSums(weights),
                ncol = 1,
                dimnames = list(NULL, col_name)))
  }

#' Title
#'
#' @param matrix.col
#'
#' @return
#' @export
#'
#' @examples
collapse_plot <-
  function(matrix.col){
    as.list(matrix.col) %>%
      reduce(cbind) %>%
      list(.)
  }

#' Title
#'
#' @param mat
#' @param point.threshold
#'
#' @return
#' @export
#'
#' @examples
calculate_relative_matrix <-
  function(mat,
           point.threshold = 0){

    species_matrix <-
      cbind(mat,
            total = rowSums(mat, na.rm = TRUE))

    relative_matrix <-
      species_matrix[, -c(ncol(species_matrix)), drop = FALSE] /
      species_matrix[, "total"]

    replace(relative_matrix,
            which(relative_matrix < point.threshold),
            0)

  }

#' Title
#'
#' @param mat
#' @param point.threshold
#' @param plot.threshold
#'
#' @return
#' @export
#'
#' @examples
get_species_presence <-
  function(mat,
           point.threshold = 0,
           plot.threshold = 0.2){

    map(mat,
            function(x){
              relative_matrix <-
                calculate_relative_matrix(x,
                                          point.threshold)

              mean_relative_measure <-
                colMeans(relative_matrix)

              mean_relative_measure <-
                mean_relative_measure[which(mean_relative_measure > plot.threshold)]

              as_tibble(as.list(mean_relative_measure))})
  }

#' Title
#'
#' @param measure
#' @param point.threshold
#' @param plot.threshold
#'
#' @return
#' @export
#'
#' @examples
get_dominant_species <-
  function(mat,
           point.threshold = 0,
           plot.threshold = 0){

    map_chr(mat,
            function(MAT){

              relative_matrix <-
                calculate_relative_matrix(MAT,
                                          point.threshold)

              dominant_species <-
                colnames(relative_matrix)[max.col(relative_matrix)]

              dominant_species <-
                replace(dominant_species,
                        which(rowSums(relative_matrix) == 0),
                        "mixed")

              dominant_species <-
                sort(table(dominant_species), decreasing = TRUE)[1]


              if(dominant_species >= plot.threshold*nrow(relative_matrix)){
                names(dominant_species)
              } else {
                "mixed"
              }
            }
    )
  }

#' Title
#'
#' @param data
#' @param ...
#' @param classifier
#' @param classifier.options
#' @param idw_power
#' @param use.parallel
#'
#' @return
#' @export
#'
#' @examples
classify_plot <-
  function(data, ...,
           classifier = get_dominant_species,
           classifier.options = NULL,
           idw_power = 3,
           use.parallel = FALSE){

    if(use.parallel){
      map <-
        furrr::future_map

      plan(multiprocess,
           workers = 4)
    } else {
      map <-
        purrr::map
    }


    measure <-
      enquos(...)

    classed_dfs <-
      map(measure,
          ~data %>%
            group_by(plot_id, species) %>%
            summarize(!!.x := interpolate_plot(!!.x, species, idw_power)) %>%
            summarize(!!.x := collapse_plot(!!.x)) %>%
            mutate(!!.x := do.call(classifier, list(mat = !!.x, classifier.options))) %>%
            unnest(!!.x))

    if(use.parallel){
      plan(sequential)
    }

    reduce(classed_dfs,
           left_join,
           by = "plot_id")
  }
