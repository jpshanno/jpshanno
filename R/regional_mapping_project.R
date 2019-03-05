# regional_mapping_project.R
#
# Functions specifically tailor to a regional mapping effort of black ash

#' Split a CFI plot id into constituent parts.
#'
#'
#' @param x A character vector, or for \code{extract_cfi_id} the name of a column
#' @param data The data containing the CFI column for \code{extract_cfi_id}
#'
#' @return \code{split_cfi_id} returns a character vector and
#'   \code{extract_cfi_id} runs \code{extract} on a column in a tibble
#' @export
#' @rdname split_cfi
#'
split_cfi_id <-
  function(x){
    split_x <-
      regmatches(x, regexec("([a-z]{3})([0-9]{3})([0-9]{3})([0-9]{3})([0-9]{4})([0-9]{6})", x))[[1]][-1]

    purrr::map_chr(split_x, str_remove, "^0+") %>%
      purrr::set_names(c("source", "state_code", "unit_code",
                         "county_code", "invyr", "plot"))
  }

#' @export
#' @rdname split_cfi
#'
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



#' Calculate the weights of each grid cell in a CFI plot
#'
#' If idw.power is one of 0.25, 0.5, 1, 2, or 3 and n.subplots = 4 then
#' precomputed weights are loaded.
#'
#' @param idw.power The weight to use for IDW
#' @param n.subplots The number of subplots in each CFI plot (4 for FIA)
#'
#' @return A matrix of weights
#' @export
#'
calculate_weights <-
  function(idw.power = 3,
           n.subplots = 4){

    if(n.subplots == 4 & idw.power %in% c(0.25, 0.5, 1, 2, 3)){
      existing_weights <-
        paste0("weights_", idw.power)

      return(get(existing_weights))
    }

    bbox_grid <-
      cells_in_plot

    subplot_centers <-
      purrr::map(list(c(0, 0),
                      c(0, 120),
                      c(120*sin(pi/3), -120 * cos(pi/3)),
                      c(-120*sin(pi/3), -120 * cos(pi/3))),
                 matrix,
                 nrow = 1)

    subplot_centers <-
      subplot_centers[1:n.subplots]

    weights <-
      purrr::map(subplot_centers,
                 function(coords){

                   x <- coords[1]
                   y <- coords[2]

                   bbox_x <- bbox_grid[["x"]]
                   bbox_y <- bbox_grid[["y"]]

                   distances <-
                     sqrt((bbox_x - x)^2 + (bbox_y - y)^2)

                   in_plot <-
                     which(distances <= 24)

                   distances[in_plot] <-
                     0

                   distances[-in_plot] <-
                     distances[-in_plot] - 24

                   weights <-
                     1 / distances^idw.power

                   weights[which(is.infinite(weights))] <-
                     1

                   weights
                 }
      ) %>%
      purrr::reduce(cbind)

    colnames(weights) <-
      paste0("subplot_", 1:4)

    weights
  }

#' Interpolate a given measure across the entire CFI plot
#'
#' @param measure A numeric vector
#' @param subgroup A grouping variable supplied as length 1 or same length as
#'   measure
#' @param ... Other parameters passed on to \code{\link{calculate_weights}}
#'
#' @return A list of matrices equal in length to measure
#' @export
#'
interpolate_plot <-
  function(measure,
           subgroup,
           ...){

    weights <-
      calculate_weights(...)

    col_name <-
      unique(subgroup)

    list(matrix((weights %*% measure) / rowSums(weights),
                ncol = 1,
                dimnames = list(NULL, col_name)))
  }

#' Collapse a list-column of matrices into a single matrix
#'
#' @param matrix.col A list column of matrices
#' @param fun One of \code{rbind} or \code{cbind} used to combine the matrices
#'
#' @return A list of matrices
#' @export
#'
collapse_matrices <-
  function(matrix.col,
           fun = cbind){
    list(reduce(matrix.col, fun))
  }

#' Calculate the rowwise relative values of a matrix
#'
#' If \code{point.threshold} is use values are not iteratively removed, just one
#' pass is performed and the relative values are not adjusted
#'
#' @param mat The matrix of raw values
#' @param point.threshold A threshold below which the value should be set to zero
#'
#' @return A matrix of the same dimensions as mat
#' @export
#'
calculate_relative_matrix <-
  function(mat,
           point.threshold = 0.2){

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

#' Get the mean relative measure of species in a plot
#'
#' @param mat A matrix of relative values
#' @param point.threshold A threshold specifying what proportion of the relative
#'   measure at given point a species has represent to be considered 'present'.
#'   Passed on to calculate_relative_matrix
#' @param plot.threshold A threshold specifying what proportion of a plot a
#'   species must be present in to be considered present
#'
#' @family plot classifiers
#' @return A tibble of n-columns with the plot-level relative measure of the
#'   species whose mean relative measure across the plot is greater than
#'   plot.threshold
#' @export
#'
get_species_proportion <-
  function(mat,
           point.threshold = 0.2,
           plot.threshold = 0.2){

    relative_matrix <-
      calculate_relative_matrix(mat,
                                point.threshold)

    mean_relative_measure <-
      colMeans(relative_matrix)

    mean_relative_measure <-
      mean_relative_measure[which(mean_relative_measure > plot.threshold)]

    as_tibble(as.list(mean_relative_measure))
  }

#' Get a binary response of presence/absence of a species in a plot
#'
#' @param mat A matrix of relative values
#' @param point.threshold A threshold specifying what proportion of the relative
#'   measure at given point a species has represent to be considered 'present'.
#'   Passed on to calculate_relative_matrix
#' @param plot.threshold A threshold specifying what proportion of a plot a
#'   species must be present in to be considered present
#'
#' @family plot classifiers
#' @return A tibble of n-columns with a value of 1 for each species whose mean
#'   relative measure across the plot is greater than plot.threshold
#' @export
#'
get_species_presence <-
  function(mat,
           point.threshold = 0.2,
           plot.threshold = 0.2){

    relative_matrix <-
      calculate_relative_matrix(mat,
                                point.threshold)

    mean_relative_measure <-
      colMeans(relative_matrix)

    mean_relative_measure <-
      mean_relative_measure[which(mean_relative_measure > plot.threshold)]

    mean_relative_measure <-
      replace(mean_relative_measure,
              seq_len(length(mean_relative_measure)),
              1)

    as_tibble(as.list(mean_relative_measure))
  }

#' Return a single dominant species for each plot
#'
#' @param mat A matrix of relative values
#' @param point.threshold A threshold specifying what proportion of the relative
#'   measure at given point a species has represent to be considered present.
#'   Passed on to calculate_relative_matrix
#' @param plot.threshold A threshold specifying what proportion of a plot a
#'   species must be present in to be considered dominant
#'
#' @family plot classifiers
#' @return A character vector of the single species that makes up the largest
#'   share of the measure in a plot, or 'mixed' if no species makes up more than
#'   \code{plot.threshold}
#' @export
#'
get_dominant_species <-
  function(mat,
           point.threshold = 0.2,
           plot.threshold = 0.2){
    relative_matrix <-
      calculate_relative_matrix(mat,
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

#' Classify a CFI plot using a specified classifier
#'
#' @param data Dataframe or tibble containing plot data with columns plot_id and
#'   species
#' @param ... The measures used to classify plots as unquoted column names
#' @param classifier The classifier to use, see below for options
#' @param point.threshold A threshold specifying what proportion of the relative
#'   measure at given point a species has represent to be considered present.
#'   Passed on to calculate_relative_matrix
#' @param plot.threshold A threshold specifying what proportion of a plot a
#'   species must be present in to be considered dominant or present
#' @param idw.power The power used for IDW
#' @param use.parallel Logical indicating if parallel processing should be used
#'   (requires the furrr package)
#'
#' @family plot classifiers
#' @return
#' A tibble containing the columns 'plot_id' and 'species' and the column(s)
#' of the results of the classifier
#'
#' @export
#'
classify_plot <-
  function(data, ...,
           classifier = get_dominant_species,
           point.threshold = 0.2,
           plot.threshold = 0.2,
           idw.power = 3,
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
            summarize(!!.x := interpolate_plot(!!.x, species, idw.power)) %>%
            summarize(!!.x := collapse_matrices(!!.x)) %>%
            mutate(!!.x := map(!!.x, ~do.call(classifier, list(mat = .x, point.threshold = point.threshold, plot.threshold = plot.threshold)))) %>%
            unnest(!!.x))

    if(use.parallel){
      plan(sequential)
    }

    reduce(classed_dfs,
           left_join,
           by = "plot_id")
  }
