# See ?RStoolbox::spectralIndices for more indices
# The list of indices to look at was originally from Wolter and Townsend 2011

# These functions are designed to work on vectors of data (especially
# tasseled_cap()) so that they work within a tidyverse pipeline

# Soil Adjusted Vegetation Index (Huete, 1988) as implemented in RStoolbox
#' Title
#'
#' @param nir
#' @param red
#' @param L
#'
#' @return
#' @export
#'
#' @examples
savi <-
  function(nir, red, L) {
    savi <- (nir - red) * (1 + L)/(nir + red + L)
    return(savi)
  }

# Simple ratio (Jordan, 1969)
#' Title
#'
#' @param nir
#' @param red
#'
#' @return
#' @export
#'
#' @examples
sr <-
  function(nir, red) {
    sr <- nir / red
    return(sr)
  }

# Moisture Stress Index (Rock et al, 1986)
#' Title
#'
#' @param swir1
#' @param nir
#'
#' @return
#' @export
#'
#' @examples
msi <-
  function(swir1, nir){
    msi <- swir1 / nir
    return(msi)
  }

# NDVI
#' Title
#'
#' @param red
#' @param nir
#'
#' @return
#' @export
#'
#' @examples
ndvi <-
  function(red, nir){
    num <- nir - red
    den <- nir + red
    num / den}

# Senescing
#' Title
#'
#' @param nir
#' @param green
#'
#' @return
#' @export
#'
#' @examples
senesce <-
  function(nir, green){
    nir / green
  }

# Global environmental monitoring index (Pinty & Verstraete, 1992) as implemented in RStoolbox
#' Title
#'
#' @param nir
#' @param red
#'
#' @return
#' @export
#'
#' @examples
gemi <-
  function(nir, red) {
    gemi <-
      (((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5)) /(nir + red + 0.5)) *
      (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5))/(nir + red + 0.5)) * 0.25)) -
      ((red - 0.125)/(1 - red))
    return(gemi)
  }

# Shortwave to visible (Wolter 2008) (equivalent to svr5)
#' Title
#'
#' @param red
#' @param blue
#' @param green
#' @param swir1
#'
#' @return
#' @export
#'
#' @examples
svr1 <-
  function(red, blue, green, swir1){
    # Don't use mean() here because then you have to do the calculations rowwise inside of a pipeline
    vis <- (red + blue + green) / 3
    svr1 <- swir1 / vis
    return(svr1)
  }

# Shortwave to visible (Wolter 2008) (equivalent to svr7)
#' Title
#'
#' @param red
#' @param blue
#' @param green
#' @param swir2
#'
#' @return
#' @export
#'
#' @examples
svr2 <-
  function(red, blue, green, swir2){
    # Don't use mean() here because then you have to do the calculations rowwise inside of a pipeline
    vis <- (red + blue + green) / 3
    svr2 <- swir2 / vis
    return(svr2)
  }

#
#' Title
#'
#' @param sr_leaf_off
#' @param sr_august
#'
#' @return
#' @export
#'
#' @examples
sr_ash <-
  function(sr_leaf_off, sr_august){
    sr_ash <- sr_leaf_off - sr_august
    return(sr_ash)
  }

#' Title
#'
#' @param blue
#' @param green
#' @param red
#' @param nir
#' @param swir5
#' @param swir7
#'
#' @return
#' @export
#'
#' @examples
tasseled_cap <-
  function(blue, green, red, nir, swir5, swir7) {

    # GEE OUTPUT for 0000e59b18f995aee40b
    # march_brightness: 0.4916403575967997
    # march_greenness: -0.025064339463412747
    # march_wetness: 0.19869151734858753
    # march_fifth: 0.011010079735144979
    # march_fourth: -0.11018490345627072
    # march_sixth: -0.06892729632407427

    # Coefficients from Biag et al 2014

    col_names <- c("blue", "green", "red", "nir", "swir_1", "swir_2")
    row_names <- c("brightness", "greenness", "wetness", "fourth", "fifth", "sixth")

    tc_coef <-
      matrix(data = c(  0.3029, 	0.2786, 	0.4733, 	0.5599, 	0.508, 	0.1872,
                        -0.2941, 	-0.243, 	-0.5424, 	0.7276, 	0.0713, 	-0.1608,
                        0.1511, 	0.1973, 	0.3283, 	0.3407, 	-0.7117, 	-0.4559,
                        -0.8239, 	0.0849, 	0.4396, 	-0.058, 	0.2013, 	-0.2773,
                        -0.3294, 	0.0557, 	0.1056, 	0.1855, 	-0.4349, 	0.8085,
                        0.1079, 	-0.9023, 	0.4119, 	0.0575, 	-0.0259, 	0.0252),
             nrow = 6,
             dimnames = list(row_names, col_names),
             byrow = TRUE)

    pixel_val <-
      matrix(c(blue, green, red, nir, swir5, swir7),
             nrow = 6,
             byrow = TRUE)

    tc_space <-
      t(tc_coef %*% pixel_val)

    tc_df <-
      setNames(asdata.frame(tc_space),
               c("brightness",
                 "greenness",
                 "wetness",
                 "fourth",
                 "fifth",
                 "sixth"))

    tc_list <-
      split(tc_df[,1:3],
            seq(nrow(tc_df)))

    return(tc_list)
  }

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
classify_plots <-
  function(data){

    df <-
      data %>%
      mutate(species = if_else(status == "dead",
                               paste0("dead-", species),
                               species)) %>%
      group_by(plot_id,
               subplot) %>%
      mutate(plot_ba_m2 = sum(tree_ba_m2,
                              na.rm = TRUE),
             n_stems = n()) %>%
      group_by(species, add = TRUE) %>%
      summarize(prevalence = n() / mean(n_stems, na.rm = TRUE),
                relative_ba = sum(tree_ba_m2, na.rm = TRUE) / mean(plot_ba_m2, na.rm = TRUE)) %>%
      mutate(iv = (prevalence + relative_ba) / 2) %>%
      ungroup()

    if(any(stringr::str_detect(class(data), "tbl_dbi"))){
      df <-
        compute(df,
                "temp")

      on.exit(DBI::dbRemoveTable(map_db,
                                 "temp"))
    }

    purrr::reduce(list(classify_by_measure(df, prevalence),
                       classify_by_measure(df, relative_ba),
                       classify_by_measure(df, iv)),
                  full_join,
                  by = c("plot_id"))
  }

#     # classed_prev <-
#     df %>%
#       filter(prevalence == max(prevalence, na.rm = TRUE)) %>%
#       group_by(plot_id,
#                species) %>%
#       mutate(rank = n() + prevalence) %>%
#       group_by(plot_id,
#                subplot) %>%
#       arrange(desc(rank),
#               desc(iv)) %>%
#       filter(row_number() == 1) %>%
#       group_by(plot_id) %>%
#       mutate(n_subplots = n()) %>%
#       filter(row_number() == 1) %>%
#       mutate(class_by_prevalence = if_else(rank > ceiling(n_subplots / 2) | prevalence < 0.5,
#                                            species,
#                                            "mixed"),
#              proportion_by_prevalence = floor(rank) / n_subplots) %>%
#       select(plot_id,
#              max_prevalence = prevalence,
#              proportion_by_prevalence,
#              class_by_prevalence)
#
#     sql("COUNT(prevalence_class) OVER (PARTITION BY plot_id, prevalence_class) + max_prevalence"))
#
# ,
# prevalence_class = if_else(stem_prevalence == max(stem_prevalence, na.rm = TRUE),
#                            species,
#                            NA_character_),
# ba_class = if_else(relative_ba == max(relative_ba, na.rm = TRUE),
#                    species,
#                    NA_character_),
# iv_class = if_else(iv == max(iv, na.rm = TRUE),
#                    species,
#                    NA_character_)) %>%
#   summarize(max_prevalence = max(stem_prevalence, na.rm = TRUE),
#             max_relative_ba = max(relative_ba, na.rm = TRUE),
#             max_iv = max(iv, na.rm = TRUE),
#             prevalence_class = max(prevalence_class, na.rm = TRUE),
#             ba_class = max(ba_class, na.rm = TRUE),
#             iv_class = max(iv_class, na.rm = TRUE)) %>%
#   ungroup()
#
#   }

#' Title
#'
#' @param data
#' @param measure
#'
#' @return
#' @export
#'
#' @examples
classify_by_measure <-
  function(data,
           measure){

    measure <-
      enquo(measure)

    measure_name <-
      rlang::quo_text(measure)

    class_name <-
      rlang::parse_expr(paste0("class_by_", measure_name))

    prop_name <-
      rlang::parse_expr(paste0("proportion_by_", measure_name))

    data %>%
      group_by(plot_id,
               subplot) %>%
      filter(!!measure == max(!!measure, na.rm = TRUE)) %>%
      group_by(plot_id,
               species) %>%
      mutate(rank = n() + !!measure) %>%
      group_by(plot_id,
               subplot) %>%
      arrange(desc(rank),
              desc(iv)) %>%
      filter(row_number() == 1) %>%
      group_by(plot_id) %>%
      mutate(n_subplots = n()) %>%
      filter(row_number() == 1) %>%
      mutate(!!class_name := if_else(rank > ceiling(n_subplots / 2) | prevalence < 0.5,
                                     species,
                                     "mixed"),
             !!prop_name := floor(rank) / n_subplots) %>%
      ungroup() %>%
      select(plot_id,
             !!measure,
             !!prop_name,
             !!class_name)
  }

