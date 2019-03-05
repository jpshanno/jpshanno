# Create grid for a FIA plot and calculate weights with some commone IDW powers

# bbox_grid <-
#   data.frame(x = rep(144:-144, each = 289),
#              y = rep(-144:144, times = 289))
#
#
# distance_from_center <-
#   sqrt((bbox_grid[["x"]] - 0)^2 + (bbox_grid[["y"]] - 0)^2)
#
# cells_in_plot <-
#   bbox_grid[which(distance_from_center <= 144), ]
#
# subplot_centers <-
#   purrr::map(list(c(0, 0),
#                   c(0, 120),
#                   c(120*sin(pi/3), -120 * cos(pi/3)),
#                   c(-120*sin(pi/3), -120 * cos(pi/3))),
#              matrix,
#              nrow = 1)
#
# idw.power <-
#   c(0.25, 0.5, 1, 2, 3)
#
# weights <-
#   purrr::map(idw.power,
#              ~purrr::map(subplot_centers,
#                          function(coords){
#
#                            x <- coords[1]
#                            y <- coords[2]
#
#                            bbox_x <- cells_in_plot[["x"]]
#                            bbox_y <- cells_in_plot[["y"]]
#
#                            distances <-
#                              sqrt((bbox_x - x)^2 + (bbox_y - y)^2)
#
#                            in_plot <-
#                              which(distances <= 24)
#
#                            distances[in_plot] <-
#                              0
#
#                            distances[-in_plot] <-
#                              distances[-in_plot] - 24
#
#                            weights <-
#                              1 / distances^.x
#
#                            weights[which(is.infinite(weights))] <-
#                              1
#
#                            weights
#                          }
#              ) %>%
#                purrr::reduce(cbind, deparse.level = 0)
#   )
#
# weights <-
#   purrr::map(weights,
#              function(x){
#                colnames(x) <-
#                  paste0("subplot_", 1:4)
#                x
#              }) %>%
#   purrr::set_names(paste0("weights_", idw.power))
#
# purrr::imap(weights,
#             ~assign(.y, .x, envir = .GlobalEnv))
#
#
# save(list = c(names(weights), "cells_in_plot"),
#      file = paste(file.path(Sys.getenv("HOME"), "Package_Development", "jpshanno", "R"),
#             "sysdata.rda",
#            sep = .Platform$file.sep))
