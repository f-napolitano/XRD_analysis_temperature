# R/analysis_core.R

#' Calculate parameters of a given peak
#'
#' @param data data frame with the XRD pattern
#' @param xmin,xmax minimum and maximum values of where the desired Bragg peak is.
#'                  Used to isolate the desired Bragg peak from a others.
#' @param alpha_beta flag to control the type of radiation source
#'                   =2 if monochromatic, =3 if Kalpha doublet
#' @param config List, configuration file
#' @importFrom stats splinefun integrate
#' @returns a list with:
#'              "pattern_lim" a data frame with the 2theta reduced and background
#'              subtracted XRD pattern and column expanded to include the values of
#'              the spline fit function over data, the normalized pattern, and
#'              the normalized spline fitted data
#'              "integ_result" a list with the integration results of the Bragg peak (through spline fit)
#'              "area_total" a double with the result of calculated area of the Bragg peak
#'              "integ_result_norm" a list with the integration results of the normalized Bragg peak (through spline fit)
#'              "twoTheta_max" a double with the 2theta position of the Bragg peak maximum
#'              "beta" a double with the integral breadth of the normalized Bragg peak
#'              "max_peak_rest" a double with the max intensity of the Bragg peak after background subtraction
#'              "beta_unorm" a double with the integral breadth of the Bragg peak before normalization.
#' @export
area_function <- function(data, xmin, xmax, alpha_beta, config) {

  pattern_lim <- data[data$twoTheta > xmin & data$twoTheta < xmax, ]  # isolate peak

  index_max <- which.max(as.data.frame(pattern_lim)$intensity)
  intensity_max <- max(pattern_lim$intensity)
  twoTheta_max <- pattern_lim$twoTheta[index_max]

  pattern_lim <- pattern_lim[pattern_lim$twoTheta >= twoTheta_max - 0.8 &
                               pattern_lim$twoTheta <= twoTheta_max + 0.8, ]  # centering on peak, hacerlo restando por cantidad de canales en vez de angulo fijo N * 0.0131308....º

  background_substracted <- bkg_subtract_sym_function(pattern_lim, twoTheta_max)  # background subtraction
  pattern_lim <- background_substracted[["result"]]


  funct_peak_spline <- stats::splinefun(x = pattern_lim$twoTheta,
                                        y = pattern_lim$int_rest)

  pattern_lim$interp <- funct_peak_spline(pattern_lim$twoTheta) # add column with fitted function

  # area
  lower <- min(pattern_lim$twoTheta)
  upper <- max(pattern_lim$twoTheta)

  result <- stats::integrate(funct_peak_spline, lower, upper)
  area <- result$value * 2 / alpha_beta


  twotheta_max <- pattern_lim$twoTheta[which.max(funct_peak_spline(pattern_lim$twoTheta))] # redefine of 2theta max after background subtraction
  intensity_max <- max(funct_peak_spline(pattern_lim$twoTheta))
  beta_unnorm <- area / intensity_max       # beta for peak (NOT normalized)

  pattern_lim$norm <- pattern_lim$int_rest / intensity_max      # normalize peak
  funct_peak_spline <- stats::splinefun(x = pattern_lim$twoTheta,
                                        y = pattern_lim$norm)
  pattern_lim$norm_interp <- funct_peak_spline(pattern_lim$twoTheta)

  # normalized area

  result_norm <- stats::integrate(funct_peak_spline,
                                  lower,
                                  upper) # Integrate the function

  # integral breadth
  beta <- result_norm$value * 2 / alpha_beta

  # Temperature
  #temperature <- data[["Temperature"]]

  # wrapping it up

  if (config$parallel$enabled == FALSE ) {
    list(pattern_reduction = pattern_lim,
         background = background_substracted[["background_function"]]$coefficients,
         integ_result = result, # results and parameters from integral calculation
         area_total = area,
         integ_result_norm = result_norm,
         twotheta_max = twotheta_max,
         beta = beta,
         max_peak_rest = max(pattern_lim$int_rest),
         beta_unnorm = beta_unnorm
         )
  } else {
    list(pattern_reduction = pattern_lim,
         temperature = mean(pattern_lim$temperature),
         direction = pattern_lim$direction[1],
         background = background_substracted[["background_function"]]$coefficients,
         integ_result = result,
         area_total = area,
         integ_result_norm = result_norm,
         twotheta_max = twotheta_max,
         beta = beta,
         max_peak_rest = max(pattern_lim$int_rest),
         beta_unnorm = beta_unnorm
         )
   #return(area)
  }
}


#' Results aggregation
#' @param nested_list A nested list with the results from several XRD diffraction patterns
#'                    analysis. Main list goes along the different patterns, inside of each pattern
#'                    list is another list with the pattern-specific results
#' @param parallel Logical, "nested_list" came from parallel or sequential pipeline
#' @importFrom purrr map list_rbind imap
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @returns A tibble with its columns being relevant peak parameters and each row corresponding
#'          to a XRD pattern.
#' @export
extract_results_function <- function(nested_list, parallel = FALSE) {
  if (parallel) {
    result_df <- nested_list |>
      purrr::map(~ tibble::tibble(direction = .x$direction,
                                  Temperature = .x$temperature,
                                  maximum = .x$max_peak_rest,
                                  beta = .x$beta,
                                  beta_unnorm = .x$beta_unnorm)) |>
      purrr::list_rbind(names_to = "file_name")  # Adds a column with the list names


  }
  else {
    purrr::map(nested_list, function(direction) {
      purrr::imap(direction, function(inner_list, id) {
        tibble::tibble(
          id = as.integer(id),
          Temperature = inner_list$Temperature,
          maximum = inner_list$pattern$max_peak_rest,
          beta = inner_list$pattern$beta,
          beta_unnorm = inner_list$pattern$beta_unnorm
        )
      }) |>
        purrr::list_rbind() |>
        dplyr::arrange(id)  # Ensure to being ordered by id
    })
  }

}
