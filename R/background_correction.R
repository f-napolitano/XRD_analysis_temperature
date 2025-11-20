# R/background_correction.R

#' Linear interpolation between two points (possible not in use!!!)
#'
#' @param x1,y1 Coordinates of first point
#' @param x2,y2 Coordinates of second point
#' @param x X-value to interpolate at
#' @return Interpolated y-value
#' @export
interp_points <- function(x1, y1, x2, y2, x) {
  (y2 - y1) * (x - x1) / (x2 - x1) + y1
}


#' Create linear background function for subtraction
#'
#' @keywords internal
#' @param a,b coefficients of linear function y = a + b * x
#' @param x X-value to calculate function at
#' @return Background value at x
.create_background_model <- function(x, a, b) {
  # Internal function - not exported for users
  # Implementation details
  a + b * x
}

#' Subtract background from data
#' @param data XRD pattern, already cleaned (2theta range filtered to be centered at the maximum of Bragg peak)
#' @param max_position 2theta value where the maximum of Bragg peak is
#' @param dist_from_center Distance to the 2theta position of the Bragg peak maximum
#'                         to exclude for the background fitting. The idea is that all 2theta in the range:
#'                         max_position - dist_from_center <= 2theta <= max_position + dist_from_center
#'                         are well inside the Bragg peak, and those outiside this range could be safely
#'                         considered at the background.
#' @returns A list with an expanded "data" data frame with two additional columns:
#'          "bkg" with the background values at each 2theta and "int_rest" with the
#'          background subtracted intensity values, therefore columns (2theta, int_rest) are the
#'          background subtracted XRD pattern
#' @export
bkg_subtract_sym_function <- function(data,
                                      max_position,
                                      dist_from_center = 0.7) {

  bkg_zone <- data[data$twoTheta <= max_position - dist_from_center |
                     data$twoTheta >= max_position + dist_from_center, ]

  fitbkground <- stats::lm(intensity ~ twoTheta, bkg_zone)

  data$bkg <- .create_background_model(data$twoTheta,
                                       fitbkground$coefficients[1],
                                       fitbkground$coefficients[2]) # background built

  data$int_rest <- data$intensity - data$bkg # subtract background from intensity

  list(result = data,
       background_function = fitbkground)
}


