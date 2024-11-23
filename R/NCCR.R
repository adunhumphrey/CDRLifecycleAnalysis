#' Calculate Net Carbon Removal Ratio (NCRR)
#'
#' This function calculates the Net Carbon Removal Ratio (NCRR) for a given dataset.
#'
#' @param data A data frame containing columns for `Total_CO2_Removed` and `Total_Lifecycle_Emissions`.
#' @return A vector of NCRR values for each row in the dataset.
#' @examples
#' # Example usage:
#' calculate_ncrr(CDR_Lifecycle)
calculate_ncrr <- function(data) {
  if (!all(c("Total_CO2_Removed", "Total_Lifecycle_Emissions") %in% names(data))) {
    stop("The dataset must contain 'Total_CO2_Removed' and 'Total_Lifecycle_Emissions' columns.")
  }

  ncrr <- (data$Total_CO2_Removed - data$Total_Lifecycle_Emissions) / data$Total_Lifecycle_Emissions
  return(ncrr)
}
