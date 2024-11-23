#' Summarize Lifecycle Emissions Data
#'
#' This function provides a summary of lifecycle emissions, net removal efficiency,
#' and NCRR for each CDR technology in the dataset.
#'
#' @param data A data frame containing lifecycle emissions data.
#' @return A summarized data frame grouped by technology.
#' @examples
#' # Example usage:
#' lifecycle_summary(CDR_Lifecycle)
lifecycle_summary <- function(data) {
  if (!all(c("Technology", "Total_Lifecycle_Emissions", "Net_Removal_Efficiency", "NCRR") %in% names(data))) {
    stop("The dataset must contain the required columns: 'Technology', 'Total_Lifecycle_Emissions', 'Net_Removal_Efficiency', and 'NCRR'.")
  }

  summary <- data %>%
    group_by(Technology) %>%
    summarize(
      Total_Emissions = sum(Total_Lifecycle_Emissions),
      Avg_Net_Removal_Efficiency = mean(Net_Removal_Efficiency),
      Avg_NCRR = mean(NCRR)
    )

  return(summary)
}
