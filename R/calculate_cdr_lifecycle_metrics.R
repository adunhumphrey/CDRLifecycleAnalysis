#' Function to Calculate and Summarize CDR Lifecycle Metrics
#'
#' This function estimates lifecycle emissions metrics for Carbon Dioxide Removal (CDR) technologies
#' based on a supplied database and includes options for filtering, visualization, and saving results.
#'
#' @param db_path Path to the GCAM database.
#' @param db_name Name of the GCAM database.
#' @param dat_file Name of the .dat file to load.
#' @param scenario_list List of scenarios to include.
#' @param region_list List of regions to filter, default is all regions.
#' @param output_path Path to save output files.
#' @param output_type Output format, either "csv" or "list". Default is "list".
#' @param create_plots Logical, if TRUE, generates plots for the results. Default is TRUE.
#' @param metric The metric to visualize in plots. One of "Total_Lifecycle_Emissions",
#'   "Net_Removal_Efficiency", "NCRR". Default is "NCRR".
#' @param ncol Number of columns for the facets in the plots. Default is 2.
#' @param nrow Number of rows for the facets in the plots. Default is NULL.
#' @param selected_years For time-based visualizations, a vector of years to include, or NULL for all years.
#' @return A list containing the calculated lifecycle metrics.
#' @import dplyr rgcam ggplot2
#' @export
calculate_cdr_lifecycle_metrics <- function(db_path,
                                            db_name,
                                            dat_file,
                                            scenario_list,
                                            region_list = NULL,
                                            output_path,
                                            output_type = c("csv", "list"),
                                            create_plots = TRUE,
                                            metric = "NCRR",
                                            ncol = 2,
                                            nrow = NULL,
                                            selected_years = NULL) {
  # Validate output_type
  output_type <- match.arg(output_type, several.ok = TRUE)

  # Validate output path
  if (!dir.exists(output_path)) {
    stop("The specified output_path does not exist.")
  }

  # Define the CDR query directly in the function
  CDR_query <- "<?xml version='1.0'?>
  <queries>
    <aQuery>
      <all-regions/>
      <supplyDemandQuery title='CDR by tech'>
        <axis1 name='technology'>technology</axis1>
        <axis2 name='Year'>physical-output[@vintage]</axis2>
        <xPath buildList='true' dataName='output' group='false' sumAll='false'>
          *[@type='sector' and @name='CDR_regional']/*[@type='subsector']/*[@type='technology' and not(@name='unsatisfied CDR demand')]/physical-output/node()
        </xPath>
      </supplyDemandQuery>
    </aQuery>
  </queries>"

  # Create a temporary XML file for the query
  query_file <- tempfile(fileext = ".xml")
  writeLines(CDR_query, query_file)

  # Establish database connection and add scenario
  CDR_tech <- addScenario(localDBConn(db_path, db_name), paste0(dat_file, ".dat"), scenario_list, query_file)

  # Query the database for CDR outputs
  CDR_Output <- getQuery(CDR_tech, "CDR by tech")

  # Filter by region if region_list is provided
  if (!is.null(region_list)) {
    CDR_Output <- CDR_Output %>% filter(region %in% region_list)
  }

  # Load the CDR_Lifecycle dataset from the package
  data("CDR_Lifecycle", package = "CDRLifecycleAnalysis")

  # Helper function to calculate lifecycle metrics
  calculate_metrics <- function(data) {
    data %>%
      group_by(across(-value)) %>%
      summarize(
        Total_Lifecycle_Emissions = sum(Total_Lifecycle_Emissions * value, na.rm = TRUE),
        Avg_Net_Removal_Efficiency = mean(Net_Removal_Efficiency * value, na.rm = TRUE),
        Avg_NCRR = mean(NCRR * value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Join the CDR Output with lifecycle metrics data
  joined_data <- dplyr::left_join(CDR_Output, CDR_Lifecycle, by = c("technology" = "Technology"))

  # Calculate metrics by year, technology, and region
  Metrics_total_year <- calculate_metrics(joined_data %>% group_by(scenario, region, year))
  Metrics_by_tech_year <- calculate_metrics(joined_data %>% group_by(scenario, region, technology, year))
  Metrics_cum_tech <- calculate_metrics(joined_data %>% group_by(scenario, region, technology))
  Metrics_cum_total <- calculate_metrics(joined_data %>% group_by(scenario, region))

  # Save CSVs if specified
  if ("csv" %in% output_type) {
    write.csv(Metrics_total_year, file.path(output_path, "Metrics_total_year.csv"), row.names = FALSE)
    write.csv(Metrics_by_tech_year, file.path(output_path, "Metrics_by_tech_year.csv"), row.names = FALSE)
    write.csv(Metrics_cum_tech, file.path(output_path, "Metrics_cum_tech.csv"), row.names = FALSE)
    write.csv(Metrics_cum_total, file.path(output_path, "Metrics_cum_total.csv"), row.names = FALSE)
  }

  results <- list(
    Metrics_total_year = Metrics_total_year,
    Metrics_by_tech_year = Metrics_by_tech_year,
    Metrics_cum_tech = Metrics_cum_tech,
    Metrics_cum_total = Metrics_cum_total
  )

  # Create plots if requested
  if (create_plots) {
    message("Generating and saving visualizations...")

    # Visualize cumulative metrics by technology
    visualize_results(
      data = Metrics_cum_tech,
      type = "cum_tech",
      metric = metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )

    # Visualize total metrics by year
    visualize_results(
      data = Metrics_total_year,
      type = "total_year",
      metric = metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      selected_years = selected_years,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )
  }

  # Return results as a list if specified
  if ("list" %in% output_type || length(output_type) == 0) {
    return(results)
  }
}
