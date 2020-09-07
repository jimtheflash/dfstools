#' Prepare a file for dfs optimization
#' @param salaries data.frame of salary data
#' @param projections data.frae with projections
#' @param platform character, dfs platform
#' @param sport character, sport
#' @return data.frame to be used as input for optimization
#' @export
structure_for_optimization <- function(salaries = NULL, projections = NULL,
                                       platform = NULL, sport = NULL) {

  # merge salaries and projections by the tidy columns
  all_names <- unique(c(names(salaries), names(projections)))
  tidy_cols <- all_names[grepl('tidy_', all_names)]
  tidy_cols <- tidy_cols[!grepl('projection', tidy_cols)]
  merged <- merge(salaries, projections, by = tidy_cols)

  # identify the points column by platform and sport
  if (platform == 'draftkings') {
    if (sport == 'nfl') {
      column <- 'AvgPointsPerGame'
    }
  }
  if (platform == 'fanduel') {
    if (sport == 'nfl') {
      column <- 'FPPG'
    }
  }
  # replace target with tidy projection
  merged[[column]] <- merged$tidy_projection

  # output the columns from salaries
  output <- merged[, names(merged) %in% names(salaries)]
  output <- output[, !grepl('tidy_', names(output))]
  output <- output[, !grepl(platform, names(output))]

  names(output) <- gsub('\\.', ' ', names(output))

  return(output)
}
