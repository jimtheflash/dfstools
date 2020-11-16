#' Insert optimized lineups into entries
#' @param entries data.frame of entries
#' @param optimized_lineups data.frame of optimized lineups
#' @param platform character platform
#' @param sport character sport
#' @return data.frame of entries with optimized lineups
#' @export
structure_optimized_entries <- function(entries = NULL, optimized_lineups = NULL,
                                        platform = NULL, sport = NULL) {

  # identify overlapping names in entries and optimized_lineups, these are the positions
  positions <- names(entries)[names(entries) %in% names(optimized_lineups)]

  # weight the lineups for selecting
  optimized_lineups$wts <- 1 / as.numeric(row.names(optimized_lineups))

  # iterate through contests, and slot in lineups accordingly
  structured_list <- list()
  contests <- unique(entries$tidy_contest)
  for (ct in contests) {
    subsetted <- entries[entries$tidy_contest == ct, ]
    subsetted <- subsetted[, !(names(subsetted) %in% positions)]
    selected_lineups <-
      optimized_lineups[sample(1:nrow(optimized_lineups),
                               nrow(subsetted),
                               prob = optimized_lineups$wts), ]
    structured <- cbind(subsetted, selected_lineups)
    structured_list[[length(structured_list) + 1]] <- structured
  }
  structured_df <- do.call(rbind, structured_list)
  # fix names
  names(structured_df) <- gsub('.[12]', '', names(structured_df))
  names(structured_df) <- gsub('\\.', ' ', names(structured_df))
  structured_df$tidy_contest <- NULL
  structured_df$tidy_entry <- NULL
  structured_df$wts <- NULL
  structured_df$Budget <- NULL
  structured_df$FPPG <- NULL

  # FIX NAMES based on platform
  if (platform == 'draftkings-tiers') {
    names(structured_df) <-
      c('Entry ID', 'Contest Name', 'Contest ID', 'Entry Fee', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'total_pts')
  }

  if (platform == 'draftkings-showdown') {
    names(structured_df) <- gsub('FLEX.*', 'FLEX', names(structured_df))
  }


  # return
  return(structured_df)
}
