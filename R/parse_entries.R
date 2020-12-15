#' Parse dfs entry files
#' @param path character, path to salary file
#' @param sport character, which sports league? Supports nfl, nba on the way
#' @param platform charcter, which dfs platform? Supports draftkings, fanduel on the way
#' @return data.frame of entry info
#' @export
parse_entries <- function(path = NULL, sport = NULL, platform = NULL) {

  entries <- read.csv(path, stringsAsFactors = FALSE)
  entries$X <- NULL
  entries$Instructions <- NULL

  if (platform %in% c('draftkings', 'draftkings-showdown', 'draftkings-tiers', 'draftkings-tiers-pm')) {
    entries$Entry.ID <- suppressWarnings(as.numeric(entries$Entry.ID))
    entries <- entries[!is.na(entries$Entry.ID), ]
  }
  if (platform == 'fanduel') {
    entries$entry_id <- suppressWarnings(as.numeric(entries$entry_id))
    entries <- entries[!is.na(entries$entry_id), ]
  }
  return(entries)
}
