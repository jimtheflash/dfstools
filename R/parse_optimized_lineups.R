#' Parse dfs optimized lineup files
#' @param path character, path to optimized lineups file
#' @param sport character, which sports league? Supports nfl, nba on the way
#' @param platform charcter, which dfs platform? Supports draftkings, fanduel on the way
#' @return data.frame of optimized lineups
#' @export
parse_optimized_lineups <- function(path = NULL, sport = NULL, platform = NULL) {
  lineups <- read.csv(path, stringsAsFactors = FALSE)
  if (platform == 'fanduel') {
    if (sport == 'nfl') {
      # fix defense
      lineups$DEF <- lineups$D
      lineups$D <- NULL
      # get rid of player names and parentheses in entries, might be mucking it up
      pos_cols <- names(lineups)[!names(lineups) %in% c('Budget', 'FPPG')]
      for (p in pos_cols) {
        split_col <- strsplit(lineups[[p]], '\\(')
        split_col <- unlist(lapply(split_col, '[[', 2))
        split_col <- gsub('\\)', '', split_col)
        lineups[[p]] <- split_col
      }
    }
  }
  return(lineups)
}