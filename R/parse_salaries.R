#' Parse dfs player salary files
#' @param path character, path to salary file
#' @param sport character, which sports league? Supports nfl, nba on the way
#' @param platform charcter, which dfs platform? Supports draftkings, fanduel on the way
#' @return data.frame of players with salary info
#' @export
parse_salaries <- function(path = NULL, sport = NULL, platform = NULL) {
  if (platform == 'draftkings') {
    if (sport == 'nfl') {
      # read path; it is a csv but it is ugly, so use readLines()
      chars <- readLines(path)
      # get the table headers
      table_names <- chars[8]
      table_names <- gsub(',,,,,,,,,,,,,,', '', table_names)
      table_names <- unlist(strsplit(table_names, ','))
      # read in the data lines
      table_data <- chars[9:length(chars)]
      table_data <- unlist(lapply(table_data, function(x) gsub(',,,,,,,,,,,,,,', '', x)))
      table_data <- strsplit(table_data, ',')
      # make a table
      tidy_table <- data.frame(do.call(rbind, table_data), stringsAsFactors = FALSE)
      names(tidy_table) <- table_names
      salaries <- tidy_table[, names(tidy_table) != '']
    }
    if (sport == 'nba') {
      stop('coming soon!')
    }
  }
  if (platform == 'fanduel') {
    if (sport == 'nfl') {
      salaries <- read.csv(path, stringsAsFactors = FALSE)
      salaries <- salaries[, !grepl('X', names(salaries))]
    }
    if (sport == 'nba') {
      stop('coming soon!')
    }
  }
  # return
  return(salaries)
}