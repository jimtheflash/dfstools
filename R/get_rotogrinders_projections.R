#' Get dfs player projections from Rotogrinders.com
#' @param sport character, which sport to use? supports nfl so far, with nba on the way
#' @param platform character, which dfs platform to use? supports draftkings so far, with fanduel on the way
#' @param from_csv character, if not NULL then path to the csv with projections
#' @param supported_sports character vector of leagues supported
#' @param csv_headers character, names of the columns in rotogrinders csv's
#' @param nfl_positions character, position names
#' @return data.frame of players and projections for selected platform
#' @export
get_rotogrinders_projections <- function(sport = NULL, platform = NULL, from_csv = NULL,
                                         supported_sports  = c('nba', 'nfl'),
                                         csv_headers = c('name', 'salary', 'teamabbrev', 'position', 'opponent', 'ceiling', 'floor', 'proj'),
                                         nfl_positions  = c('qb', 'rb', 'wr', 'te', 'defense', 'kicker')) {
  if (!(sport %in% supported_sports)) {
    stop('sport not available')
  }
  
  if (!is.null(from_csv)) {
    output_df <- read.csv(from_csv, col.names = csv_headers, stringsAsFactors = FALSE)
    return(output_df)
  } else {
    output_list <- list()
    if (sport == 'nfl') {
      for (pos in nfl_positions) {
        scrape_url <- paste0('https://rotogrinders.com/projected-stats/nfl-', pos, '.csv?site=', platform)
        scraped <- read.csv(scrape_url, col.names = csv_headers, stringsAsFactors = FALSE)
        output_list[[pos]] <- scraped
      }
    }
    if (sport == 'nba') {
      print('requires csv input, please supply path to csv with projection in from_csv arg')
      return()
    }
    output_df <- do.call(rbind, output_list)
    return(output_df)
  }
}