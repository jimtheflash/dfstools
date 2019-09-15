#' Get dfs player projections from Rotogrinders.com
#' @param sport character, which sport to use? supports nfl so far, with nba on the way
#' @param platform character, which dfs platform to use? supports draftkings so far, with fanduel on the way
#' @return data.frame of players and projections for selected platform
#' @export
get_rotogrinders_projections <- function(sport = NULL, platform = NULL) {
  sports <- c('nba', 'nfl')
  if (!(sport %in% sports)) {
    stop('sport not available')
  }
  output_list <- list()
  if (sport == 'nfl') {
    positions <- c('qb', 'rb', 'wr', 'te', 'defense', 'kicker')
    for (pos in positions) {
      scrape_url <- paste0('https://rotogrinders.com/projected-stats/nfl-', pos, '.csv?site=', platform)
      scraped <- read.csv(scrape_url, header = FALSE, stringsAsFactors = FALSE)
      names(scraped) <- c('name', 'salary', 'teamabbrev', 'position', 'opponent', 'ceiling', 'floor', 'proj')
      output_list[[pos]] <- scraped
    }
  }
  if (sport == 'nba') {
    print('coming soon!')
    return()
  }
  output_df <- do.call(rbind, output_list)
  return(output_df)
}