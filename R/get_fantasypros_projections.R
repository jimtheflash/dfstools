#' Get dfs player projections from fantasypros.com
#' @param sport character, which sport to use? supports nfl so far, with nba on the way
#' @param from_csv character, if not NULL then path to the csv or csv's with projections
#' @param supported_sports character vector of leagues supported
#' @param csv_headers character, names of the columns in rotogrinders csv's
#' @param nfl_positions character, position names
#' @return data.frame of players and projections for selected platform
#' @export
get_fantasypros_projections <- function(sport = NULL,
                                        from_csv = NULL,
                                        supported_sports  = c('nfl'),
                                        scraped_headers = c('rank', 'wsis', 'name', 'opp', 'best', 'worst', 'avg', 'stdev', 'proj', 'notes'),
                                        csv_headers = c("RK","PLAYER NAME", "TEAM", "OPP","MATCHUP (?)","START/SIT","PROJ. FPTS"),
                                        nfl_positions  = c('qb', 'rb', 'wr', 'te', 'dst')) {

  if (!(sport %in% supported_sports)) {
    stop('sport not available')
  }

  if (!is.null(from_csv)) {
    output_list <- list()
    for (i in from_csv) {
      proj <- read.csv(i, col.names = csv_headers, stringsAsFactors = FALSE)

      # set the position
      if (grepl('RB', i)) {
        pos <- 'RB'
      }
      if (grepl('WR', i)) {
        pos <- 'WR'
      }
      if (grepl('TE', i)) {
        pos <- 'TE'
      }
      if (grepl('QB', i)) {
        pos <- 'QB'
      }
      if (grepl('DST', i)) {
        pos <- 'DST'
      }
      proj$position <- pos

      # set the team if it isn't its own column
      # teams <- unlist(stringr::str_match_all(proj$PLAYER.NAME, "(?<=\\().+?(?=\\))"))
      # proj$TEAM <- teams

      # set team name column for easier cleaning
      proj$teamabbrev <- proj$TEAM

      # set the name
      names_split <- strsplit(proj$PLAYER.NAME, ' (', fixed = TRUE)
      player_names <- unlist(lapply(names_split, '[[', 1))
      proj$name <- player_names

      # store the output
      output_list[[i]] <- proj
    }

    output_df <- do.call(rbind, output_list)

    # fix proj here instead of in loop cuz of naming :-|
    output_df$proj <- as.numeric(output_df$PROJ..FPTS)

    # get rid of NA projections
    output_df <- output_df[!is.na(output_df$proj), ]

    return(output_df)

  } else {
    output_list <- list()
    if (sport == 'nfl') {
      for (pos in nfl_positions) {
        # make url based on position group
        if (pos %in% c('rb', 'te', 'wr')) {
          scrape_url <- paste0('https://www.fantasypros.com/nfl/rankings/ppr-', pos, '.php?print=true')
        } else {
          scrape_url <- paste0('https://www.fantasypros.com/nfl/rankings/', pos, '.php?print=true')
        }

        scraped <- xml2::read_html(scrape_url)
        scraped_tables <- rvest::html_table(scraped, fill = TRUE)
        rank_table <- scraped_tables[[1]]
        names(rank_table) <- scraped_headers
        rank_table$position <- pos
        output_list[[pos]] <- rank_table
      }
    }
    if (sport == 'nba') {
      print('requires csv input, please supply path to csv with projection in from_csv arg')
      return()
    }

    output_df <- do.call(rbind, output_list)
    # re-engineer the name column
    name_list <- strsplit(output_df$name, ' ')
    first_names <- unlist(lapply(name_list, '[[', 1))
    last_names <- unlist(lapply(name_list, function(x) `[[`(x, length(x) - 1)))
    team_abbrevs <- unlist(lapply(name_list, function(x) `[[`(x, length(x))))
    output_df$name <- NULL

    # get the right DST team abbreviation
    dst_team_abbrevs <- ifelse(grepl('\\)', last_names), strsplit(last_names, '\\)'), NA)
    dst_team_abbrevs <- unlist(lapply(dst_team_abbrevs, function(x) `[[`(x, length(x))))

    # put the re-engineered columns back in the data
    output_df$name <- paste0(first_names, ' ', last_names)
    output_df$teamabbrev <- ifelse(output_df$position %in% c('dst'), dst_team_abbrevs, team_abbrevs)

    # filter out free agents
    output_df <- output_df[output_df$teamabbrev != 'FA', ]

    # fix the opponents
    opps <- unlist(lapply(strsplit(output_df$opp, '  '), '[[', 2))
    output_df$opp <- opps

    # return
    return(output_df)
  }
}
