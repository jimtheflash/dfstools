#' Append column of tidy team abbreviations
#' @param df data.frame with team abbreviation column
#' @param lookup character, path to csv lookup
#' @param platform character, which dfs platform to use
#' @param sport character, which sport
#' @return data.frame with column "tidy_teamabbrev" appended for easier joining
#' @export
add_tidy_teamabbrev <- function(df = NULL,
                                lookup = system.file('extdata', 'teamabbrev_lu.csv', package = 'dfstools'),
                                platform = NULL, sport = NULL) {

  # read the lookup table
  lu <- read.csv(lookup, stringsAsFactors = FALSE)
  # engineer the platform team abbreviation column names
  if (platform %in% c('draftkings', 'draftkings-showdown', 'draftkings-tiers')) {
    platform <- 'draftkings'
  }
  platform_col <- paste0(platform, '_teamabbrev')
  target <- paste0(sport, '_teamabbrev')
  # subset lookup table for joining
  lu_sub <- lu[, c(target, platform_col)]
  lu_sub <- lu_sub[!is.na(lu_sub[[target]]), ]
  # set the column in df based on platform and sport
  if (platform == 'draftkings') {
    column <- 'TeamAbbrev'
    }
  if (platform == 'fanduel') {
    column <- 'Team'
  }
  if (platform %in% c('fantasypros', 'rotogrinders')) {
    column <- 'teamabbrev'
    }

  # create a column that overlaps across tables
  df[[platform_col]] <- df[[column]]

  # merge
  merged <- merge(df, lu_sub)
  # tidy
  merged$tidy_teamabbrev <- tolower(trimws(merged[[target]]))
  merged[[target]] <- NULL

  # return
  return(merged)
}

#' Append column of tidy player names
#' @param df data.frame with player column
#' @param lookup character, path to csv lookup
#' @param platform character, which dfs platform to use
#' @param sport character, which sport
#' @return data.frame with column "tidy_playername" appended for easier joining
#' @export
add_tidy_playernames <- function(df,
                                 lookup = NULL,
                                 platform = NULL, sport = NULL) {
  # set the column in df based on platform and sport
  if (platform %in% c('draftkings', 'draftkings-showdown', 'draftkings-tiers')) {
    column <- 'Name'
    }
  if (platform == 'fanduel') {
    column <- 'Nickname'
  }
  if (platform %in% c('fantasypros', 'rotogrinders')) {
    column <- 'name'
  }

  # tidy
  #### if its nfl dst, combine the tidy_teamabbrev and tidy_position as the tidy_playername
  if (sport == 'nfl') {
    if (is.null(df$tidy_position) | is.null(df$tidy_teamabbrev)) {
      stop('run add_tidyposition() and add_tidy_teamabbrev() prior to this step')
    }
    dst_tidynames <- paste0(df$tidy_teamabbrev, df$tidy_position)
    df[[column]] <- ifelse(df$tidy_position == 'dst', dst_tidynames, df[[column]])
  }

  df$tidy_playername <- tolower(trimws(df[[column]]))
  df$tidy_playername <- gsub("[^[:alnum:]]", "", df$tidy_playername)
  df$tidy_playername <- gsub("jr$|sr$|iv$|iii$|ii$", "", df$tidy_playername)
  # return
  return(df)
}

#' Append column of tidy player positions
#' @param df data.frame with position column
#' @param platform character, which dfs platform to use
#' @param sport character, which sport
#' @return data.frame with column "tidy_position" appended for easier joining
#' @export
add_tidy_position <- function(df,
                              lookup = NULL,
                              platform = NULL, sport = NULL) {

  # set the column in df based on platform and sport
  if (platform %in% c('draftkings', 'draftkings-showdown', 'draftkings-tiers')) {
    column <- 'Position'
  }
  if (platform == 'fanduel') {
    column <- 'Position'
  }
  if (platform %in% c('fantasypros', 'rotogrinders')) {
    column <- 'position'
  }
  df$tidy_position <- df[[column]]
  df$tidy_position <- tolower(trimws(df$tidy_position))
  df$tidy_position <- gsub("[^[:alnum:]]", "", df$tidy_position)

  return(df)
}

#' Append column of tidy projections
#' @param df data.frame with position column
#' @param column character, which column to use
#' @return data.frame with column "tidy_projection" appended for easier joining
#' @export
add_tidy_projection <- function(df, column = NULL) {
  df$tidy_projection <- df[[column]]
  return(df)
}

#' Append column of tidy contest identifiers
#' @param df data.frame with contest identifier column
#' @param platform character, which platform the contests are from
#' @param sport character, which sport contests are from
#' @return data.frame with column "tidy_contest" appended for easier counting
#' @export
add_tidy_contest <- function(df, platform = NULL, sport = NULL) {
  if (platform %in% c('draftkings', 'draftkings-showdown', 'draftkings-tiers')) {
    column <- 'Contest.ID'
  }
  if (platform == 'fanduel') {
    column <- 'contest_id'
  }
  df$tidy_contest <- df[[column]]
  return(df)
}

#' Append column of tidy contest entry identifiers
#' @param df data.frame with entry identifier column
#' @param platform character, which platform the entries are from
#' @param sport character, which sport entries are from
#' @return data.frame with column "tidy_entry" appended for easier counting
#' @export
add_tidy_entry <- function(df, platform = NULL, sport = NULL) {
  if (platform %in% c('draftkings', 'draftkings-showdown', 'draftkings-tiers')) {
    column <- 'Entry.ID'
  }
  if (platform == 'fanduel') {
    column <- 'entry_id'
  }
  df$tidy_entry <- df[[column]]
  return(df)
}

