#' create optimized lineups for draftkings tiers
#' @importFrom magrittr %>%
#' @param to_optimize data.frame of player projections, salary and roster info
#' @param lineups number of lineups to create
#' @param max_exposure number maximum exposure %
#' @param max_repeated_players number NOT CURRENTLY USED
#' @param platform character should always be "draftkings-tiers"
#' @param sport character
#' @return data.frame
#' @export
create_optimized_tiers <- function(to_optimize = NULL, lineups = NULL,
                                   max_exposure = NULL, max_repeated_players = NULL,
                                   platform = NULL, sport = NULL) {

  if (platform != 'draftkings-tiers') {
    stop('only supported platform = drafkings-tiers')
  }

  if (sport != 'nfl') {
    stop('only supported sport = nfl')
  }

  splitted_dfs <- split.data.frame(to_optimize, to_optimize$`Roster Position`)

  expanded_grid <- expand.grid(
    T1 = unique(splitted_dfs$T1$`Name + ID`),
    T2 = unique(splitted_dfs$T2$`Name + ID`),
    T3 = unique(splitted_dfs$T3$`Name + ID`),
    T4 = unique(splitted_dfs$T4$`Name + ID`),
    T5 = unique(splitted_dfs$T5$`Name + ID`),
    T6 = unique(splitted_dfs$T6$`Name + ID`),
    stringsAsFactors = FALSE)

  merged <- expanded_grid %>%
    dplyr::left_join(splitted_dfs$T1 %>%
                       dplyr::select(`Name + ID`,
                                     t1game = `Game Info`, t1pts = AvgPointsPerGame),
                     by = c("T1" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T2 %>%
                       dplyr::select(`Name + ID`,
                                     t2game = `Game Info`, t2pts = AvgPointsPerGame),
                     by = c("T2" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T3 %>%
                       dplyr::select(`Name + ID`,
                                     t3game = `Game Info`, t3pts = AvgPointsPerGame),
                     by = c("T3" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T4 %>%
                       dplyr::select(`Name + ID`,
                                     t4game = `Game Info`, t4pts = AvgPointsPerGame),
                     by = c("T4" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T5 %>%
                       dplyr::select(`Name + ID`,
                                     t5game = `Game Info`, t5pts = AvgPointsPerGame),
                     by = c("T5" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T6 %>%
                       dplyr::select(`Name + ID`,
                                     t6game = `Game Info`, t6pts = AvgPointsPerGame),
                     by = c("T6" = "Name + ID")) %>%
    dplyr::mutate(total_pts = t1pts + t2pts + t3pts + t4pts + t5pts + t6pts) %>%
    dplyr::select(dplyr::matches('T[1-8]|total|game', ignore.case = FALSE)) %>%
    dplyr::arrange(dplyr::desc(total_pts))

  # filter out cases with only 1 game
  games <- merged %>%
    dplyr::select(dplyr::matches('game')) %>%
    apply(., 1, function(x) length(unique(x)))

  merged <- merged %>%
    dplyr::filter(games > 1)

  # apply other parameters
  max_obs <- max_exposure * lineups

  output_lineups <- merged[1, ]

  for (i in 2:nrow(merged)) {

    new_row <- merged[i, ]

    if (sum(output_lineups$T1 == new_row$T1) < max_obs &
        sum(output_lineups$T2 == new_row$T2) < max_obs &
        sum(output_lineups$T3 == new_row$T3) < max_obs &
        sum(output_lineups$T4 == new_row$T4) < max_obs &
        sum(output_lineups$T5 == new_row$T5) < max_obs &
        sum(output_lineups$T6 == new_row$T6) < max_obs) {

      # if none of the players exceed the threshold, then add the next lineup to the output
      output_lineups <- rbind(output_lineups, new_row)

      message(nrow(output_lineups), ' lineups stored')

      if (nrow(output_lineups) == lineups) {
        break
      }
    }

  }
  return(output_lineups)

}
