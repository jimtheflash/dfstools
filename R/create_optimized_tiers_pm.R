#' create optimized lineups for draftkings tiers
#' @importFrom magrittr %>%
#' @param to_optimize data.frame of player projections, salary and roster info
#' @param lineups number of lineups to create
#' @param max_exposure number maximum exposure %
#' @param max_repeated_players number NOT CURRENTLY USED
#' @param platform character should always be "draftkings-tiers-pm"
#' @param sport character
#' @return data.frame
#' @export
create_optimized_tiers_pm <- function(to_optimize = NULL, lineups = NULL,
                                   max_exposure = NULL, max_repeated_players = NULL,
                                   platform = NULL, sport = NULL) {

  if (platform != 'draftkings-tiers-pm') {
    stop('only supported platform = drafkings-tiers-pm')
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
    T7 = unique(splitted_dfs$T7$`Name + ID`),
    T8 = unique(splitted_dfs$T8$`Name + ID`),
    stringsAsFactors = FALSE)

  merged <- expanded_grid %>%
    dplyr::left_join(splitted_dfs$T1 %>%
                       dplyr::select(`Name + ID`, t1pts = AvgPointsPerGame),
                     by = c("T1" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T2 %>%
                       dplyr::select(`Name + ID`, t2pts = AvgPointsPerGame),
                     by = c("T2" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T3 %>%
                       dplyr::select(`Name + ID`, t3pts = AvgPointsPerGame),
                     by = c("T3" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T4 %>%
                       dplyr::select(`Name + ID`, t4pts = AvgPointsPerGame),
                     by = c("T4" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T5 %>%
                       dplyr::select(`Name + ID`, t5pts = AvgPointsPerGame),
                     by = c("T5" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T6 %>%
                       dplyr::select(`Name + ID`, t6pts = AvgPointsPerGame),
                     by = c("T6" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T7 %>%
                       dplyr::select(`Name + ID`, t7pts = AvgPointsPerGame),
                     by = c("T7" = "Name + ID")) %>%
    dplyr::left_join(splitted_dfs$T8 %>%
                       dplyr::select(`Name + ID`, t8pts = AvgPointsPerGame),
                     by = c("T8" = "Name + ID")) %>%
    dplyr::mutate(total_pts = t1pts + t2pts + t3pts + t4pts + t5pts + t6pts + t7pts + t8pts) %>%
    dplyr::select(dplyr::matches('T[1-8]|total', ignore.case = FALSE)) %>%
    dplyr::arrange(dplyr::desc(total_pts))

  max_obs <- max_exposure * lineups

  output_lineups <- merged[1, ]

  for (i in 2:nrow(merged)) {

    new_row <- merged[i, ]

    if (sum(output_lineups$T1 == new_row$T1) < max_obs &
        sum(output_lineups$T2 == new_row$T2) < max_obs &
        sum(output_lineups$T3 == new_row$T3) < max_obs &
        sum(output_lineups$T4 == new_row$T4) < max_obs &
        sum(output_lineups$T5 == new_row$T5) < max_obs &
        sum(output_lineups$T6 == new_row$T6) < max_obs &
        sum(output_lineups$T7 == new_row$T7) < max_obs &
        sum(output_lineups$T8 == new_row$T8) < max_obs) {

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
