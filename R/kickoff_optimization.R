#' Kickoff dfs optimization pipeline
#' @param SPORT character, which dfs sport/league
#' @param PLATFORM character, which dfs platform
#' @param PROJECTIONS character, which projections to use
#' @param PROJ_COL character, which column in projections
#' @param LU_MULT numeric, LU_MULT * number of entries = number of lineups generated
#' @param PROJ_PATH character, path to projections csv
#' @param SALARY_PATH character, path to player salary csv
#' @param ENTRY_PATH character, path to entry csv
#' @param TEMP_PATH character, path to directory for writing temp files
#' @param MAX_EXP numeric, maximum exposure to any individual player in lineups
#' @param MAX_REPEATED_PLAYERS numeric, maximum number of repeated combinations of players
#' @return outputs a csv to be entered into dfs site
#' @export
kickoff_optimization <- function(SPORT = NULL, PLATFORM = NULL, 
                                 PROJECTIONS = NULL, PROJ_COL = NULL, LU_MULT = NULL,
                                 PROJ_PATH = NULL, SALARY_PATH = NULL, ENTRY_PATH = NULL, TEMP_PATH = NULL,
                                 MAX_EXP = NULL, MAX_REPEATED_PLAYERS = NULL) {
  
  # get and tidy salary info
  salaries <- parse_salaries(path = SALARY_PATH, sport = SPORT, platform = PLATFORM)
  salaries <- add_tidy_teamabbrev(salaries, sport = SPORT, platform = PLATFORM)
  salaries <- add_tidy_position(salaries, sport = SPORT, platform = PLATFORM)
  salaries <- add_tidy_playernames(salaries, sport = SPORT, platform = PLATFORM)
  
  # get projections
  if (PROJECTIONS == 'rotogrinders') {
    projections <- get_rotogrinders_projections(sport = SPORT, platform = PLATFORM, from_csv = PROJ_PATH) 
  }
  # tidy projections
  projections <- add_tidy_teamabbrev(projections, sport = SPORT, platform = PROJECTIONS)
  projections <- add_tidy_position(projections, sport = SPORT, platform = PROJECTIONS)
  projections <- add_tidy_playernames(projections, sport = SPORT, platform = PROJECTIONS)
  projections <- add_tidy_projection(projections, column = PROJ_COL)

  # structure output for optimization
  to_optimize <- structure_for_optimization(salaries = salaries, projections = projections,
                                            sport = SPORT, platform = PLATFORM)
  to_optim_path <- paste0(TEMP_PATH, 'to_optim.csv')
  write.csv(to_optimize, to_optim_path)
  
  # read entries to determine how many lineups should be generated
  entries <- parse_entries(ENTRY_PATH, sport = SPORT, platform = PLATFORM)
  entries <- add_tidy_contest(entries, sport = SPORT, platform = PLATFORM)
  entries <- add_tidy_entry(entries, sport = SPORT, platform = PLATFORM)
  LINEUPS <- round(max(table(entries$tidy_contest)) * LU_MULT)
  
  # optimize
  optim_path <- system.file('python', 'optimizer.py', package = 'dfstools')
  optim_string <- paste0('/Users/jim/anaconda3/bin/python ', optim_path, ' ', #execute the optimizer
                         to_optim_path, ' ', #arg1=location of salary info for optimizin
                         PLATFORM, ' ', #arg2=platform, i.e. site, for entering these lineups
                         SPORT, ' ', #arg3=sport, i.e. professional sporting league
                         LINEUPS, ' ', #arg4=nlineups, number of lineups to generate
                         MAX_EXP, ' ', #arg5=maximum exposure, max exposure to any one player
                         MAX_REPEATED_PLAYERS #arg6=maximum repeated players
                         )
  system(optim_string)
  
  # import optimized lineups
  optimized_path <- paste0(TEMP_PATH, 'optimized.csv')
  optimized_lineups <- parse_optimized_lineups(optimized_path, platform = PLATFORM, sport = SPORT)
  
  # structure optimized entries
  optimized_entries <- structure_optimized_entries(entries, optimized_lineups)
  output_path <- paste0(TEMP_PATH, 'to_', PLATFORM, '.csv')
  write.csv(optimized_entries, output_path, quote = FALSE, row.names = FALSE)
}