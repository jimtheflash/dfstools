
library(tidyverse)
library(xml2)
library(rvest)
load_all()


# FIRST ATTEMPT: USING WORLD GOLF RANKINGS --------------------------------



# event_url <- 'http://www.owgr.com/en/Events/EventResult.aspx?eventid=7997'
# event_list <- event_url %>%
#   read_html() %>%
#   html_nodes("table") %>%
#   html_table(header = TRUE, fill = TRUE) %>%
#   `[[`(1)
# names(event_list) <- as.character(event_list[1, ])
# event_list <- event_list[-1, ]


# m2 <- 'http://www.owgr.com/en/Ranking/PlayerProfile.aspx?playerID=9531'
# m2 %>%
#   read_html() %>%
#   html_nodes("#player_banner li, #player_banner b") %>%
#   html_text()

# owgr_table <- 'http://www.owgr.com/ranking?pageNo=1&pageSize=All&country=All' %>%
#   read_html() %>%
#   html_nodes("table") %>%
#   html_table(trim = TRUE, fill = TRUE) %>%
#   `[[`(1)
#
# tidy_owgr <- owgr_table %>%
#   # inner_join(event_list %>% select(Name)) %>%
#   mutate(Name = gsub("\\([^()]+\\)", '', Name)) %>%
#   add_tidy_playernames(platform = 'draftkings', sport = 'golf') %>%
#   mutate(tidy_playername = case_when(tidy_playername == 'paulwaring' ~ 'paulwaring',
#                                      tidy_playername == 'ahmedskaik' ~ 'ahmedskaik',
#                                      tidy_playername == 'davidcoupland' ~ 'davecoupland',
#                                      tidy_playername == 'lorenzofiliposcalise' ~ 'lorenzoscalise',
#                                      tidy_playername == 'pepanglesros' ~ 'pepangles',
#                                      tidy_playername == 'calumhill' ~ 'calumhill',
#                                      tidy_playername == 'mjdaffue' ~ 'matthysdaffue',
#                                      tidy_playername == 'gonzalofdezcastano' ~ 'gonzalofernandezcastano',
#                                      tidy_playername == 'jamesdupreezjnr' ~ 'jamesdupreez',
#                                      tidy_playername == 'chriscannon' ~ 'christophercannon',
#                                      tidy_playername == 'benjaminhenrypoke' ~ 'benjaminpoke',
#                                      tidy_playername == 'michaelpalmer' ~ 'michaelgpalmer',
#                                      tidy_playername == 'makgethamazibuko' ~ 'makhethamazibuko',
#                                      tidy_playername == 'totothimbajnr' ~ 'totothimba',
#                                      tidy_playername == 'benjaminfollettsmith' ~ 'benjaminfolletsmith',
#                                      tidy_playername == 'sebastianjmunoz' ~ 'sebastianmunoz',
#                                      tidy_playername == 'williamgordon' ~ 'willgordon',
#                                      tidy_playername == 'zhangxinjun' ~ 'xinjunzhang',
#                                      tidy_playername == 'sunghoonkang' ~ 'sungkang',
#                                      tidy_playername == 'santiagotarrioben' ~ 'santiagotarrio',
#                                      TRUE ~ tidy_playername))
#
#
# salaries <- parse_salaries(path = '/Users/jim/Downloads/DKSalaries (1).csv', sport = 'golf', platform = 'draftkings')
#
# tidy_salaries <- salaries %>%
#   add_tidy_playernames(platform = 'draftkings', sport = 'golf')
#
# test <- tidy_salaries %>%
#   left_join(tidy_owgr, by = 'tidy_playername')
#
# merged <- tidy_salaries %>%
#   left_join(tidy_owgr %>% select(tidy_playername, `Average Points`), by = "tidy_playername") %>%
#   mutate(AvgPointsPerGame = `Average Points`) %>%
#   select(-`Average Points`, -tidy_playername) %>%
#   filter(!is.na(AvgPointsPerGame)) %>%
#   filter(AvgPointsPerGame != '-') %>%
#   filter(!grepl('Langasque', Name)) %>%
#   filter(!grepl('Lewis', Name)) %>%
#   filter(!grepl('Lieser', Name))
#
# write_csv(merged, '/Users/jim/Downloads/to_optim4.csv')


# SECOND ATTEMPT: USING VEGAS ODDS ----------------------------------------

## seems to work well!
#### FUNCTIONS ####
get_dk_tourney_odds <- function(json_xhr_url) {
  # get the json
  big_json <- jsonlite::fromJSON(json_xhr_url)
  # start parsing!
  ## at least in one version of this, the tournament outcome odds are located here. grab the decimal for this process
  tourney_odds <- big_json[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]][[1]][["offerSubcategory"]][["offers"]][[1]][[1]][["outcomes"]][[1]]
  tourney_odds$tidy_playername <- gsub('[^[:alnum:]]', '', tolower(tourney_odds$participant))
  tourney_odds$tidy_odds_dk <- as.numeric(tourney_odds$oddsDecimal)

  return(tourney_odds)

}

get_fd_tourney_odds <- function(json_xhr_url) {
  # get the json
  big_json <- jsonlite::fromJSON(json_xhr_url)
  # start parsing!
  ## at least in one version of this, the tournament outcome odds are located here, in decimal as the price column
  tourney_odds <- big_json[["events"]][["markets"]][[1]][["selections"]][[1]]
  tourney_odds$tidy_playername <- gsub('[^[:alnum:]]', '', tolower(tourney_odds$name))
  tourney_odds$tidy_odds_fd <- as.numeric(tourney_odds$price)

  return(tourney_odds)
}

get_bovada_tourney_odds <- function(event_url) {
  events <- jsonlite::fromJSON(event_url)

  output_list <- list()

  for (i in 1:nrow(events)) {
    event_row <- events[i, ]
    # get the paths
    path_list <- as.data.frame(event_row$path)
    # get the tournaments
    tournament_paths <- path_list[path_list$type == 'TOURNAMENT', ]
    if (nrow(tournament_paths) == 0) {
      next
    }

    # get the event data
    event_list <- as.data.frame(event_row$events)
    event_list <- event_list[event_list$live == TRUE, ]
    if (nrow(event_list) == 0) {
      next
    }

    event_output <- list()
    for (j in 1:nrow(event_list)) {
      event_list_row <- event_list[j, ]
      disp_group_list <- as.data.frame(event_list_row$displayGroups)
      default_markets <- disp_group_list[disp_group_list$defaultType == TRUE, ]
      markets <- as.data.frame(default_markets$markets)
      outcomes <- as.data.frame(markets$outcomes)
      if (is.null(outcomes$price)) {
        next
      }
      tidy_outcomes <- outcomes %>%
        select(-price) %>%
        bind_cols(outcomes$price) %>%
        mutate(event = event_list_row$description,
               tidy_playername = gsub('[^[:alnum:]]', '', tolower(description)),
               tidy_odds_bv = as.numeric(decimal))

      event_output[[length(event_output) + 1]] <- tidy_outcomes
    }

    to_output <- dplyr::bind_rows(event_output)

    output_list[[event_list_row$description]] <- tidy_outcomes
  }

  return(output_list)
}

#### RUN ####
# get salaries
salaries <- parse_salaries(path = '/Users/jim/Downloads/DKSalaries (5).csv',
                           sport = 'golf', platform = 'draftkings') %>%
  add_tidy_playernames(platform = 'draftkings', sport = 'golf')


# dk and fd have separate urls by tournament
dk_euro_open_url <- 'https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/1658/full?includePromotions=true&format=json'
dk_lpga_open_url <- 'https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/1996/full?includePromotions=true&format=json'
fd_euro_open_url <- 'https://il.sportsbook.fanduel.com/cache/psmg/UK/60543.3.json'

# bovada has separate urls by sports
bovada_golf_url <- 'https://www.bovada.lv/services/sports/event/v2/events/A/description/golf'

## dk
dk_euro_odds <- get_dk_tourney_odds(dk_euro_open_url) %>%
  mutate(tidy_playername = case_when(tidy_playername == 'jordansmith' ~ 'jordanlsmith',
                                     tidy_playername == 'samivälimäki' ~ 'samivalimaki',
                                     TRUE ~ tidy_playername)) %>%
  select(tidy_playername, tidy_odds_dk)

dk_lpga_odds <- get_dk_tourney_odds(dk_lpga_open_url) %>%
  mutate(tidy_playername = case_when(tidy_playername == 'allymcdonald' ~ 'allyewing',
                                     tidy_playername == 'brookemhenderson' ~ 'brookehenderson',
                                     TRUE ~ tidy_playername)) %>%
  select(tidy_playername, tidy_odds_dk)

## fd
fd_euro_odds <- get_fd_tourney_odds(fd_euro_open_url) %>%
  select(tidy_playername, tidy_odds_fd)

## bv
bv_odds <- get_bovada_tourney_odds(bovada_golf_url)
bv_euro_odds <- bv_odds$`DP World Tour Championship` %>%
  mutate(tidy_playername = case_when(tidy_playername == 'jordansmith' ~ 'jordanlsmith',
                                     tidy_playername == 'samivälimäki' ~ 'samivalimaki',
                                     TRUE ~ tidy_playername)) %>%
  select(tidy_playername, tidy_odds_bv)

bv_lpga_odds <- bv_odds$`U.S. Women's Open` %>%
  mutate(tidy_playername = case_when(tidy_playername == 'allymcdonald' ~ 'allyewing',
                                     tidy_playername == 'brookemhenderson' ~ 'brookehenderson',
                                     tidy_playername == 'minyounglee2' ~ 'minyoung2lee',
                                     TRUE ~ tidy_playername)) %>%
  select(tidy_playername, tidy_odds_bv)

#### MERGE ####
euro_merged <- salaries %>%
  left_join(dk_euro_odds) %>%
  left_join(fd_euro_odds) %>%
  left_join(bv_euro_odds)

lpga_merged <- salaries %>%
  left_join(dk_lpga_odds) %>%
  left_join(bv_lpga_odds)

#### ADJUST ODDS ####
euro_avg_odds <- euro_merged %>%
  select(starts_with('tidy_odds')) %>%
  rowMeans(na.rm = TRUE)

euro_merged$avg_odds <- euro_avg_odds
euro_merged$prob <- 1/euro_merged$avg_odds

euro_merged$AvgPointsPerGame <- round(euro_merged$prob, 6)


lpga_avg_odds <- lpga_merged %>%
  select(starts_with('tidy_odds')) %>%
  rowMeans(na.rm = TRUE)

lpga_merged$avg_odds <- lpga_avg_odds
lpga_merged$prob <- 1/lpga_merged$avg_odds

lpga_merged$AvgPointsPerGame <- round(lpga_merged$prob, 6)

#### OUTPUT ####
euro_output <- euro_merged %>%
  filter(!is.na(AvgPointsPerGame)) %>%
  filter(tidy_playername != 'graememcdowell') %>%
  select(-starts_with('tidy'), -avg_odds, -prob)
euro_output_string <- paste0('/Users/jim/Documents/euro_sal_', round(as.numeric(Sys.time())), '.csv')
write_csv(euro_output, euro_output_string)

lpga_output <- lpga_merged %>%
  filter(!is.na(AvgPointsPerGame)) %>%
  select(-starts_with('tidy'), -avg_odds, -prob)
lpga_output_string <- paste0('/Users/jim/Documents/lpga_sal_', round(as.numeric(Sys.time())), '.csv')
write_csv(lpga_output, lpga_output_string)
