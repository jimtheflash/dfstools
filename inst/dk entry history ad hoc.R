## THIS SHOULD BE A SHINY DASHBOARD JIM YOU DUMMY

library(tidyverse)

# make some helper functions
strip_dollars <- function(x) {
  as.numeric(gsub('\\$', '', x))
}



# get history
entry_history <- read_csv('/Users/jim/Downloads/draftkings-contest-entry-history (1).csv')

# filter to dates
filtered <- entry_history %>%
  # filter(Sport == 'NFL') %>%
  filter(Contest_Date_EST >= '2020-01-01') %>%
  # filter(Contest_Date_EST < '2020-12-01') %>%
  mutate(Game_Type = if_else(grepl('Tiers:', Game_Type), 'Tiers_extra', Game_Type),
         Entry_Fee = strip_dollars(Entry_Fee),
         Winnings_Non_Ticket = strip_dollars(Winnings_Non_Ticket))

# group by sports
aggregate_entry_data <- function(entry_data, ...) {
  grouped <- entry_data %>%
    group_by(...) %>%
    summarise(entries = n_distinct(Entry_Key),
              contests = n_distinct(Contest_Key),
              fees = sum(Entry_Fee, na.rm = TRUE),
              winnings_dollars = sum(Winnings_Non_Ticket, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(profit = winnings_dollars - fees,
           profit_per_entry = profit / entries,
           profit_per_contest = profit / contests,
           net = winnings_dollars / fees) %>%
    #filter(!is.na(net)) %>%
    arrange(desc(net))
  grouped
}

filtered %>%
  aggregate_entry_data(., Sport) %>%
  View()

