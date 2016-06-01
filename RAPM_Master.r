#
# RAPM Master File
#

# Daniel Myers (@DSMok1)
# Started 2016-05-24

# Components will be added as additional files containing functions as they
# are completed.

###  Required Packages ####

require(dplyr)
require(magrittr)

###  Load Data ####

# header data per email from Dave Corby, BBRef
headers <- c("game_id","date_game","play_id","quarter",
             "time_elapsed_game","time_elapsed_play",
             "player1_id","player2_id","player3_id",
             "play_code","play_type",
             "visitor_team_id","home_team_id","focus_team_id","other_team_id",
             "visitor_lineup","home_lineup",
             "fg_dist","visitor_score","home_score","play_pts"
)

# Single regular season PbP file
matchup_base <- read.csv(unz("Data/pbp2016reg2.zip","pbp2016reg2.csv"),
                         header = F, na.strings = "\\N")

names(matchup_base) <- headers

matchup_base %<>% arrange(game_id,play_id)  # Arrange file in sequence

play_code_filter <- c("sub","timeout")  # Play codes to filter out

matchup_base %<>% filter(!(play_code %in% play_code_filter)) %>%
  filter(!(is.na(play_code) & time_elapsed_play == 0))



###  Summarize Play Codes and Types ####

play_code_summary <- group_by(matchup_base,play_code,play_type) %>%
  summarise (
    number = length(play_id)
  )

###  Assign Possession End Plays ####

focus_has_ball_play_code <- c("drb",
                         "fg2",
                         "fg2x",
                         "fg3",
                         "fg3x",
                         "ft",
                         "ftx",
                         "orb",
                         "tov"
                         )     #These codes indicate focus team has ball


focus_has_ball_play_type <- c("OFFENSIVE_CHARGE_FOUL",
                             "OFFENSIVE_FOUL"
                              )   #These types indicate focus team has ball


focus_not_ball_play_type <- c("CLEAR_PATH_FOUL",
                              "DEF_3_SEC_TECH_FOUL",
                              "INBOUND_FOUL",
                              "PERSONAL_BLOCK_FOUL",
                              "PERSONAL_FOUL",
                              "PERSONAL_TAKE_FOUL",
                              "SHOOTING_BLOCK_FOUL",
                              "SHOOTING_FOUL",
                              "DEF_GOALTENDING_VIOLATION",
                              "KICKED_BALL_VIOLATION",
                              "LANE_VIOLATION"
                              )   #These types indicate focus team does not have ball







play_code_play_end <- c("tov","period_end")
ft_play_end <- c("FREE_THROW_1_OF_1",
                 "FREE_THROW_2_OF_2",
                 "FREE_THROW_3_OF_3")

matchup_base$poss_end <- 0
matchup_base$poss_end[matchup_base$play_code=="tov"] <- 1
matchup_base$poss_end[matchup_base$play_code=="period_end"] <- 1
matchup_base$poss_end[lead(matchup_base$play_code=="drb")] <- 1
matchup_base$poss_end[matchup_base$play_code=="ft" &
                        matchup_base$play_type %in% "FREE_THROW_1_OF_1"] <- 1
matchup_base$poss_end[matchup_base$play_code=="ft" &
                        matchup_base$play_type=="FREE_THROW_2_OF_2"] <- 1
matchup_base$poss_end[matchup_base$play_code=="ft" &
                        matchup_base$play_type=="FREE_THROW_3_OF_3"] <- 1


###  Overall Game Data ####

# Assign win probability as 1, 0, or 0.5 (if it went to overtime)

game_data <- group_by(matchup_base,game_id) %>%
  summarise (
    date_game = first(date_game),
    visitor_team_id = first(visitor_team_id),
    home_team_id = first(home_team_id),
    max_time = max(time_elapsed_game),
    minutes = max_time/600,
    quarters = max(quarter),
    visitor_final_score = max(visitor_score),
    home_final_score = max(home_score),
    visitor_win = if (quarters > 4) {
      0.5} else if (visitor_final_score > home_final_score) {
        1} else {
          0},
    home_win = 1-visitor_win
  )






