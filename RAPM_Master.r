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

# Single regular season PbP file (Add support for multiple later)
# matchup_base <- read.csv(unz("Data/pbp2016reg2.zip","pbp2016reg2.csv"),
#                         header = F, na.strings = "\\N", stringsAsFactors = F)

# Sample PbP file (for faster prototyping)
matchup_base <- read.csv(gzfile("Data/Sample_PbP.csv.gz"),
                         na.strings = "NA", stringsAsFactors = F)

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

has_ball_focus_play_code <- c("drb",
                         "fg2",
                         "fg2x",
                         "fg3",
                         "fg3x",
                         "ft",
                         "ftx",
                         "orb",
                         "tov"
                         )     #These codes indicate focus team has ball


has_ball_focus_play_type <- c("OFFENSIVE_CHARGE_FOUL",
                             "OFFENSIVE_FOUL"
                              )   #These types indicate focus team has ball


has_ball_other_tm_play_type <- c("CLEAR_PATH_FOUL",
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

has_ball_next_play_code <- c("jump_ball",
                             "period_start"
                             )  #  whoever has the ball on next play should have here

has_ball_prev_play_code <- c("period_end")
  #  whoever has the ball on previous play should have here

has_ball_next_play_type <- c("JUMP_BALL_VIOLATION")
  #  whoever has the ball on next play should have here

has_ball_prev_play_type <- c("AWAY_FROM_PLAY_FOUL",
                             "FLAGRANT_FOUL_TYPE_1",
                             "FOUL",
                             "LOOSE_BALL_FOUL",
                             "TECHNICAL_FOUL",
                             "DELAY_OF_GAME_VIOLATION",
                             "DOUBLE_LANE_VIOLATION"
                             ) #  whoever has the ball on previous play should have here


# These three sets define who has the ball on almost every play

matchup_base$has_ball[matchup_base$play_code %in% has_ball_focus_play_code] <-
  matchup_base$focus_team_id[matchup_base$play_code %in% has_ball_focus_play_code]

matchup_base$has_ball[matchup_base$play_type %in% has_ball_focus_play_type] <-
  matchup_base$focus_team_id[matchup_base$play_type %in% has_ball_focus_play_type]

matchup_base$has_ball[matchup_base$play_type %in% has_ball_other_tm_play_type] <-
  matchup_base$other_team_id[matchup_base$play_type %in% has_ball_other_tm_play_type]


# These four sets define who has the ball based on who has it
# on the next or previous play (Iterate 5 times)

for (i in 1:5) {
matchup_base$has_ball[matchup_base$play_code %in% has_ball_next_play_code] <-
  matchup_base$has_ball[lag(matchup_base$play_code %in% has_ball_next_play_code, n=1, default = FALSE)]

matchup_base$has_ball[matchup_base$play_type %in% has_ball_next_play_type] <-
  matchup_base$has_ball[lag(matchup_base$play_type %in% has_ball_next_play_type, n=1, default = FALSE)]

matchup_base$has_ball[matchup_base$play_code %in% has_ball_prev_play_code] <-
  matchup_base$has_ball[lead(matchup_base$play_code %in% has_ball_prev_play_code, n=1, default = FALSE)]

matchup_base$has_ball[matchup_base$play_type %in% has_ball_prev_play_type] <-
  matchup_base$has_ball[lead(matchup_base$play_type %in% has_ball_prev_play_type, n=1, default = FALSE)]
}

NAs <- matchup_base[is.na(matchup_base$has_ball),]



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






