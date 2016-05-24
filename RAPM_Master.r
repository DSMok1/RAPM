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




