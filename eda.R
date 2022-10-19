####### EDA ########
source("field_plot.R")
library(tidyverse)
library(gganimate)


###### ANIMATE A PLAY ########

pff <- read_csv("pffScoutingData.csv")
week7 <- read_csv("week7.csv")
plays <- read_csv("plays.csv")

#plays %>%
  filter(possessionTeam == "GB",
         defensiveTeam == "WAS") %>% view()

valid_game_ids <- unique(week7$gameId)

sample_play_info <- sample_n(plays %>% filter(gameId %in% valid_game_ids), 1) %>% select(gameId, playId)
sample_play_id <- sample_play_info %>% pull(playId)
sample_game_id <- sample_play_info %>% pull(gameId)

sample_play <- week7 %>%
  filter(gameId == 2021102405, playId == 1874) %>%
  inner_join(pff) %>%
  mutate(rushing_qb = pff_role == "Pass Rush",
         snap_frame = max(frameId[event == "ball_snap"])) %>%
  filter(frameId < snap_frame)

football_field +
  geom_point(data = sample_play, aes(x = x, y = y, fill = team, group = nflId, color = rushing_qb), alpha = 0.7,
             size = 6.5) +
  geom_text(data = sample_play, aes(x = x, y = y, label = jerseyNumber), color = "white",
            vjust = 0.36, size = 3.5) +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("#654321", "#c60c30"), guide = FALSE) +
  transition_time(frameId)  +
  ease_aes('linear') -> animation

play.length.ex <- length(unique(sample_play$frameId))

animate(animation, fps = 10, nframes = play.length.ex + 5, width = 1000, end_pause = 5) -> anim

anim_save("animated_play.gif")


# IDEAS #

# expected threats and expected threats neutralized by blocking
# threat: hurry, hit, or sack
# based on speed relative to QB at end point of plays where there was a threat, but calculate them in real time

# How many pass rushers by play?

pff %>%
  group_by(gameId, playId) %>%
  summarize(pass_rushers = sum(pff_role == "Pass Rush")) %>%
  arrange(desc(pass_rushers)) %>%
  filter(gameId %in% game_ids)
