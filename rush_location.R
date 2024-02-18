library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(nflreadr)
library(dplyr)
library(ggrepel)
library(nflplotR)
left_rush <- load_pbp(seasons=most_recent_season())|>
  filter(rush==1,
         run_location=="left")|>
  group_by(posteam)|>
  summarize(left_epa_play=mean(epa))
right_rush <- load_pbp(seasons=most_recent_season())|>
  filter(rush==1,
         run_location=="right")|>
  group_by(posteam)|>
  summarize(right_epa_play=mean(epa))
outside_rush <- left_join(left_rush,right_rush,by=c("posteam"))|>
  mutate(difference=right_epa_play-left_epa_play)
outside_rush <- left_join(outside_rush,teams_colors_logos,by=c("posteam"="team_abbr"))
outside_rush|>
  ggplot(aes(x=left_epa_play,y=right_epa_play))+
  geom_hline(yintercept=mean(outside_rush$right_epa_play),linetype="dashed")+
  geom_vline(xintercept=mean(outside_rush$left_epa_play),linetype="dashed")+
  geom_nfl_logos(aes(team_abbr=posteam,width=0.065,alpha=10))+
  labs(x="EPA/Play on Runs to the Left",
       y="EPA/Play on Runs to the Right",
       title = "EPA per Play on Rushes to the Left and Right, 2022",
       caption = "By Aariv Iyengar | @AarivAnalytics")+
  theme_bw()+
  theme(plot.title=element_text(face="bold",hjust=0.5,size=22),
        panel.grid.major.y = element_blank())
ggsave('epa_left_right.png',width=14,height=10,dpi="retina")
outside_rushe <- load_pbp(seasons=most_recent_season())|>
  filter(rush==1,
         run_location=="left"|run_location=="right")|>
  group_by(posteam)|>
  summarize(outside_epa_play=mean(epa))
inside_rush <- load_pbp(seasons=most_recent_season())|>
  filter(rush==1,
         run_location=="middle")|>
  group_by(posteam)|>
  summarize(inside_epa_play=mean(epa))
total_rush <- left_join(inside_rush,outside_rushe,by=c("posteam"))|>
  mutate(difference = outside_epa_play-inside_epa_play)
total_rush|>
  ggplot(aes(x=inside_epa_play,y=outside_epa_play))+
  geom_hline(yintercept=mean(total_rush$outside_epa_play),linetype="dashed")+
  geom_vline(xintercept=mean(total_rush$inside_epa_play),linetype="dashed")+
  geom_nfl_logos(aes(team_abbr=posteam,width=0.065,alpha=10))+
  labs(x="EPA/Play on Runs to the Inside",
       y="EPA/Play on Runs to the Outside",
       title = "EPA per Play on Rushes to the Inside and Outside, 2022",
       caption = "By Aariv Iyengar | @AarivAnalytics")+
  theme_bw()+
  theme(plot.title=element_text(face="bold",hjust=0.5,size=22),
        panel.grid.major.y = element_blank())
ggsave('epa_inside_outside.png',width=14,height=10,dpi="retina")
