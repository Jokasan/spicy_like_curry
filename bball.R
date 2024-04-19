# Resources for project:

# Packages:

https://hoopr.sportsdataverse.org/articles/getting-started-hoopR.html
https://rpubs.com/emmartey/bball_ds_with_r_codes

# Load libraries and data:

pacman::p_load(BasketballAnalyzeR,tidyverse,zoo,gt,hoopR)

# We want all data pertaining to Curry. 
# Curry is a point guard that plays for the 
# Golden State Warriors.

tm <- c("BOS","CLE","GSW","HOU")
selTeams <- which(Tadd$team %in% tm)
FF.sel <- fourfactors(Tbox[selTeams,], Obox[selTeams,])

plot(FF.sel)

# OK, so I think the strategy should be the following:

# Use the BasketballAnalyzeR package to visualize & wrangle
# and hoopr to get the data at the desired granularity, always 
# referencing BAR package for data structure.
# An initial analysis suggests that the main structure is 
# df.

# Ok it looks like the play-by-play data will be the
# most useful for our purposes. Let's get that data:

tictoc::tic()
progressr::with_progress({
  nba_pbp <- hoopR::load_nba_pbp(2022:hoopR::most_recent_nba_season())
})
tictoc::toc()

# We retrieve play by play data since 2022:

nba_pbp

# How much data is that?:
glue::glue("{nrow(nba_pbp)} rows of NBA team boxscore data from {length(unique(nba_pbp$game_id))} games.")


# Thats 3,750 games & 7,500 rows.

# Next we get the same but for the player level:

tictoc::tic()
progressr::with_progress({
  nba_player_box <- hoopR::load_nba_player_box(2022:hoopR::most_recent_nba_season())
})
tictoc::toc()

nba_player_box

# How much data is that?:

glue::glue("{nrow(nba_player_box)} rows of NBA player boxscore data from {length(unique(nba_player_box$game_id))} games.")

# Thats 3,750 games & 97,082 rows.


# Next we can get the same stats for the team level:

tictoc::tic()
progressr::with_progress({
  nba_team_box <- hoopR::load_nba_team_box(2022:hoopR::most_recent_nba_season())
})

tictoc::toc()

nba_team_box

# How much data is that?:
glue::glue("{nrow(nba_team_box)} rows of NBA team boxscore data from {length(unique(nba_team_box$game_id))} games.")

# 7,500 rows of NBA team boxscore data from 3,750 games.

# With this triage we have enough to start our analysis,
# Aggregating at different levels of granularity.


# Get Curry Player stat data. Curry ID is 3975.
espn_nba_player_stats(athlete_id = 3975,year=2022,season_type = "regular",total=FALSE)

# create a function using the code in line 88 such that it returns a data frame 
# that has the appends the player stats from 2022 to 2024 in a single dataframe:

get_player_stats <- function(player_id){
  stats <- map_df(2022:2024,~espn_nba_player_stats(athlete_id = player_id,year=.x,season_type = "regular",total=FALSE))
  return(stats)
}


# The meat of this analysis comes from play-by-play data,
# which is the most granular data we have. We can use this

# EDA

# Let's also load the logos of the teams:

team_logos <- readr::read_csv("Logo_info.csv") %>% 
  select(TEAM_LOGO,TEAM_NAME,CONFERENCE) %>% 
  unique() %>% 
 rbind(c("https://upload.wikimedia.org/wikipedia/en/thumb/2/21/Portland_Trail_Blazers_logo.svg/2560px-Portland_Trail_Blazers_logo.svg.png","Portland Trail Blazers", "Western")) %>% 
  mutate(TEAM_NAME=if_else(TEAM_NAME == "Los Angeles Clippers","LA Clippers",TEAM_NAME)) 

team_names <- nba_team_box %>% 
  select(team_name,team_display_name) %>% 
  unique()

# 1. A reactablefmtr table joint. An offshoot of this:
# https://kcuilla.github.io/reactablefmtr/articles/nba_player_ratings.html

# We will make a table in the same fashion that shows only Point Guards.
# And also allow the user to filter for year. Does the nba_player_box
# data have a position column?:

colnames(nba_player_box)

# Ok, so we can filter by position. Let's do that:

nba_player_box %>% 
  filter(athlete_position_name == "Point Guard") -> point_guard_df

# Next we select the stats:

# We will also want an average, across seasons:

point_guard_df %>% 
  select(season,athlete_id,athlete_display_name,athlete_position_name,team_name,
         minutes,field_goals_made,three_point_field_goals_made,free_throws_made,
         rebounds,assists,offensive_rebounds,defensive_rebounds,steals,blocks) -> pg_stats

# Let's take the following stats: 

# Minutes played
# Field goals made
# Three Point Field Goals Made
# Free Throws Made
# rebounds
# assists
# offensive rebounds
# defensive rebounds
# steals
# blocks

# Starting point for reactive table.
pg_stats %>% 
  filter(season == 2023 & !grepl("Team",team_name)) %>%
  group_by(athlete_display_name, team_name) %>% 
  mutate(total_points =    sum(
    field_goals_made + three_point_field_goals_made + free_throws_made,
    na.rm = TRUE
  )) %>% 
  ungroup() %>%
  group_by(athlete_display_name, team_name) %>% 
  summarise(
  minutes = sum(minutes, na.rm = TRUE),
  field_goals_made = mean(field_goals_made, na.rm = TRUE),
  three_point_field_goals_made = mean(three_point_field_goals_made, na.rm = TRUE),
  free_throws_made = mean(free_throws_made, na.rm = TRUE),
  assists = mean(assists, na.rm = TRUE),
  rebounds = mean(offensive_rebounds + defensive_rebounds, na.rm = TRUE),
  total_points = sum(max(total_points))) %>%
  arrange(desc(total_points)) %>% 
  ungroup() %>% 
  left_join(team_names, by = c("team_name" = "team_name")) %>% 
  left_join(team_logos, by = c("team_display_name" = "TEAM_NAME")) %>% 
  select(athlete_display_name,TEAM_LOGO,everything())-> pg_stats

# In the radial plot example the following stats are used ^

# Court view of throws. 

# Don't forget we want the breakdown by season. Ok Viz.1 done.

# Next is the radial plots:

pg_stats

Pbox

Pbox.PG <- subset(Pbox, Player=="Russell Westbrook" |
                    Player=="Stephen Curry" |
                    Player=="Chris Paul" |
                    Player=="Kyrie Irving" |
                    Player=="Damian Lillard" |
                    Player=="Kyle Lowry" |
                    Player=="John Wall" |
                    Player=="Rajon Rondo" |
                    Player=="Kemba Walker")


pg_stats %>% 
  select(team_name,athlete_display_name,field_goals_made,three_point_field_goals_made,free_throws_made,rebounds,assists) %>%
  rename(
    Team = team_name,
    Player = athlete_display_name,
    P2M = field_goals_made,
    P3M = three_point_field_goals_made,
    FTM = free_throws_made,
    REB = rebounds,
    AST = assists) %>% 
  as.data.frame() %>% 
  arrange(desc(P2M)) %>% 
  slice_head(n = 10) -> Pbox.PG
  
attach(Pbox.PG)
X <- data.frame(P2M, P3M, FTM, REB, AST)

detach(Pbox.PG)
radialprofile(data=X, title=Pbox.PG$Player, std=FALSE,ncol.arrange = 5)


# Next is the court view of throws.




