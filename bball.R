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


# Next is the court view of throws. For this we need play by play data 

# So what variables do we need for the court shots?


rm(list=ls())
PbP <- PbPmanipulation(PbP.BDB)

subdata <- subset(PbP, player=="Stephen Curry")
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75

shotchart(data=subdata, x="xx", y="yy", type=NULL,
          scatter=TRUE)

shotchart(data=subdata, x="xx", y="yy", z="result", type=NULL,
          scatter=TRUE)

shotchart(data=subdata, x="xx", y="yy", z="playlength", 
          num.sect=5, type="sectors", scatter = TRUE)

shotchart(data=subdata, x="xx", y="yy", z="playlength", 
          num.sect=5, type="sectors", scatter=FALSE, result="result")


# It looks like we have everything, so first we subset the 
# data for season 2023 and Curry only:

nba_pbp %>%  
  filter(season == 2023 & athlete_id_1 == 3975 & shooting_play == TRUE &!grepl("Free Throw",type_text)) %>% 
  select(coordinate_x,coordinate_y,coordinate_x_raw,coordinate_y_raw,scoring_play,score_value,
         shooting_play,clock_minutes) %>% 
  mutate(result=as.factor(if_else(scoring_play == TRUE, "made","missed")))-> curry_shots

subdata <- curry_shots %>% as.data.frame()
subdata$xx <- subdata$coordinate_x_raw-25
subdata$yy <- subdata$coordinate_y_raw-44

shotchart(data=subdata, x="xx", y="yy",z="result" ,type=NULL,scatter=TRUE) -> p1

shotchart(data=subdata, x="xx", y="yy", type="density-raster",
          scatter=FALSE)-> p2

shotchart(data=subdata, x="xx", y="yy", z="clock_minutes", 
          num.sect=5, type="sectors", scatter=FALSE, result="result") -> p3

p1+p2

p3
library(patchwork)

# Modelling:

nba_player_box %>%
  select(athlete_id,athlete_display_name) %>% 
  distinct() %>%
  arrange(athlete_id)-> player_ids

player_ids %>% 
  group_by(athlete_id) %>%
  mutate(row = row_number()) %>%
  filter(row==1) %>% 
  ungroup() %>% 
  select(athlete_id,athlete_display_name) -> player_ids

nba_team_box %>% 
  filter(season <= 2023 & team_display_name == "Golden State Warriors") %>%
  select(game_id,opponent_team_name) -> game_ids

nba_pbp %>%  
  filter(season <= 2023 & 
           athlete_id_1 == 3975 &
           shooting_play == TRUE &
           !grepl("Free Throw",type_text)) %>% 
  select(game_id,coordinate_x_raw,coordinate_y_raw,scoring_play,
         clock_minutes, athlete_id_2,clock_minutes,clock_seconds,qtr) %>% 
  left_join(player_ids,by=c("athlete_id_2"="athlete_id")) %>% 
  inner_join(game_ids, by=join_by(game_id)) %>% 
  select(-athlete_id_2) %>% 
  rename(oponent_athlete=athlete_display_name) %>% 
  mutate(oponent_athlete = if_else(is.na(oponent_athlete),"no direct oponent",oponent_athlete),
         scoring_play = fct_rev(as_factor(scoring_play))) -> base_steph_pbp

# Dataset is ready to model:

library(tidymodels)

# Create splits:

set.seed(123)
steph_split <- initial_split(base_steph_pbp, prop = 0.8, strata = scoring_play)
steph_train <- training(steph_split)
steph_test <- testing(steph_split)

steph_folds <- vfold_cv(steph_train, v = 10, strata = scoring_play)

# Preprocessing:
# Add new step to address NAs

steph_recipe <- recipe(scoring_play ~ ., data = steph_train) %>%
  update_role(game_id, new_role = "ID") %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) 

xgb_spec <- boost_tree(
  trees = 5000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

steph_workflow <- workflow() %>%
  add_recipe(steph_recipe) %>%
  add_model(xgb_spec)

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), steph_train),
  learn_rate(),
  size = 30
)

doParallel::registerDoParallel() # Activate parallel computing
set.seed(1234)

xgb_results <-  tune_grid(
  steph_workflow,
  resamples = steph_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean)) +
  geom_point(alpha = 0.8, show.legend = FALSE, color="midnightblue") +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")+
  scale_color_brewer(palette = "Spectral")+
  theme_minimal()

# Select best model and finalise workflow:

best_auc_model <- select_best(xgb_results,"roc_auc")

final_xgb <- finalize_workflow(
  steph_workflow,
  best_auc_model)

final_xgb %>%
  fit(data = steph_train) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point") +
  theme_minimal()

final_res <- last_fit(final_xgb, steph_split)

# This can also be done with  the final_res object

final_xgb %>%
  fit(data = steph_train) -> fit_steph

augment(fit_steph,steph_test)%>% 
  roc_curve(truth = scoring_play, .pred_TRUE) %>% 
  autoplot()

final_res %>%
  collect_predictions() %>%
  roc_curve(scoring_play, `.pred_TRUE`) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(linewidth = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    linewidth = 1.2)+
  theme_minimal()

collect_metrics(final_res)

collect_predictions(final_res) %>%
  conf_mat(scoring_play, .pred_class) %>%
  autoplot()+
  theme_minimal()

# Consult this page for more info on how to 
# predict with the model:

https://www.tidymodels.org/start/recipes/

# Get the data for the new season:
# So we filtered the data for the Golden State Warriors:

  nba_team_box %>% 
  filter(season == 2024 & team_display_name == "Golden State Warriors") %>%
  select(game_id,opponent_team_name) -> game_ids
  
  
  nba_pbp %>%  
  filter(season == 2024 & 
           athlete_id_1 == 3975 &
           shooting_play == TRUE &
           !grepl("Free Throw",type_text)) %>% 
  select(game_id,coordinate_x_raw,coordinate_y_raw,scoring_play,
         clock_minutes, athlete_id_2,clock_minutes,clock_seconds,qtr) %>% 
  left_join(player_ids,by=c("athlete_id_2"="athlete_id")) %>% 
  inner_join(game_ids, by=join_by(game_id)) %>% 
  select(-athlete_id_2) %>% 
  rename(oponent_athlete=athlete_display_name) %>% 
  mutate(oponent_athlete = if_else(is.na(oponent_athlete),"no direct oponent",oponent_athlete),
         scoring_play = fct_rev(as_factor(scoring_play))) -> base_steph_pbp_2024

  
# Should probably first fit to the whole dataset and then predict:
  
final_xgb %>%
    fit(data = base_steph_pbp) -> fit_steph

predict(fit_steph,base_steph_pbp_2024)

augment(fit_steph, base_steph_pbp_2024) %>%
  roc_curve(truth = scoring_play, .pred_TRUE) %>% 
  autoplot()

augment(fit_steph, base_steph_pbp_2024) %>%
  roc_auc(truth = scoring_play, .pred_TRUE)

augment(fit_steph, base_steph_pbp_2024) -> steph_preds_2024

# Plot the predictions on the court:

# Flow 1: Predictions

steph_preds_2024 %>% 
  select(coordinate_x_raw,coordinate_y_raw,.pred_class,clock_minutes) %>%
  mutate(result=as.factor(if_else(.pred_class == TRUE,"made","missed")))->curry_shots_2024

subdata <- curry_shots_2024 %>% as.data.frame()
subdata$xx <- subdata$coordinate_x_raw-25
subdata$yy <- subdata$coordinate_y_raw-44

shotchart(data=subdata, x="xx", y="yy",z="result" ,type=NULL,scatter=TRUE) +ggtitle(label = "   Testing title") -> p1

shotchart(data=subdata, x="xx", y="yy", type="density-raster",
          scatter=FALSE)-> p2

shotchart(data=subdata, x="xx", y="yy", z="clock_minutes", 
          num.sect=5, type="sectors", scatter=FALSE, result="result") -> p3

# Flow 2: Actual


steph_preds_2024 %>% 
  select(coordinate_x_raw,coordinate_y_raw,scoring_play,clock_minutes) %>%
  mutate(result=as.factor(if_else(scoring_play == TRUE,"made","missed")))->curry_shots_2024

subdata <- curry_shots_2024 %>% as.data.frame()
subdata$xx <- subdata$coordinate_x_raw-25
subdata$yy <- subdata$coordinate_y_raw-44

shotchart(data=subdata, x="xx", y="yy",z="result" ,type=NULL,scatter=TRUE) -> p1

shotchart(data=subdata, x="xx", y="yy", type="density-raster",
          scatter=FALSE)-> p2

shotchart(data=subdata, x="xx", y="yy", z="clock_minutes", 
          num.sect=4, type="sectors", scatter=FALSE, result="result") -> p3


# Do the above, but with false positives and false negatives.


