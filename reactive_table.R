# Reactable table code:
# Need to pick it apart 

library(reactablefmtr)
library(htmltools)
library(crosstalk) # for control filters

### load data from 538
data <- read.csv(file = "RAPTOR_by_team_19-20.csv")

### create shared dataset for crosstalk
crosstalk_data <- SharedData$new(pg_stats)

### crosstalk team filter
team_filter <- filter_select(
  id = "team",
  label = "TEAM",
  sharedData = crosstalk_data,
  group = ~ team_display_name
)

### crosstalk conference filter
conference_filter <- filter_select(
  id = "conf",
  label = "CONFERENCE",
  sharedData = crosstalk_data,
  group = ~ CONFERENCE
)

### crosstalk minutes filter
minutes_filter <- filter_slider(
  id = "minutes",
  label = "MINUTES PLAYED",
  sharedData = crosstalk_data,
  column = ~ minutes,
  ticks = TRUE,
  dragRange = FALSE,
  step = 100,
  width = "60%"
)

### load font from google fonts
htmltools::tags$link(href = "https://fonts.googleapis.com/css?family=Chivo:400,600,700&display=swap", rel = "stylesheet")

nba_table <- reactable(
  crosstalk_data,
  theme = fivethirtyeight(centered = TRUE),
  compact = TRUE,
  columnGroups = list(
    colGroup(name = "OVERALL STATS", columns = c("field_goals_made","three_point_field_goals_made","free_throws_made","assists","rebounds"))
  ),
  showSortIcon = FALSE,
  searchable = TRUE,
  language = reactableLang(
    searchPlaceholder = "SEARCH FOR A PLAYER..."
  ),
  defaultPageSize = 20,
  columns = list(
    team_display_name = colDef(show = FALSE),
    team_name = colDef(show = FALSE),
    CONFERENCE = colDef(show = FALSE),
    TEAM_LOGO = colDef(
      name = "Team",
      maxWidth = 70,
      align = "center",
      cell = embed_img(height = 25, width = 40),
      style = list(borderRight = "1px solid #777")
    ),
    athlete_display_name = colDef(maxWidth = 150,
                                  name = "Player",
                                  # merge the "Name" column with the "Postion" column and place it below
                                  cell = merge_column(pg_stats, "team_name", merged_position = "below"),
                                  style = list(borderRight = "1px solid #777")),
    minutes = colDef(
      name = "Minutes",
      maxWidth = 85,
      align = "center",
      cell = icon_assign(
        pg_stats,
        icon = "stopwatch",
        fill_color = "#555555",
        buckets = 5
      ),
      style = list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)")
    ),
    field_goals_made = colDef(
      name = "AFG",
      maxWidth = 60,
      style = color_scales(pg_stats, colors = c("#fd84a9", "white", "#42c2ca")),
      format = colFormat(digits =2)
    ),
    three_point_field_goals_made = colDef(
      name = "3PAFG",
      maxWidth = 60,
      style = color_scales(pg_stats, colors = c("#fd84a9", "white", "#42c2ca")),
      format = colFormat(digits =2)
    ),
    free_throws_made = colDef(
      name = "AFT",
      maxWidth = 60,
      style = color_scales(pg_stats, colors = c("#fd84a9", "white", "#42c2ca")),
      format = colFormat(digits =2)
    ),
    assists = colDef(
      name = "AST",
      maxWidth = 60,
      style = color_scales(pg_stats, colors = c("#fd84a9", "white", "#42c2ca")),
      format = colFormat(digits =2)
    ),
    rebounds = colDef(
      name = "REB",
      maxWidth = 60,
      style = color_scales(pg_stats, colors = c("#fd84a9", "white", "#42c2ca")),
      format = colFormat(digits =2)
    ),
    total_points = colDef(
      name = "TPTS",
      maxWidth = 100,
      align = "center",
      cell = data_bars(
        pg_stats,
        fill_color = c("#de425b", "#ec9c9d", "#8cbcac", "#488f31"),
        number_fmt = scales::comma_format(),
        round_edges = TRUE,
        border_style = "solid",
        border_color = "gold",
        border_width = ".8px",
        text_position = "above"),
      style = list(borderLeft = "1px dashed rgba(0, 0, 0, 0.3)")
    )
  )
)

### display crosstalk filters
div(bscols(
  widths = c(4, NA, NA),
  list(team_filter,
       conference_filter,
       minutes_filter))
)

### display table
div(nba_table)


## The issue was that you have to reference the og data frame 
## 

# Points assists and rebounds seem to be the most important stats


