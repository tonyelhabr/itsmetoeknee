library(engsoccerdata)
library(tibble)
library(lubridate)
library(dplyr)
library(qs)

PROJ_DIR <- 'posts/elo-margin-of-victory'
team_mapping <- readr::read_csv(file.path(PROJ_DIR, 'team-mapping.csv'))
team_mapping_vec <- setNames(team_mapping$team_clubelo, team_mapping$team_engsoccerdata)

CLUBELO_DATA_DIR <- file.path(PROJ_DIR, 'data')
dir.create(CLUBELO_DATA_DIR, showWarnings = FALSE)

get_clubelo_ratings <- function(team) {
  team <- gsub(' ', '', team)
  path <- file.path(CLUBELO_DATA_DIR, paste0(team, '.qs'))
  if (file.exists(path)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1, 1, 3))
  message(sprintf('Scraping clubelo for %s.', team))
  url <- sprintf('http://api.clubelo.com/%s', team)
  resp <- httr::GET(url)
  httr::stop_for_status(resp)
  res <- httr::content(resp)
  qs::qsave(res, path)
  invisible(res)
}

game_scores <- engsoccerdata::england |> 
  dplyr::filter(
    tier == 1,
    dplyr::between(Season, 2012, 2022)
  ) |> 
  dplyr::transmute(
    season = Season,
    date = lubridate::date(Date),
    game_id = dplyr::row_number(),
    home_team = team_mapping_vec[home],
    away_team = team_mapping_vec[visitor],
    home_g = hgoal,
    away_g = vgoal,
    gd = goaldif,
    result
  ) |> 
  tibble::as_tibble()

game_scores <- dplyr::rows_update(
  game_scores,
  ## match moved back one day due to bomb threat
  ##   https://www.nytimes.com/2016/05/16/world/europe/manchester-united-bournemouth-canceled-package-premier-league.html
  tibble::tibble(
    game_id = 1520,
    date = lubridate::ymd('2016-05-17')
  ),
  by = 'game_id'
)

raw_clubelo_ratings <- purrr::map_dfr(
  team_mapping$team_clubelo,
  get_clubelo_ratings
) |> 
  dplyr::filter(
    To >= min(game_scores$date),
    From <= max(game_scores$date + lubridate::days(1))
  ) |> 
  dplyr::transmute(
    team = Club,
    elo = Elo,
    from_date = From,
    to_date = To
  )

long_game_scores <- dplyr::inner_join(
  game_scores |> 
    dplyr::select(
      season,
      date,
      game_id,
      result,
      home = home_team,
      away = away_team
    ) |> 
    tidyr::pivot_longer(
      c(home, away),
      names_to = 'side',
      values_to = 'team'
    ),
  game_scores |> 
    dplyr::select(
      game_id,
      gd,
      home = home_g,
      away = away_g
    ) |> 
    tidyr::pivot_longer(
      c(home, away),
      names_to = 'side',
      values_to = 'g'
    ),
  by = dplyr::join_by(game_id, side)
) |> 
  dplyr::inner_join(
    raw_clubelo_ratings |> dplyr::select(team, pre_elo = elo, date = to_date),
    by = dplyr::join_by(team, date)
  ) |> 
  dplyr::inner_join(
    raw_clubelo_ratings |> dplyr::transmute(team, post_elo = elo, date = from_date - lubridate::days(1)),
    by = dplyr::join_by(team, date)
  )
game_scores |> 
  # mutate(
  #   across(
  #     gd,
  #     \(.x) case_when(
  #       gd <= -3 ~ -3,
  #       gd >= 3 ~ 3,
  #       TRUE ~ gd
  #     )
  #   )
  # ) |> 
  count(gd = abs(gd), sort = TRUE) |> 
  arrange(gd) |> 
  mutate(prop = n / sum(n), .keep = 'unused')

augmented_game_scores <- game_scores |> 
  dplyr::inner_join(
    raw_clubelo_ratings |> 
      dplyr::select(
        home_team = team, 
        home_pre_elo = elo,
        date = to_date
      ),
    by = dplyr::join_by(home_team, date)
  ) |> 
  dplyr::inner_join(
    raw_clubelo_ratings |> 
      dplyr::transmute(
        home_team = team, 
        home_post_elo = elo, 
        date = from_date - lubridate::days(1)
      ),
    by = dplyr::join_by(home_team, date)
  ) |> 
  dplyr::inner_join(
    raw_clubelo_ratings |> 
      dplyr::select(
        away_team = team,
        away_pre_elo = elo, 
        date = to_date
      ),
    by = dplyr::join_by(away_team, date)
  ) |> 
  dplyr::inner_join(
    raw_clubelo_ratings |> 
      dplyr::transmute(
        away_team = team, 
        away_post_elo = elo, 
        date = from_date - lubridate::days(1)
      ),
    by = dplyr::join_by(away_team, date)
  )

long_game_scores <- dplyr::bind_rows(
  augmented_game_scores |> 
    dplyr::transmute(
      season,
      date,
      game_id,
      side = 'home',
      team = home_team,
      opponent_team = away_team,
      g = home_g,
      opponent_g = away_g,
      gd = home_g - away_g,
      result = dplyr::case_when(
        result == 'D' ~ 0.5,
        result == 'H' ~ 1,
        result == 'A' ~ 0
      ),
      pre_elo = home_pre_elo,
      post_elo = home_post_elo,
      opponent_pre_elo = away_pre_elo,
      opponent_post_elo = away_post_elo
    ),
  augmented_game_scores |> 
    dplyr::transmute(
      season,
      date,
      game_id,
      side = 'away',
      team = away_team,
      opponent_team = home_team,
      g = away_g,
      opponent_g = home_g,
      gd = away_g - home_g,
      result = dplyr::case_when(
        result == 'D' ~ 0.5,
        result == 'A' ~ 1,
        result == 'H' ~ 0
      ),
      pre_elo = away_pre_elo,
      post_elo = away_post_elo,
      opponent_pre_elo = home_pre_elo,
      opponent_post_elo = home_post_elo
    )
) |> 
  dplyr::arrange(
    game_id,
    side
  )

pr_elo <- function(r_i, r_j, xi = 400) {
  1 / (1 + 10 ^ ((r_j - r_i) / xi))
}

standard_elo <- function(r_i, r_j, w_ij = 1, k = 20, ...) {
  p_ij_hat <- pr_elo(r_i = r_i, r_j = r_j, ...)
  r_i + k * (w_ij - p_ij_hat)
}

calculated_elos <- long_game_scores |> 
  dplyr::transmute(
    season,
    date,
    game_id,
    side,
    team,
    opponent_team,
    pre_elo,
    opponent_pre_elo,
    g,
    opponent_g,
    gd,
    result,
    post_elo,
    standard_post_elo = purrr::pmap_dbl(
      list(
        pre_elo,
        opponent_pre_elo,
        result
      ),
      \(r_i, r_j, w_ij) {
        standard_elo(
          r_i = r_i,
          r_j = r_j,
          w_ij = w_ij
        )
      }
    ),
    elo_d = post_elo - standard_post_elo
  )

calculated_elos |> 
  filter(
    team == 'Arsenal'
  ) |> 
  tail()
