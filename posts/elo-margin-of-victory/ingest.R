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
    away_g = vgoal
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
      opponent = away_team,
      team__g = home_g,
      opponent__g = away_g,
      team__pre_elo = home_pre_elo,
      team__post_elo = home_post_elo,
      opponent__pre_elo = away_pre_elo,
      opponent__post_elo = away_post_elo
    ),
  augmented_game_scores |> 
    dplyr::transmute(
      season,
      date,
      game_id,
      side = 'away',
      team = away_team,
      opponent = home_team,
      team__g = away_g,
      opponent__g = home_g,
      team__pre_elo = away_pre_elo,
      team__post_elo = away_post_elo,
      opponent__pre_elo = home_pre_elo,
      opponent__post_elo = home_post_elo
    )
) |> 
  dplyr::arrange(
    game_id,
    side
  )

pr_elo <- function(r1, r2) {
  1 / (1 + 10 ^ ((r2 - r1) / 400))
}

convert_scores_to_wld_numeric <- function(score1, score2) {
  dplyr::case_when(
    score1 < score2 ~ 0,
    score1 > score2 ~ 1,
    score1 == score2 ~ 0.5
  ) 
}

standard_elo <- function(r1, r2, score1, score2, k = 20, ...) {
  p12 <- pr_elo(r1 = r1, r2 = r2, ...)
  w12 <- convert_scores_to_wld_numeric(score1, score2)
  r1 + k * (w12 - p12)
}

init_elo <- long_game_scores |> 
  dplyr::group_by(team) |> 
  dplyr::slice_min(date, n = 1, with_ties = FALSE) |> 
  dplyr::ungroup() |> 
  dplyr::transmute(
    team,
    pre_elo = team__pre_elo,
    post_elo = pre_elo
  )

do_calculate_elos <- function(games, init_elo, elo_f) {
  purrr::map_dfr(
    games$game_id,
    \(game_id) {
      
      if ((game_id %% 100) == 0) {
        cli::cli_inform('Computing elo for {game_id} of {nrow(games)} game{?s}.')
      }
      
      game <- dplyr::filter(games, .data$game_id == .env$game_id)
      
      team_pre_elo <- init_elo |> 
        dplyr::filter(team == game$team) |> 
        dplyr::pull(pre_elo)
      opponent_pre_elo <- init_elo |> 
        dplyr::filter(team == game$opponent) |> 
        dplyr::pull(pre_elo)
      
      team_post_elo <- standard_elo(
        r1 = team_pre_elo,
        r2 = opponent_pre_elo,
        score1 = game$team__g,
        score2 = game$opponent__g
      )
      opponent_post_elo <- standard_elo(
        r2 = team_pre_elo,
        r1 = opponent_pre_elo,
        score2 = game$team__g,
        score1 = game$opponent__g
      )
      
      init_elo <- init_elo |> 
        dplyr::mutate(
          pre_elo = ifelse(
            team %in% c(game$team, game$opponent),
            .data$post_elo,
            .data$pre_elo
          ),
          post_elo = dplyr::case_when(
            team == game$team ~ team_post_elo,
            team == game$opponent ~ opponent_post_elo,
            TRUE ~ .data$post_elo
          )
        )
      
      init_elo |> 
        dplyr::filter(
          team %in% c(game$team, game$opponent),
        ) |> 
        dplyr::transmute(
          game_id = .env$game_id,
          team,
          pre_elo,
          post_elo
        )
    }
  )
}

long_home_game_scores <- long_game_scores |> dplyr::filter(side == 'home')
standard_computed_elos <- do_calculate_elos(
  games = long_home_game_scores,
  init_elo = init_elo,
  elo_f = standard_elo
)

augment_with_computed_elos <- function(games, computed_elos, suffix = '') {
  games |> 
    dplyr::left_join(
      computed_elos |> 
        dplyr::select(
          game_id, 
          team, 
          'team__pre_elo__{suffix}' := pre_elo,
          'team__post_elo__{suffix}' := post_elo
        ),
      by = dplyr::join_by(game_id, team)
    ) |> 
    dplyr::left_join(
      computed_elos |> 
        dplyr::select(
          game_id, 
          opponent = team, 
          'opponent__pre_elo__{suffix}' := pre_elo,
          'opponent__post_elo__{suffix}' := post_elo
        ),
      by = dplyr::join_by(game_id, opponent)
    )
}

long_game_scores |> 
  dplyr::select(
    season,
    date,
    game_id,
    side,
    team,
    opponent,
    team__g,
    opponent__g,
    team__post_elo__clubelo = team__post_elo,
    opponent__post__elo__clubelo = opponent__post_elo
  ) |> 
  augment_with_computed_elos(
    standard_computed_elos,
    suffix = 'standard'
  ) |> 
  dplyr::select(-matches('__pre'))


mov_elo_538 <- function(r1, r2, score1, score2, k = 20, numerator_multiplier = 1, numerator_buffer = 1, tie_margin = 1, ...) {
  p12 <- pr_elo(r1 = r1, r2 = r2, ...)
  w12 <- convert_scores_to_wld_numeric(score1, score2)
  score_d <- ifelse(
    score1 == score2,
    tie_margin,
    abs(score1 - score2)
  )
  mov_multiplier <- log(numerator_multiplier * score_d + numerator_buffer) * (2.2 / (abs(p12) * 0.001 + 2.2))
  r1 + mov_multiplier * k * (w12 - p12)
}


football_elo_mov_multiplier <- tibble::tibble(
  mov = c(0:28)
)
football_elo_mov_multiplier$elo <- purrr::map_dbl(
  football_elo_mov_multiplier$mov,
  \(mov) {
    mov_elo_538(r1 = 1500, r2 = 1600, score1 = mov, score2 = 0, numerator_buffer = 1)
  }
)

soccer_elo_mov_multiplier <- tibble::tibble(
  mov = c(0:4)
)
soccer_elo_mov_multiplier$elo <- purrr::map_dbl(
  soccer_elo_mov_multiplier$mov,
  \(mov) {
    mov_elo_538(r1 = 1500, r2 = 1600, score1 = mov, score2 = 0, numerator_multiplier = 3.5, numerator_buffer = 7)
  }
)

library(ggplot2)
football_elo_mov_multiplier |> 
  ggplot() +
  aes(
    x = mov,
    y = elo
  ) +
  geom_point() +
  geom_point(
    data = soccer_elo_mov_multiplier,
    color = 'red',
    aes(
      y = elo + 1,
      x = 3.5 * mov
    )
  )
