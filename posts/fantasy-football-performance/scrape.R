library(ffscrapr)
library(dplyr)
library(purrr)
library(lubridate)
library(tibble)

library(readr)
library(qs)

PROJ_DIR <- 'posts/fantasy-football-performance'
DATA_DIR <- file.path(PROJ_DIR, 'data')
SEASONS <- 2018:2023
ESPN_LEAGUE_ID <- 899513
ESPN_S2 <- Sys.getenv('FF_ESPN_S2')
SWID <- Sys.getenv('FF_SWID')

## data scrape ----
manage_io_operations <- function(conn, f, data_type, overwrite = FALSE, ...) {
  season <- conn$season
  message(sprintf('Scraping %s scores for season = %s.', data_type, conn$season))
  path <- file.path(DATA_DIR, paste0(data_type, '-scores-', conn$season, '.qs'))
  
  if (isFALSE(dir.exists(dirname(path)))) {
    dir.create(dirname(path), recursive = TRUE)
  }
  
  if (file.exists(path) & isFALSE(overwrite)) {
    return(qs::qread(path))
  }
  Sys.sleep(runif(1, 1, 3))
  
  res <- f(conn, ...)
  qs::qsave(res, path)
  res
}

.add_season_col <- function(df, conn) {
  df |> 
    dplyr::mutate(
      season = as.integer(conn$season),
      .before = 1
    )
}

scrape_franchises <- function(conn) {
  ffscrapr::ff_franchises(conn) |> 
    .add_season_col(conn)
}

scrape_schedules <- function(conn) {
  ffscrapr::ff_schedule(conn) |> 
    .add_season_col(conn)
}

scrape_weekly_player_scores <- function(conn) {
  max_week <- 18 # ifelse(conn$season == lubridate::year(Sys.Date()), 14, 18)
  ffscrapr::ff_starters(conn, weeks = 1:max_week) |> 
    .add_season_col(conn)
}

scrape_league <- function(conn) {
  res <- ffscrapr::espn_getendpoint(
    conn = conn,
    view = 'mSettings'
  )
  res[['content']]
}

ff_data <- purrr::map(
  SEASONS,
  \(season) {
    overwrite <- ifelse(season == max(SEASONS), TRUE, FALSE)
    
    conn <- ffscrapr::espn_connect(
      season = season,
      league_id = ESPN_LEAGUE_ID,
      espn_s2 = ESPN_S2,
      swid = SWID
    )
    
    franchises <- manage_io_operations(
      conn, 
      data_type = 'franchises',
      f = scrape_franchises,
      overwrite = overwrite
    )
    
    schedules <- manage_io_operations(
      conn, 
      data_type = 'schedule',
      f = scrape_schedules,
      overwrite = overwrite
    )
    
    player_scores <- manage_io_operations(
      conn, 
      data_type = 'player',
      f = scrape_weekly_player_scores,
      overwrite = overwrite
    )
    
    
    league <- manage_io_operations(
      conn, 
      data_type = 'league',
      f = scrape_league,
      overwrite = TRUE
    )
    
    list(
      'franchises' = franchises,
      'schedules' = schedules,
      'player_scores' = player_scores,
      'league' = league
    )
  }
)

map_dfr_ff_data <- function(ff_data, name) {
  purrr::map_dfr(
    ff_data,
    \(.x) .x[[name]]
  ) 
}

## call functions ----
franchises <- map_dfr_ff_data(ff_data, 'franchises')
schedules <- map_dfr_ff_data(ff_data, 'schedules')
weekly_player_scores <- map_dfr_ff_data(ff_data, 'player_scores')
leagues <- purrr::map(ff_data, \(.x) .x[['league']])

weekly_projected_scores <- weekly_player_scores |> 
  dplyr::filter(lineup_slot != 'BE') |> 
  dplyr::group_by(
    season,
    week,
    franchise_id
  ) |> 
  dplyr::summarize(
    projected_score = sum(projected_score)
  ) |> 
  dplyr::ungroup()

## combine data ----
weekly_team_scores <- schedules |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    user_score = franchise_score,
    opponent_id,
    opponent_score,
    result
  ) |> 
  dplyr::inner_join(
    franchises |> 
      dplyr::select(
        season,
        franchise_id,
        user_name
      ),
    by = dplyr::join_by(season, franchise_id)
  ) |> 
  dplyr::inner_join(
    franchises |> 
      dplyr::select(
        season,
        opponent_id = franchise_id,
        opponent_user_name = user_name
      ),
    by = dplyr::join_by(season, opponent_id)
  ) |> 
  dplyr::left_join(
    weekly_projected_scores |> 
      dplyr::select(
        season,
        week,
        franchise_id,
        user_projected_score = projected_score
      ),
    by = dplyr::join_by(season, week, franchise_id)
  ) |> 
  dplyr::left_join(
    weekly_projected_scores |> 
      dplyr::select(
        season,
        week,
        opponent_id = franchise_id,
        opponent_projected_score = projected_score
      ),
    by = dplyr::join_by(season, week, opponent_id)
  ) |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    user_name,
    opponent_user_name,
    user_score,
    opponent_id,
    opponent_score,
    user_projected_score,
    opponent_projected_score,
    result
  )

## final output ----
readr::write_csv(
  franchises,
  file.path(DATA_DIR, 'franchises-all.csv')
)

qs::qsave(
  weekly_player_scores,
  file.path(DATA_DIR, 'player-scores-all.qs')
)

readr::write_csv(
  weekly_team_scores,
  file.path(DATA_DIR, 'team-scores-all.csv')
)

qs::qsave(
  leagues,
  file.path(DATA_DIR, 'leagues-all.qs')
)


## optimal replacement scores ----
filt_weekly_player_scores <- weekly_player_scores |> 
  dplyr::inner_join(
    franchises |> 
      dplyr::select(
        season,
        franchise_id,
        user_name
      ),
    by = dplyr::join_by(season, franchise_id)
  ) |> 
  dplyr::filter(lineup_slot != 'IR')

starter_scores <- filt_weekly_player_scores |> 
  dplyr::filter(lineup_slot != 'BE') |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    player_id,
    player_score,
    lineup_slot
  ) |> 
  dplyr::arrange(season, week, franchise_id, lineup_slot, player_score)

init_bench_scores <- filt_weekly_player_scores |> 
  dplyr::filter(lineup_slot == 'BE')

POSITION_MAP <- ffscrapr:::.espn_lineupslot_map()
slots <- league$settings$rosterSettings$lineupSlotCounts |> 
  purrr::imap(
    \(.x, .y) {
      setNames(
        as.integer(.x),
        POSITION_MAP[as.character(.y)]
      )
    }
  ) |> 
  purrr::flatten()

bench_potential_lineup_slots <- init_bench_scores |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    player_id,
    eligible_lineup_slots
  ) |> 
  tidyr::unnest_longer(eligible_lineup_slots) |> 
  dplyr::mutate(
    lineup_slot = POSITION_MAP[as.character(eligible_lineup_slots)],
    .keep = 'unused'
  )

bench_scores <- init_bench_scores |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    player_id,
    player_score
  ) |> 
  dplyr::left_join(
    bench_potential_lineup_slots |> 
      dplyr::select(
        season,
        week,
        franchise_id,
        player_id,
        lineup_slot
      ),
    by = dplyr::join_by(season, week, franchise_id, player_id),
    relationship = 'many-to-many'
  ) |> 
  dplyr::semi_join(
    starter_scores |> dplyr::distinct(season, lineup_slot),
    by = dplyr::join_by(season, lineup_slot)
  ) |> 
  dplyr::arrange(
    season, 
    week,
    franchise_id, 
    lineup_slot, 
    dplyr::desc(player_score)
  )

nested_scores <- dplyr::left_join(
  starter_scores |> 
    dplyr::select(
      season,
      week,
      franchise_id,
      lineup_slot,
      player_id,
      player_score
    ) |> 
    tidyr::nest(
      starters = c(player_id, player_score)
    ) |> 
    dplyr::mutate(
      starters = purrr::map(starters, tibble::deframe)
    ),
  bench_scores |> 
    dplyr::select(
      season,
      week,
      franchise_id,
      lineup_slot,
      player_id,
      player_score
    ) |> 
    tidyr::nest(
      bench = c(player_id, player_score)
    ) |> 
    dplyr::mutate(
      bench = purrr::map(bench, tibble::deframe)
    ),
  by = dplyr::join_by(
    season,
    week,
    franchise_id,
    lineup_slot
  )
)

swap_bench_and_starters <- function(starters, bench) {
  if (is.null(bench)) {
    return(starters)
  }
  
  new_starters <- vector('double', length = length(starters))
  nms <- vector('character', length = length(starters))
  for (i in seq_along(starters)) {
    # Find bench values greater than the current starter
    eligible_bench <- bench[bench > starters[i]]
    
    if (length(eligible_bench) > 0) {
      # Find the maximum eligible bench value
      max_bench_value <- max(eligible_bench)
      max_bench_name <- names(eligible_bench[which.max(eligible_bench)])
      
      # Swap the values
      new_starters[i] <- max_bench_value
      nms[i] <- max_bench_name
      
      # Remove the used bench value
      bench[bench == max_bench_value] <- -Inf
    } else {
      new_starters[i] <- starters[i]
      nms[i] <- names(starters[i])
    }
  }
  
  stats::setNames(new_starters, nms)
}

get_nested_name <- function(x) {
  purrr::map(x, \(.x) names(.x))
}
get_nested_value <- function(x) {
  purrr::map(x, \(.x) unname(.x))
}
replacement_scores <- nested_scores |> 
  dplyr::mutate(
    best = purrr::map2(
      starters,
      bench,
      swap_bench_and_starters
    )
  ) |> 
  dplyr::select(
    season,
    week,
    franchise_id,
    lineup_slot,
    starters,
    best
  ) |> 
  dplyr::mutate(
    starter_player_id = get_nested_name(starters),
    starter_player_score = get_nested_value(starters),
    best_player_id = get_nested_name(best),
    best_player_score = get_nested_value(best),
    .keep = 'unused'
  ) |> 
  tidyr::unnest(
    c(
      starter_player_id, 
      starter_player_score, 
      best_player_id, 
      best_player_score
    )
  ) |> 
  dplyr::mutate(
    dplyr::across(c(starter_player_id, best_player_id), as.integer),
    is_replacement = starter_player_id != best_player_id,
    score_improvement = best_player_score - starter_player_score
  )

readr::write_csv(
  replacement_scores,
  file.path(DATA_DIR, 'replacement-scores-all.csv')
)
