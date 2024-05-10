empirical_actions <- dplyr::inner_join(
  xy |> 
    dplyr::filter() |> 
    dplyr::filter(
      period_id == 2L,
      goalscore_diff == -1L,
      (result_success_a0 == 1L | result_fail_a0 == 1L)
    ) |> 
    dplyr::inner_join(
      vaep |>
        dplyr::select(
          competition_id,
          season_id,
          game_id,
          period_id,
          team_id,
          action_id,
          pred_scores,
          pred_concedes,
          pred,
          ovaep,
          dvaep,
          vaep
        ),
      by = dplyr::join_by(
        competition_id,
        season_id,
        game_id,
        period_id,
        team_id,
        action_id
      )
    ),
  crossed_freq_grid,
  by = dplyr::join_by(
    start_x_a0 >= start_x, 
    start_x_a0 < next_start_x, 
    start_y_a0 >= start_y,
    start_y_a0 < next_start_y,
    end_x_a0 >= end_x, 
    end_x_a0 < next_end_x, 
    end_y_a0 >= end_y,
    end_y_a0 < next_end_y
  )
)

agg_empirical_actions <- empirical_actions |> 
  dplyr::group_by(
    idx
  ) |> 
  dplyr::summarize(
    n = dplyr::n(),
    dplyr::across(
      c(
        pred_scores,
        pred_concedes,
        pred,
        ovaep,
        dvaep,
        vaep
      ),
      list(actual = \(.x) mean(.x, na.rm = TRUE))
    )
  ) |> 
  dplyr::ungroup() |> 
  dplyr::right_join(
    crossed_freq_grid,
    by = dplyr::join_by(idx),
    relationship = 'many-to-many'
  ) |> 
  dplyr::mutate(
    idx1 = sprintf('%02d', i1 - 1L),
    idy1 = sprintf('%02d', j1 - 1L),
    idx2 = sprintf('%02d', i2 - 1L),
    idy2 = sprintf('%02d', j2 - 1L)
  )

agg_empirical_action_props <- agg_empirical_actions |> 
  dplyr::group_by(
    idx1,
    idy1
  ) |> 
  dplyr::transmute(
    idx2,
    idy2,
    n,
    prop = n / sum(n, na.rm = TRUE),
    pred_actual,
    vaep_actual
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(prop))


plot_pass_grid <- function(start_x = 4, start_y = 5, end_x = 8, end_y = 6) {
  df <- agg_empirical_action_props |> 
    dplyr::transmute(
      n,
      prop,
      x1 = as.integer(idx1) + 1L,
      x2 = as.integer(idx2) + 1L,
      y1 = as.integer(idy1) + 1L,
      y2 = as.integer(idy2) + 1L,
      x = as.integer(idx2) * X_DELTA,
      y = as.integer(idy2)* Y_DELTA,
      width = X_DELTA,
      height = Y_DELTA,
      `VAEP` = f(vaep_actual),
      `Relative Frequency` = prop
    ) |> 
    dplyr::filter(
      x1 == start_x,
      y1 == start_y
    ) |> 
    tidyr::pivot_longer(
      c(`Relative Frequency`, `VAEP`)
    ) |> 
    dplyr::arrange(desc(value)) 
  
  p1 <- df |> 
    dplyr::filter(name == 'Relative Frequency') |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = x2,
      y = y2
    ) +
    ggplot2::geom_tile(
      data = df |> 
        dplyr::filter(name == 'Relative Frequency') |> 
        dplyr::filter(value < max(value, na.rm = TRUE)),
      ggplot2::aes(
        fill = value
      ),
      alpha = 0.8,
      show.legend = FALSE
    ) +
    ggplot2::geom_tile(
      inherit.aes = FALSE,
      ggplot2::aes(
        x = start_x,
        y = start_y,
      ),
      linewidth = 1,
      color = 'red',
      fill = 'white'
    ) +
    ggplot2::geom_text(
      data = df |> dplyr::filter(name == 'Relative Frequency'),
      ggplot2::aes(
        label = scales::percent(value, accuracy = 0.1)
      ),
      size = 9 / .pt
    ) +
    ggplot2::geom_segment(
      arrow = grid::arrow(length = grid::unit(6, 'pt'), type = 'closed'),
      color = 'red',
      linewidth = 2,
      ggplot2::aes(
        x = start_x + 0.25,
        y = start_y + 0.25,
        xend = end_x - 0.25,
        yend = end_y - 0.25
      )
    ) +
    ggplot2::scale_fill_viridis_c(option = 'B', direction = 1, end = 0.8, begin = 0.1) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::labs(
      title = 'Period: 2, Gamestate: -1, Actual start point: (31m,38m), Hypothetical end point: (65m,47m)',
      subtitle = 'Relative frequency of end point of actions',
      x = NULL,
      y = NULL
    )
  p1
  
  p2 <- df |> 
    dplyr::filter(name == 'VAEP') |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = x2,
      y = y2
    ) +
    ggplot2::geom_tile(
      data = df |> 
        dplyr::filter(name == 'VAEP'),
      ggplot2::aes(
        fill = value
      ),
      alpha = 0.8,
      show.legend = FALSE
    ) +
    ggplot2::geom_tile(
      inherit.aes = FALSE,
      ggplot2::aes(
        x = start_x,
        y = start_y,
      ),
      linewidth = 1,
      color = 'red',
      fill = 'white'
    ) +
    ggplot2::geom_text(
      data = df |> dplyr::filter(name == 'VAEP'),
      ggplot2::aes(
        label = scales::number(value, accuracy = 0.001)
      ),
      size = 9 / .pt
    ) +
    ggplot2::geom_segment(
      arrow = grid::arrow(length = grid::unit(6, 'pt'), type = 'closed'),
      color = 'red',
      linewidth = 2,
      ggplot2::aes(
        x = start_x + 0.25,
        y = start_y + 0.25,
        xend = end_x - 0.25,
        yend = end_y - 0.25
      )
    ) +
    ggplot2::scale_fill_viridis_c(option = 'H', direction = 1) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::labs(
      subtitle = 'Avg. VAEP of actions',
      caption = 'Direction of play: left to right',
      x = NULL,
      y = NULL
    )
  
  filt_start_actions <- empirical_actions |> 
    ## use .env$ since the same names are in the df, but represent meters
    dplyr::filter(
      i1 == .env$start_x,
      j1 == .env$start_y
    ) |> 
    mutate(
      capped_vaep = case_when(
        vaep > 0.02 ~ 0.02,
        vaep < -0.02 ~ -0.02,
        TRUE ~ vaep
      ),
      group = case_when(
        .env$end_x == i2 & .env$end_y == j2 ~ 'Match Pass End Location',
        TRUE ~ 'All Other Actions'
      )
    )
  
  medians <- filt_start_actions |> group_by(group) |> summarize(vaep = median(vaep, na.rm = TRUE))
  n_lt <- filt_start_actions |> 
    filter(
      vaep < (medians |> filter(group == 'Match Pass End Location') |> pull(vaep))
    ) |> 
    nrow()
  
  prop <- n_lt / nrow(filt_start_actions)
  
  p3 <- filt_start_actions |> 
    ggplot2::ggplot() +
    aes(x = capped_vaep) +
    geom_histogram(
      aes(fill = group), 
      binwidth = 0.001
    ) +
    scale_fill_manual(
      values = c(`All Other Actions` = 'gray50', `Match Pass End Location` = 'red')
    ) +
    geom_vline(
      data = medians |> filter(group == 'All Other Actions'),
      aes(xintercept = vaep),
      color = 'black',
      linewidth = 2
    ) +
    geom_vline(
      data = medians |> filter(group == 'Match Pass End Location'),
      aes(xintercept = vaep),
      color = 'red',
      linewidth = 2
    ) +
    geom_text(
      data = medians |> filter(group == 'All Other Actions'),
      aes(label = paste0('Median: ', scales::number(vaep, accuracy = 0.001)), x = 0.002, y = 1200),
      color = 'black',
      vjust = 1,
      hjust = 0,
      fontface = 'bold',
      size = 14 / .pt
    ) +
    geom_text(
      data = medians |> filter(group == 'Match Pass End Location'),
      aes(label = paste0('Median: ', scales::number(vaep, accuracy = 0.001),'\n>', scales::percent(prop, accuracy = 1), ' of other actions\nhave lower value'), x = 0.009, y = 1200),
      color = 'red',
      vjust = 1,
      hjust = 0,
      fontface = 'bold',
      size = 14 / .pt
    ) +
    scale_y_continuous(labels = comma) +
    guides(fill = guide_legend('')) +
    theme_minimal(base_size = 14) +
    theme(
      plot.caption = element_text(size = 14),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      legend.position = c(0.2, 0.9)
    ) +
    labs(table = 'Histogram of action values', caption = 'Capped at +-0.02', x = NULL, y = 'Count of actions')
  p3
  p <- (p1 + p2) / p3
  p
  ggsave(
    file.path(PROJ_DIR, 'z_score.png'),
    width = 12,
    height = 8
  )
}

library(scales)
plot_pass_grid(
  x = 4,
  y = 5,
  # f = \(.x) { pmax(pmin(x, 0.25), -0.25) },
  f = \(.x) { .x },
  low = muted("red"),
  mid = 'white',
  high = muted("blue"),
)


empirical_actions |> 
  filter(idx == 24) |> 
  ggplot() +
  aes(
    x = vaep
  ) +
  geom_histogram()
