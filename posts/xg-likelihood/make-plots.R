library(ggplot2)
library(sysfonts)
library(showtext)
library(ggtext)
library(htmltools)

TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8("&#xf099;")), style = 'font-family:fb'),
  htmltools::tags$span("@TonyElHabr"),
)
CAPTION_LABEL <- '**Data**: Opta via fbref. Updated through 2024-04-25.'
# SUBTITLE_LABEL <- 'Big 5 Leagues, 2017/18 - 2022/23'
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb' # '#f1f1f1'
BLACKISH_BACKGROUND_COLOR <- '#1c1c1c'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fb', 'Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 18, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggtext::element_markdown(face = 'bold', size = 18, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 18, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  # axis.title = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(color = WHITISH_FOREGROUND_COLOR, hjust = 0, size = 10, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)

group_palette <- c(
  'highlight' = '#00bbf9',
  'base' = '#6E7275'
)

## target_o ----
labels <- c(seq(0.1, 2, by = 0.1), '>=2')
labels[seq.int(1, 19, by = 2)] <- ''
raw_o_plot <- all_players_to_evaluate |> 
  # dplyr::filter(prior_shots >= 100) |> 
  dplyr::select(
    player,
    `2023/24 ("target")` = target_o,
    `2018/19-2023/24 ("prior")` = prior_o
  ) |> 
  tidyr::pivot_longer(
    -c(player),
    names_to = 'group',
    values_to = 'o'
  ) |> 
  dplyr::mutate(
    o = ifelse(o > 2, 2.05, o)
  ) |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = o
  ) +
  ggplot2::geom_histogram(
    fill = group_palette[['base']],
    binwidth = 0.1,
    color = BLACKISH_BACKGROUND_COLOR,
    boundary = 0,
    show.legend = FALSE
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(
      xintercept = 1
    ),
    color = WHITISH_FOREGROUND_COLOR,
    linetype = 1,
    linewidth = 1
  ) +
  ggplot2::geom_text(
    data = tibble::tibble(
      x = c(0.55, 1.55),
      y = c(75, 60),
      hjust = c(1, 0),
      group = c('2023/24 ("target")', '2018/19-2023/24 ("prior")'),
      label = c(
        scales::number(maddison_uu_approach1$target_o, accuracy = 0.001),
        scales::number(maddison_uu_approach1$prior_o, accuracy = 0.01)
      )
    ),
    ggplot2::aes(
      x = x,
      y = y,
      hjust = hjust,
      label = paste0('Maddison: ', label)
    ),
    vjust = 0,
    family = FONT,
    color = group_palette[['highlight']],
    size = 12 / .pt
  ) +
  ggplot2::geom_curve(
    data = tibble::tibble(
      x = c(0.57, 1.53),
      xend = c(0.83, 1.35),
      y = c(77, 60),
      yend = c(65, 40),
      group = c('2023/24 ("target")', '2018/19-2023/24 ("prior")')
    ),
    ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    arrow = grid::arrow(length = grid::unit(3, 'pt'), type = 'closed'),
    color = group_palette[['highlight']],
    curvature = 0
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(0, 2, by = 0.5), 2.09),
    labels = c(seq(0, 2, by = 0.5), '>2')
  ) +
  ggplot2::facet_wrap(~group, ncol = 1) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.subtitle = ggtext::element_markdown(size = 12),
    panel.background = ggplot2::element_rect(color = WHITISH_FOREGROUND_COLOR)
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0, 2.1),
    expand = FALSE
  ) +
  ggplot2::labs(
    title = 'Distribution of G / xG ratios',
    subtitle = 'Only including players with >100 shots prior to 2023/24',
    y = 'Count of Players',
    x = 'Outperformance (G / xG)',
    caption = CAPTION_LABEL,
    tag = TAG_LABEL
  )
raw_o_plot

ggplot2::ggsave(
  raw_o_plot,
  filename = file.path(PROJ_DIR, 'raw_o.png'),
  width = 8,
  height = 8 / 1.5
)

## maddison_uu_approach2_plot ----
maddison_resampled_o <- all_resampled_o |>
  dplyr::filter(player == 'James Maddison') |> 
  dplyr::mutate(
    group = ifelse(o <= target_o, 'highlight', 'base')
  )
maddison_uu_approach2 <- all_uu_approach2 |> 
  dplyr::filter(player == 'James Maddison')

maddison_uu_approach2_plot <- maddison_resampled_o |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = o
  ) +
  ggplot2::geom_histogram(
    ggplot2::aes(fill = group),
    binwidth = 0.05,
    boundary = 0,
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = group_palette
  ) +
  ggplot2::geom_vline(
    data = maddison_uu_approach2,
    ggplot2::aes(xintercept = target_o),
    linetype = 2,
    color = 'white'
  ) +
  ggplot2::geom_vline(
    data = maddison_uu_approach2,
    ggplot2::aes(xintercept = prior_o),
    linetype = 2,
    color = 'white'
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = maddison_uu_approach2$target_o - 0.05,
    y = 45,
    hjust = 1,
    label = "Maddison's 2023/24\nG / xG ratio",
    color = WHITISH_FOREGROUND_COLOR,
    fontface = 'bold',
    family = FONT,
    size = 10 / .pt
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = maddison_uu_approach2$prior_o + 0.05,
    y = 45,
    hjust = 0,
    label = "Maddison's pre-2023/24\nG / xG ratio",
    color = WHITISH_FOREGROUND_COLOR,
    fontface = 'bold',
    family = FONT,
    size = 10 / .pt
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = 1),
    color = 'white',
    linetype = 1,
    linewidth = 1.5
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 48), 
    xlim = c(0, 4),
    expand = FALSE,
    clip = 'off'
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = 2.5,
    y = 40,
    hjust = 0,
    vjust = 1,
    label = glue::glue("An outperformance ratio of {scales::number(maddison_uu_approach2$target_o, accuracy = 0.001)}\n(Maddison's 2023/24 G / xG ratio)\nor worse occurs in {scales::percent(maddison_uu_approach2$uu, accuracy = 1)} of {scales::number(R, scale = 1e-3, suffix = 'k')}\nsimulations."),
    color = group_palette[['highlight']],
    fontface = 'bold',
    family = FONT,
    size = 12 / .pt
  ) +
  ggplot2::annotate(
    geom = 'curve',
    x = 2.4,
    y = 35,
    xend = 0.8,
    yend = 29,
    arrow = grid::arrow(length = grid::unit(3, 'pt'), type = 'closed'),
    linewidth = 0.5,
    curvature = 0.25,
    color = group_palette[['highlight']]
  )  +
  ggplot2::annotate(
    ggpath::GeomFromPath,
    x = 4,
    y = 52,
    path = file.path(PROJ_DIR, '493165.png'),
    width = 0.08
  ) +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 12)
  ) +
  ggplot2::labs(
    title = 'Resampled G / xG Ratio for James Maddison',
    subtitle = "Shots Sampled from Maddison's 2018/19 - 2022/2023 Seasons",
    x = 'Outperformance (G / xG)',
    y = 'Count of Simulations',
    caption = CAPTION_LABEL,
    tag = TAG_LABEL
  )

ggplot2::ggsave(
  maddison_uu_approach2_plot,
  filename = file.path(PROJ_DIR, 'maddison_uu_approach2.png'),
  width = 8,
  height = 8 / 1.5
)

## all_uu_approach2_plot ----
all_uu_approach2_plot <- all_uu_approach2 |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = uu
  ) +
  ggplot2::geom_histogram(
    fill = group_palette[['base']],
    binwidth = 0.1,
    color = BLACKISH_BACKGROUND_COLOR,
    boundary = 0,
    show.legend = FALSE
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = 0.25,
    y = 70,
    hjust = 0,
    vjust = 0,
    label = glue::glue("Maddison's 2023/24 G / xG ratio was\nin the {scales::ordinal(maddison_uu_approach2$uu * 100)} percentile of\nunlikely outcomes."),
    color = group_palette[['highlight']],
    fontface = 'bold',
    family = FONT,
    size = 12 / .pt
  ) +
  ggplot2::annotate(
    ggpath::GeomFromPath,
    x = 0.2,
    y = 80,
    path = file.path(PROJ_DIR, '493165.png'),
    width = 0.08
  ) +
  ggplot2::annotate(
    geom = 'curve',
    x = 0.24,
    y = 70,
    xend = 0.16,
    yend = 50,
    arrow = grid::arrow(length = grid::unit(3, 'pt'), type = 'closed'),
    linewidth = 0.5,
    curvature = 0.25,
    color = group_palette[['highlight']]
  ) +
  ggplot2::coord_cartesian(
    # expand = FALSE,
    clip = 'off'
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)
  ) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 12)
  ) +
  ggplot2::labs(
    title = 'Underperformance Unlikeliness % of 2023/24 Player G / xG Ratios',
    subtitle = glue::glue('Calculated via a <b><span style="color:gold">Resampling Approach</span></b>'),
    y = 'Count of Players',
    x = 'Underperformance Unlikeliness %',
    caption = CAPTION_LABEL,
    tag = TAG_LABEL
  )
all_uu_approach2_plot

ggplot2::ggsave(
  all_uu_approach2_plot,
  filename = file.path(PROJ_DIR, 'all_uu_approach2.png'),
  width = 8,
  height = 8 / 1.5
)

## maddison_uu_approach3_plot ----
sample_from_gamma_o <- function(
    shape,
    rate,
    min = 0, 
    max = 2.5, 
    n_samples = N_SIMS,
    seed = 42
) {
  withr::local_seed(42)
  rgamma(
    N_SIMS,
    shape = shape,
    rate = rate
  )
}

maddison_uu_approach3_samples <- maddison_uu_approach3 |>
  dplyr::select(
    player,
    shape,
    rate,
    target_o
  ) |> 
  dplyr::mutate(
    o = purrr::map2(
      shape,
      rate,
      \(.shape, .rate) {
        sample_from_gamma_o(
          shape = .shape,
          rate = .rate
        )
      }
    ),
    .keep = 'unused'
  ) |> 
  tidyr::unnest_longer(o) |> 
  dplyr::mutate(
    group = ifelse(o < target_o, 'highlight', 'base')
  )

maddison_uu_approach3_plot <- maddison_uu_approach3_samples |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = o
  ) +
  ggplot2::geom_histogram(
    ggplot2::aes(fill = group),
    binwidth = 0.05,
    boundary = 0,
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = group_palette
  ) +
  ggplot2::geom_vline(
    data = maddison_uu_approach3,
    ggplot2::aes(xintercept = target_o),
    linetype = 2,
    color = 'white'
  ) +
  ggplot2::geom_vline(
    data = maddison_uu_approach3,
    ggplot2::aes(xintercept = prior_o),
    linetype = 2,
    color = 'white'
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = maddison_uu_approach3$target_o - 0.05,
    y = 650,
    hjust = 1,
    label = "Maddison's 2023/24\nG / xG ratio",
    color = WHITISH_FOREGROUND_COLOR,
    fontface = 'bold',
    family = FONT,
    size = 10 / .pt
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = maddison_uu_approach3$prior_o + 0.05,
    y = 650,
    hjust = 0,
    label = "Maddison's pre-2023/24\nG / xG ratio",
    color = WHITISH_FOREGROUND_COLOR,
    fontface = 'bold',
    family = FONT,
    size = 10 / .pt
  ) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = 1),
    color = 'white',
    linetype = 1,
    linewidth = 1.5
  ) +
  ggplot2::coord_cartesian(
    ylim = c(-50, 700), 
    xlim = c(-0.05, 2.55),
    expand = FALSE,
    clip = 'off'
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = 1.6,
    y = 600,
    hjust = 0,
    vjust = 1,
    label = glue::glue("An outperformance ratio of {scales::number(maddison_uu_approach3$prior_o, accuracy = 0.001)}\n(Maddison's 2023/24 G / xG ratio)\nor worse occurs in {scales::percent(maddison_uu_approach3$uu, accuracy = 1)} of {scales::number(N_SIMS, scale = 1e-3, suffix = 'k')}\ndraws."),
    color = group_palette[['highlight']],
    fontface = 'bold',
    family = FONT,
    size = 11 / .pt
  ) +
  ggplot2::annotate(
    geom = 'curve',
    x = 1.55,
    y = 550,
    xend = 0.83,
    yend = 240,
    arrow = grid::arrow(length = grid::unit(3, 'pt'), type = 'closed'),
    linewidth = 0.5,
    curvature = 0.25,
    color = group_palette[['highlight']]
  )  +
  ggplot2::annotate(
    ggpath::GeomFromPath,
    x = 2.5,
    y = 730,
    path = file.path(PROJ_DIR, '493165.png'),
    width = 0.08
  ) +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 12)
  ) +
  ggplot2::labs(
    title = 'Estimated Distribution of G / xG Ratio for James Maddison',
    subtitle = "Fit Based on Maddison's 2018/19 - 2022/2023 Seasons",
    x = 'Outperformance (G / xG)',
    y = 'Count of Samples',
    caption = CAPTION_LABEL,
    tag = TAG_LABEL
  )

ggplot2::ggsave(
  maddison_uu_approach3_plot,
  filename = file.path(PROJ_DIR, 'maddison_uu_approach3.png'),
  width = 8,
  height = 8 / 1.5
)

## all_uu_approach3_plot ----
all_uu_approach3_plot <- all_uu_approach3 |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = uu
  ) +
  ggplot2::geom_histogram(
    fill = group_palette[['base']],
    binwidth = 0.1,
    color = BLACKISH_BACKGROUND_COLOR,
    boundary = 0,
    show.legend = FALSE
  ) +
  ggplot2::annotate(
    geom = 'text',
    x = 0.2,
    y = 70,
    hjust = 0,
    vjust = 0,
    label = glue::glue("Maddison's 2023/24 G / xG ratio was\nin the {scales::ordinal(maddison_uu_approach3$uu * 100)} percentile of\nunlikely outcomes."),
    color = group_palette[['highlight']],
    fontface = 'bold',
    family = FONT,
    size = 12 / .pt
  ) +
  ggplot2::annotate(
    geom = 'curve',
    x = 0.18,
    y = 70,
    xend = 0.08,
    yend = 55,
    arrow = grid::arrow(length = grid::unit(3, 'pt'), type = 'closed'),
    linewidth = 0.5,
    curvature = 0.25,
    color = group_palette[['highlight']]
  ) +
  ggplot2::annotate(
    ggpath::GeomFromPath,
    x = 0.15,
    y = 80,
    path = file.path(PROJ_DIR, '493165.png'),
    width = 0.08
  ) +
  ggplot2::coord_cartesian(
    # expand = FALSE,
    clip = 'off'
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.1)
  ) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title = ggtext::element_markdown(size = 16),
    plot.subtitle = ggtext::element_markdown(size = 12)
  ) +
  ggplot2::labs(
    title = 'Underperformance Unlikeliness % of 2023/24 Player G / xG Ratios',
    subtitle = 'Calculated via a <b><span style="color:gold">Cumulative Distribution Function</span></b>',
    y = 'Count of Players',
    x = 'Underperformance Unlikeliness %',
    caption = CAPTION_LABEL,
    tag = TAG_LABEL
  )
all_uu_approach3_plot

ggplot2::ggsave(
  all_uu_approach3_plot,
  filename = file.path(PROJ_DIR, 'all_uu_approach3.png'),
  width = 8,
  height = 8 / 1.5
)

## scatters ----
all_uus <- dplyr::left_join(
  all_uu_approach3 |> dplyr::select(player, uu_approach3 = uu),
  dplyr::bind_rows(
    all_uu_approach1 |> dplyr::transmute(player, approach = 'Approach 1 (Percentile Ranking)', uu),
    all_uu_approach2 |> dplyr::transmute(player, approach = 'Approach 2 (Resampling)', uu)
  ),
  by = dplyr::join_by(player)
) |> 
  dplyr::left_join(
    all_players_to_evaluate,
    by = dplyr::join_by(player)
  )

all_uus_plot <- all_uus |> 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = uu,
    y = uu_approach3
  ) +
  ggplot2::geom_abline(
    ggplot2::aes(
      slope = 1,
      intercept = 0
    ),
    linewidth = 2,
    linetype = 1,
    color = WHITISH_FOREGROUND_COLOR
  ) +
  ggplot2::geom_point(
    data = all_uus |> dplyr::filter(player != 'James Maddison'),
    ggplot2::aes(
      size = prior_shots
    ),
    shape = 21,
    color = 'white',
    stroke = 0.25,
    fill = group_palette[['base']]
  ) +
  ggplot2::geom_point(
    data = all_uus |> dplyr::filter(player == 'James Maddison'),
    size = 4,
    shape = 21,
    color = 'white',
    stroke = 0.25,
    fill = group_palette[['highlight']]
  ) +
  ggtext::geom_richtext(
    data = tibble::tibble(approach = 'Approach 1 (Percentile Ranking)'),
    ggplot2::aes(
      x = 0.04,
      y = 0.8,
      label = glue::glue(
        "
        **Maddison's unlikeliness**<br/>
        Approach 1: {scales::ordinal(maddison_uu_approach1$uu * 100)} percentile<br/>
        Approach 3: {scales::ordinal(maddison_uu_approach3$uu * 100)} percentile
         "
      )
    ),
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), 'pt'), # remove padding
    hjust = 0,
    vjust = 0,
    color = group_palette[['highlight']],
    # fontface = 'bold',
    fontface = 'plain',
    family = FONT,
    size = 11 / .pt
  ) +
  ggtext::geom_richtext(
    data = tibble::tibble(approach = 'Approach 2 (Resampling)'),
    ggplot2::aes(
      x = 0.04,
      y = 0.8,
      label = glue::glue(
        "
        **Maddison's unlikeliness**<br/>
        Approach 2: {scales::ordinal(maddison_uu_approach2$uu * 100)} percentile<br/>
        Approach 3: {scales::ordinal(maddison_uu_approach3$uu * 100)} percentile
         "
      )
    ),
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), 'pt'), # remove padding
    hjust = 0,
    vjust = 0,
    color = group_palette[['highlight']],
    # fontface = 'bold',
    fontface = 'plain',
    family = FONT,
    size = 11 / .pt
  ) +
  ggplot2::coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    expand = FALSE
  ) +
  ggplot2::scale_size_continuous(
    labels = scales::comma,
    range = c(0.5, 3)
  ) +
  ggplot2::guides(
    size = ggplot2::guide_legend(
      'Number of total shots', 
      title.theme = ggplot2::element_text(size = 12, color = 'white', family = FONT),
      label.theme = ggplot2::element_text(size = 12, color = 'white', family = FONT)
    )
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::percent
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent
  ) +
  ggplot2::facet_wrap(~approach, scales = 'fixed') +
  ggplot2::theme(
    legend.position = 'top',
    plot.subtitle = ggtext::element_markdown(size = 12),
    panel.grid.major = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(color = WHITISH_FOREGROUND_COLOR)
  ) +
  ggplot2::labs(
    title = 'Approaches for Estimating Underperforming Unlikeliness',
    subtitle = 'Each point represents one player',
    y = 'Approach 3 (CDF)',
    x = NULL,
    caption = CAPTION_LABEL,
    tag = TAG_LABEL
  )
all_uus_plot

ggplot2::ggsave(
  all_uus_plot,
  filename = file.path(PROJ_DIR, 'all_uus.png'),
  width = 8,
  height = 8 / 1.5
)

