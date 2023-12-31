.gt_theme_538_dark <- function(gt_object, ..., dark_color = '#222222') {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = 'gt_tbl' %in% class(gt_object))
  
  table_id <- subset(gt_object[['_options']], parameter == 'table_id')[['value']][[1]]
  
  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which('table_id' %in% gt_object[['_options']][['parameter']])[[1]]
    gt_object[['_options']][['value']][[opt_position]] <- table_id
  }
  
  gt_object |>
    opt_table_font(
      font = list(
        google_font('Titillium Web'),
        default_fonts()
      ),
      weight = 400
    ) |>
    tab_style(
      style = cell_text(
        color = 'white',
        font = google_font('Titillium Web'),
        transform = 'uppercase'
      ),
      locations = cells_column_labels(everything())
    ) |>
    tab_style(
      locations = cells_title('title'),
      style = cell_text(
        font = google_font('Titillium Web'),
        weight = 700
      )
    ) |>
    tab_style(
      locations = cells_title('subtitle'),
      style = cell_text(
        font = google_font('Titillium Web'),
        weight = 400
      )
    ) |>
    tab_style(
      style = list(
        cell_borders(
          sides = 'top', color = '#222222', weight = px(0)
        ),
        cell_text(
          font = google_font('Titillium Web'),
          transform = 'uppercase',
          v_align = 'bottom',
          size = px(14),
          weight = 200
        )
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    ) |>
    tab_style(
      style = cell_borders(
        sides = 'bottom', color = '#222222', weight = px(1)
      ),
      locations = cells_row_groups()
    ) |>
    tab_options(
      column_labels.background.color = 'white',
      data_row.padding = px(3),
      heading.border.bottom.style = 'none',
      table.border.top.width = px(3),
      table.border.top.style = 'none', # transparent
      table.border.bottom.style = 'none',
      column_labels.font.weight = 'normal',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = dark_color,
      row_group.border.top.style = 'none',
      row_group.border.top.color = dark_color,
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = 'white',
      stub.border.color = 'white',
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = 'none',
      table.font.size = 16,
      heading.align = 'left',
      ...
    ) |>
    opt_css(
      paste0('#', table_id, ' tbody tr:last-child {border-bottom: 2px solid #ffffff00;}'),
      add = TRUE
    ) |>
    tab_options(
      heading.align = 'left',
      heading.border.bottom.style = 'none',
      table.background.color = dark_color,
      table.font.color.light = 'white',
      table.border.top.style = 'none',
      table.border.bottom.color = dark_color,
      table.border.left.color = dark_color,
      table.border.right.color = dark_color,
      table_body.border.top.style = 'none',
      table_body.border.bottom.color = dark_color,
      column_labels.border.top.style = 'none',
      column_labels.background.color = dark_color,
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = 'white',
      data_row.padding = px(7),
      ...
    )
}

TAG_LABEL <- htmltools::tagList(
  htmltools::tags$span(htmltools::HTML(enc2utf8('&#xf099;')), style = 'font-family:fb'),
  htmltools::tags$span('@TonyElHabr'),
)
PLOT_RESOLUTION <- 300
WHITISH_FOREGROUND_COLOR <- 'white'
COMPLEMENTARY_FOREGROUND_COLOR <- '#cbcbcb'
BLACKISH_BACKGROUND_COLOR <- '#222222'
COMPLEMENTARY_BACKGROUND_COLOR <- '#4d4d4d'
FONT <- 'Titillium Web'
sysfonts::font_add_google(FONT, FONT)
## https://github.com/tashapiro/tanya-data-viz/blob/main/chatgpt-lensa/chatgpt-lensa.R for twitter logo
sysfonts::font_add('fa-brands', 'Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi = PLOT_RESOLUTION)

ggplot2::theme_set(ggplot2::theme_minimal())
ggplot2::theme_update(
  text = ggplot2::element_text(family = FONT),
  title = ggplot2::element_text(size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title = ggtext::element_markdown(face = 'bold', size = 20, color = WHITISH_FOREGROUND_COLOR),
  plot.title.position = 'plot',
  plot.subtitle = ggtext::element_markdown(size = 16, color = COMPLEMENTARY_FOREGROUND_COLOR),
  axis.text = ggplot2::element_text(color = WHITISH_FOREGROUND_COLOR, size = 14),
  axis.title.x = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.title.y = ggtext::element_markdown(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0.99),
  axis.line = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 14, color = WHITISH_FOREGROUND_COLOR, face = 'bold', hjust = 0),
  legend.position = 'top',
  legend.text = ggplot2::element_text(size = 12, color = WHITISH_FOREGROUND_COLOR, face = 'plain'),
  legend.title = ggplot2::element_text(size = 12, color = WHITISH_FOREGROUND_COLOR, face = 'bold'),
  panel.grid.major = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor = ggplot2::element_line(color = COMPLEMENTARY_BACKGROUND_COLOR),
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  plot.margin = ggplot2::margin(10, 20, 10, 20),
  plot.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR),
  plot.caption = ggtext::element_markdown(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 0, face = 'plain'),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown(size = 10, color = WHITISH_FOREGROUND_COLOR, hjust = 1),
  plot.tag.position = c(0.99, 0.01),
  panel.spacing.x = grid::unit(2, 'lines'),
  panel.background = ggplot2::element_rect(fill = BLACKISH_BACKGROUND_COLOR, color = BLACKISH_BACKGROUND_COLOR)
)
ggplot2::update_geom_defaults('text', list(color = WHITISH_FOREGROUND_COLOR, size = 12 / .pt))

