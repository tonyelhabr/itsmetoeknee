
library(tidyverse)

dir_root <- 'c:/users/aelhabr/documents/projects/'
# posts ---
dirs_post <-
  fs::path(dir_root, 'netlify/content/post') %>% 
  fs::dir_ls(
    type = 'directory'
  )
dirs_post

dirs_post_info <- 
  dirs_post %>% 
  fs::file_info() %>% 
  arrange(desc(birth_time)) %>% 
  select(path, birth_time)
dirs_post_info

# loop posts ---
copy_post <- function(dir_post) {
  # dir_post <- dirs_post_info$path[2]
  paths_post <- dir_post %>% fs::dir_ls()
  paths_post
  
  # TODO: Check for both Rmd and md here.
  path_index <- paths_post %>% str_subset('index[.]md$')
  assertthat::assert_that(length(path_index) == 1L)
  
  metadata <- path_index %>% rmarkdown::yaml_front_matter()
  metadata
  date_post <- metadata[['date']]
  date_post
  dir_parent <- path_index %>% dirname() %>% str_remove_all('(^.*\\/)')
  dir_dst <- fs::path(dir_root, 'itsmetony/_posts', sprintf('%s-%s', date_post, dir_parent))
  fs::dir_create(dir_dst)
  
  paths_post_info <-
    paths_post %>% 
    str_subset('html$', negate = TRUE) %>% 
    tibble(path_src = .) %>% 
    mutate(
      basename = path_src %>% basename(),
      path_dst = basename %>% fs::path(dir_dst, .)
    ) %>% 
    mutate(is_index = path_dst %>% str_detect('index.*md$')) %>% 
    mutate(
      across(
        path_dst, 
        list(
          renamed = ~if_else(
            is_index, 
            str_replace(.x, 'index', path_src %>% dirname() %>% basename()),
            .x %>% as.character()
          )
        )
      )
    )
  paths_post_info
  
  paths_post_info %>% 
    mutate(res = pmap(list(path_src, path_dst, path_dst_renamed), ~fs::file_copy(..1, ..2, overwrite = T) %>% file.rename(..3))) %>% 
    select(-res)
  
  path_index_new <- paths_post_info %>% filter(is_index) %>% pull(path_dst_renamed)
  path_index_new
  
  # Tbere could be 2 of these.
  rewrite_post <- function(path_index_new) {
    metadata_new <- path_index_new %>% rmarkdown::yaml_front_matter()
    
    lines <- 
      path_index_new %>% 
      read_lines() %>% 
      tibble(line = .) %>% 
      mutate(idx = row_number()) %>% 
      relocate(idx)
    lines
    
    header_idx <- 
      lines %>% 
      mutate(is_yaml_stop = line %>% str_detect('^---$')) %>% 
      filter(is_yaml_stop) %>% 
      # There may be more due to `---` being used for underlining titles.
      filter(row_number() <= 2L) %>% 
      pull(idx)
    header_idx
    
    body <-
      lines %>% 
      mutate(is_header = if_else(between(idx, header_idx[1], header_idx[2]), TRUE, FALSE)) %>% 
      filter(!is_header) %>% 
      # select(line) %>% 
      pull(line)
    body
    
    new_header <- glue::glue("
---
title: {metadata_new$title}
description: '{metadata_new$title}'
author:
  - name: Tony ElHabr
    url: 'https://twitter.com/TonyElHabr'
base_url: 'https//tonyelhabr.rbind.io'
date: {metadata_new$date}
categories:
{metadata_new$tags %>% paste0('  - ', ., collapse = '\n')}
output: 
  distill::distill_article:
    toc: true
    toc_depth: 3
    self_contained: false
preview: {metadata_new$header$image}
twitter:
  site: '@TonyElHabr'
  creator: '@TonyElHabr'
---
")
    new_header
    write_lines(c(new_header, body), path_index_new)
  }
  path_index_new %>% walk(rewrite_post)
}

copy_post_safely <- safely(copy_post, otherwise = NULL)
res_post <-
  dirs_post_info %>% 
  # slice(c(1:10)) # %>% 
  # slice(1) %>% 
  filter(str_detect(path, 'gallery', negate = TRUE)) %>% 
  slice(c(11:n())) %>% 
  mutate(res = map(path, copy_post_safely))
res_post %>% 
  unnest_wider(res) %>% 
  select(error)

# projects ---
dirs_proj <-
  fs::path(dir_root, 'netlify/content/project') %>% 
  fs::dir_ls(
    type = 'directory'
  )
dirs_proj

# loop projects ---
dir_proj <- dirs_proj[1]
paths_proj <- dir_proj %>% fs::dir_ls()
paths_proj

path_index_md <- paths_proj %>% str_subset('index')
lines <- 
  path_index_md %>% 
  read_lines() %>% 
  tibble(line = .) %>% 
  mutate(idx = row_number()) %>% 
  relocate(idx) %>% 
  mutate(is_stop = if_else(line == '+++', TRUE, FALSE))
lines
summ <- lines %>% filter(line %>% str_detect('^summary')) %>% pull(line) %>% str_remove_all('^summary\\s[=]\\s\\"|\\"$') %>% str_trim()
summ

header_idx <- 
  lines %>% 
  filter(is_stop) %>% 
  filter(row_number() <= 2L) %>% 
  pull(idx)
header_idx

body <- lines %>% filter(idx > header_idx[2]) %>% filter(line != '')
body
res <-
  body %>% 
  add_row(line = glue::glue('+ ___Summary:___ {summ}'), .before = 0)
res

# end projects ----

subscribe_html <- glue::glue("
<form method='post' action='https://blogtrottr.com'>
  <p>Enjoy this blog? Get notified of new posts via email:</p>
  <input type='text' name='btr_email' />
  <input type='hidden' name='btr_url'
         value='https://beta.rstudioconnect.com/content/11424/index.xml'/>
  <input type='hidden' name='schedule_type' value='0' />
  <input type='submit' value='Subscribe' />
</form>
")


write_lines(subscribe_html, fs::path(dir_root, '_subscribe.html'))

custom_html <- glue::glue("
 <div class='sidebar-section custom'>
  <h3>About</h3>
  <a href='https://www.r-project.org/'>R</a> and Other Things
</div>

<div class='sidebar-section custom'>
  <h3>Search</h3>
  <form action='https://duckduckgo.com/'>
    <input type='text' name='q' placeholder='Search...'>
    <input type='hidden' name='sites' 
      value='https://tonyelhabr.rbind.io'> <!-- Replace with your blog URL -->
    <!--<button type='submit'>Search</button>-->
  </form>
</div> 
")

write_lines(custom_html, fs::path(dir_root, '_custom.html'))

dir_dst_static <- fs::path(dir_dst, 'static')
fs::dir_create(dir_dst_static)
fs::copy(fs::path(dir_root, 'static/img/tony-tidydevday-2.jpg'), fs::path(dir_dst_static, 'site-preview.png'))

about_rmd <- glue::glue("
---
title: 'whoami'
---

```{=html}
<style>
d-article li {
    margin-bottom: 0em;
}
d-article h3 {
    margin-bottom: 0em;
    margin-top: 0em;
}
</style>
```
### Education

-   M.S. in Analytics (Data Science), May 2020 \| Georgia Institute of Technology
-   B.S. in Electrical Engineering, May 2016 \| The University of Texas at Austin

### Interests

-   Energy markets
-   Sports analytics

### Contact

-   [Twitter](https://twitter.com/TonyElHabr)
-   [Website](https://tonyelhabr.rbind.io)
-   [GitHub](https://github.com/tonyelhabr)
-   [LinkedIn](https://www.linkedin.com/in/anthony-elhabr)
")

write_lines(about_rmd, fs::path(dir_dst, 'about.Rmd'))

projects_rmd <- glue::glue("
---
title: 'Projects'
---

```{=html}
<style>
d-article li {
    margin-bottom: 0em;
}
d-article h3 {
    margin-bottom: 0em;
    margin-top: 0em;
}
</style>
```
### Historical and Predicted Zillow Home Value Index (ZHVI) for Austin, Texas

Zillow Home Value Index (ZHVI) is a metric that quantifies the market value of homes. Using published ZHVI values for Austin, Texas (from April 1996 through September 2019), models and predictions are made (up through December 2029) for every combination of zip code and ZHVI series (173 total) using Facebook's open-sourced prophet package.

+ __Time:__ Nov. - Dec. 2019
+ __Duration:__ 6 - 8 weeks
+ __Goal(s)/Learning Outcome(s):__
  + Gain experience with the `{prophet}` R package and Tableau.
  + Complete a semester-end project for CSE6242x class - Data and Visual Analytics (fall 2019) in Georga Tech's MSA program.
  + Collaborate in a team.
+ __Deliverable(s):__
  + https://github.com/tonyelhabr/cse-6242-project

")

write_lines(projects_rmd, fs::path(dir_dst, 'projects.Rmd'))

site_yml <- glue::glue("
name: 'itsmetony'
title: 'itsmetony'
description: |
 R And Other Things
base_url: https://tonyelhabr.rbind.io
output_dir: 'docs'
navbar:
  right:
    - text: 'Home'
      href: index.html
    - text: 'Projects'
      href: projects.html
    - text: 'About'
      href: about.html
    - icon: fa fa-twitter
      href: https://twitter.com/TonyElHabr
    - icon: fa fa-github
      href: https://github.com/tonyelhabr
    - icon: fa fa-rss
      href: index.xml
collections:
  posts:
    share: [twitter, linkedin]
    subscribe: _subscribe.html
    custom: _custom.html
google_analytics: 'UA-79842648-2'
twitter:
  site: '@TonyElHabr'
  creator: '@TonyElHabr'
output: 
  distill::distill_article:
    css: styles.css
preview: static/site-preview.png
rss:
  full_content: false
")
write_lines(site_yml, fs::path(dir_root, '_site.yml'))
