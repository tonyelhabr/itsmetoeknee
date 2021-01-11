---
title: NBA Team Twitter Analysis Flexdashboard
description: 'NBA Team Twitter Analysis Flexdashboard'
author:
  - name: Tony ElHabr
    url: 'https://twitter.com/TonyElHabr'
base_url: 'https//tonyelhabr.rbind.io'
date: 2018-03-11
categories:
  - r
  - flexdashboard
  - nba
  - tidytext
output: 
  distill::distill_article:
    toc: true
    toc_depth: 3
    self_contained: false
preview: featured.jpg
twitter:
  site: '@TonyElHabr'
  creator: '@TonyElHabr'
---

I just wrapped up a mini-project that allowed me to do a handful of
things I've been meaning to do:

-   Try out the [`{flexdashboard}`
    package](https://rmarkdown.rstudio.com/flexdashboard/), which is
    [supposed to be good for prototypying larger
    dashboards](/post/using-flexdashboard) (perhaps created with
    [`{shinydashboard}`](https://rstudio.github.io/shinydashboard/).
-   Test out my (mostly completed) personal [`{tetext}`
    package](https://github.com/tonyelhabr/tetext) for quick and tidy
    text analysis. (It implements a handful of the techniques shown by
    [David Robinson](http://varianceexplained.org/) and [Julia
    Silge](https://juliasilge.com/blog/), in their blogs and in their
    [*Tidy Text Mining with R* book](https://www.tidytextmining.com/).
-   Explore the interaction of social media and the NBA, which is well
    regarded for leading the sports industry in engaging fans through
    modern technological means.

{{% staticref "files/nba-tms.html" %}}The dashboard can be accessed here.{{% /staticref %}}
