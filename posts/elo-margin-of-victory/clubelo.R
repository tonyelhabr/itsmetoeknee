calculate_win_probability <- function(dr) {
  return(1 / (10^(-dr / 400) + 1))
}

calculate_points_exchange <- function(R, E, k = 20) {
  return((R - E) * k)
}

calculate_elo_margin <- function(delta_elo_all, margin, p_margin, p_all) {
  sum_sqrt_margin = sum(sqrt(1:length(p_margin)) * p_margin / p_all)
  delta_elo_1goal = delta_elo_all / sum_sqrt_margin
  return(delta_elo_1goal * sqrt(margin))
}

# Example usage
# Assuming dr = 100, R = 1 (win), margin = 2, p_margin = c(0.1, 0.2, 0.3, ...), p_all = 0.6

dr <- 1798 - 1660
R <- 0
margin <- 1
p_margin <- c(
  `0` = 0.238,
  `1` = 0.372,
  `2` = 0.219,
  `3` = 0.107,
  `4` = 0.0424,
  `5` = 0.0168,
  `6` = 0.00289,
  `7` = 0.00105,
  `8` = 0.000789,
  `9` = 0.000526
)
p_all <- sum(p_margin)

E <- calculate_win_probability(dr)
delta_elo_all <- calculate_points_exchange(R, E)
delta_elo_margin <- calculate_elo_margin(delta_elo_all, margin, p_margin, p_all)

print(delta_elo_margin)


## Man United loss to Newscatle on 12/30/2023. standard show -13.8, but clubelo has it at -10.3
standard_elo(1798, 1660, 0) - 1798
