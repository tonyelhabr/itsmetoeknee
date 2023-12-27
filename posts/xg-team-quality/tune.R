library(tidymodels)
library(finetune)
library(workflowsets)

define_xgboost_spec <- function(...) {
  boost_tree(
    trees = 500,
    learn_rate = 0.01,
    tree_depth = tune(),
    min_n = tune(), 
    loss_reduction = tune(),
    sample_size = tune(), 
    mtry = tune(),
    stop_iter = tune()
  ) |>
    set_engine('xgboost') |> 
    set_mode('classification')
}

spec_base <- define_xgboost_spec()

elo_features <- rec_elo |> recipes::prep() |> recipes::juice() |> colnames()
spec_elo <- define_xgboost_spec(
  monotone_constraints = !!ifelse(elo_features %in% c('elo', 'elo_diff'), 1, 0))
)

grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  stop_iter(range = c(10L, 50L)),
  size = 50
)

wf_sets <- workflow_set(
  preproc = list(
    base = rec_base, 
    elo = rec_elo
  ),
  models = list(
    model = spec_base,
    model = spec_elo
  ),
  cross = FALSE
)

met_set <- metric_set(f_meas)
control <- control_race(
  save_pred = TRUE,
  parallel_over = 'everything',
  save_workflow = TRUE,
  verbose = TRUE,
  verbose_elim = TRUE
)

set.seed(42)
train_folds <- vfold_cv(train, strata = scores, v = 5)

options(tidymodels.dark = TRUE)
tuned_results <- workflow_map(
  wf_sets,
  fn = 'tune_race_anova',
  grid = grid,
  control = control,
  metrics = met_set,
  resamples = train_folds,
  seed = 42
)
qs::qsave(tuned_results, file.path(PROJ_DIR, 'tuned_results.qs'))
