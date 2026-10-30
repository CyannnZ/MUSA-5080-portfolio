# Run from the portfolio root after rendering Assignment 5.
suppressPackageStartupMessages({library(tidyverse); library(sf); library(spdep)})
x <- readRDS("data/assignment5/model_results.rds")
p <- read_csv("assignments/team_Mahjong_predictions.csv", show_col_types = FALSE)
t <- read_csv("data/assignment5/chicago_test.csv", show_col_types = FALSE)
ref <- read_csv("data/assignment5/reference_ns_predictions.csv", show_col_types = FALSE)
design <- readRDS("data/assignment5/validation_design.rds")
expected <- c(paste0("m", 1:10), paste0("E", 1:4), "NS")
stopifnot(x$final_name == "NS", identical(names(x$formulas), expected),
  identical(x$fold_id, design$fold_id), identical(p$id, t$id), identical(p$id, ref$id),
  nrow(p) == 5789L, !anyDuplicated(p$id), all(is.finite(p$pred)), sum(p$pred < 0) == 2L,
  x$m_final$rank == 124L, length(coef(x$m_final)) == 124L,
  max(abs(predict(x$m_final, x$test) - p$pred)) < 1e-5,
  max(abs(p$pred - ref$pred)) < 1e-5,
  all(vapply(split(x$fold_id, x$diagnostics$location_id),
    function(z) length(unique(z)) == 1L, logical(1))))
vars <- all.vars(delete.response(terms(x$f_final)))
stopifnot(!any(c("income", "income_group", "sale_price", "ratio", "pred", "residual") %in% vars))
for (name in expected) {
  m <- x$model_log %>% filter(model == name)
  e <- x$train$sale_price - x$oof[[name]]
  scores <- vapply(1:10, function(k) sqrt(mean(e[x$fold_id == k]^2)), numeric(1))
  stopifnot(abs(mean(scores) - m$cv_rmse) < 1e-6,
    abs(sqrt(mean(e^2)) - m$pooled_oof_rmse) < 1e-6,
    abs(unname(moran(e, x$weights, length(e), Szero(x$weights))$I) - m$moran_i) < 1e-10)
}
final <- x$model_log %>% filter(model == "NS")
low <- x$diagnostics %>% filter(income_group == "Under $50k")
stopifnot(abs(final$cv_rmse - 179512.1571931076) < 1e-6,
  abs(median(low$ratio) - final$low_income_median_ratio) < 1e-10,
  sum(x$oof$NS < 0) == 10L)
source <- readLines("assignments/assignment 5.qmd")
a <- grep("label: final-formula", source)
b <- which(seq_along(source) > a & source == "```")[[1]]
display <- new.env()
eval(parse(text = paste(source[(a + 1):(b - 1)], collapse = "\n")), display)
stopifnot(identical(attr(terms(display$f_final), "term.labels"),
                    attr(terms(x$f_final), "term.labels")))
write_csv(tibble(check = c("NS_model_and_annotated_formula", "all_15_models_recomputed",
  "original_grouped_folds", "no_outcome_or_income_predictors", "all_test_ids_and_predictions",
  "source_NS_predictions_reproduced", "income_diagnostics_recomputed"), passed = TRUE),
  "data/assignment5/verification.csv")
print(final %>% select(model, parameters, cv_rmse, moran_i, oof_negative, test_negative))
