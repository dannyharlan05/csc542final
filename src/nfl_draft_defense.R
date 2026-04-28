# CSC 642 Group Project
# Prediction of PFF draft grade from NCAA data.

library(tidyverse)
library(boot)

options(scipen = 999)

file <- "C:/Users/brand/OneDrive - University of Miami/Obsidian/UMiami/2025-2026/Notes/CSC642/group_project/data/last_set.csv"
data <- read_csv(file)

data <- data %>%
  separate(`1st_combine__ht`, 
           into = c("feet", "inches"), 
           sep = "-", 
           convert = TRUE, 
           fill = "right") %>%
  mutate(`1st_combine_ht_in` = (feet * 12) + replace_na(inches, 0)) %>%
  mutate(`1st_combine_ht_in` = replace_na(`1st_combine_ht_in`, 0)) %>%
  mutate(draft_grade_over_75 = if_else(overall_nfl_grade >= 75, 1, 0))

def_pos <- c("CB", "S", "LB", "ED", "DI")
defense <- data %>%
  filter(`1st_position` %in% def_pos) %>%
  select(-contains("offense"))


dline_features <- c("1st_draft__age",
                    "1st_combine__3cone",
                    "1st_combine__bench",
                    "1st_combine_ht_in", # not sure about height yet
                    "1st_combine__wt", # big guys need to be big
                    "last_defense_summary_ncaa__player_game_count", # long NCAA careers = experienced
                    "last_defense_summary_ncaa__sacks",
                    "last_defense_summary_ncaa__batted_passes",
                    "last_defense_summary_ncaa__tackles_for_loss",
                    "last_defense_summary_ncaa__hurries",
                    "last_defense_summary_ncaa__total_pressures"
                    )

lb_features <- c("1st_draft__age",
                 "1st_combine__bench",
                 "1st_combine__broad jump",
                 "1st_combine_ht_in",
                 "1st_combine__shuttle",
                 "last_defense_summary_ncaa__player_game_count",
                 "last_defense_summary_ncaa__snap_counts_defense",
                 "last_defense_summary_ncaa__sacks",
                 "last_defense_summary_ncaa__tackles_for_loss",
                 "last_defense_summary_ncaa__hits")

sec_features <- c("1st_draft__age",
                        "1st_combine__40yd",
                        "1st_combine_ht_in",
                        "1st_combine__vertical",
                        "last_defense_summary_ncaa__player_game_count",
                        "last_defense_summary_ncaa__receptions",
                        "last_defense_coverage_summary_ncaa__touchdowns",
                        "last_defense_coverage_summary_ncaa__pass_break_ups",
                        "last_defense_summary_ncaa__catch_rate",
                        "last_defense_coverage_summary_ncaa__missed_tackle_rate",
                        "last_defense_summary_ncaa__targets",
                        "last_defense_coverage_summary_ncaa__forced_incompletion_rate")

# ---- # of Players (DEFENSE ONLY) ----

def_pos <- c("CB", "S", "LB", "ED", "DI")

def_counts <- data %>%
  filter(`1st_position` %in% def_pos) %>%
  mutate(
    pos_group = case_when(
      `1st_position` %in% c("DI", "ED") ~ "DL",
      `1st_position` %in% c("CB", "S") ~ "SEC",
      `1st_position` == "LB" ~ "LB"
    ),
    grade_group = if_else(overall_nfl_grade >= 75, "PFF ≥ 75", "PFF < 75")
  )

# Overall
overall <- def_counts %>%
  count(pos_group) %>%
  pivot_wider(names_from = pos_group, values_from = n) %>%
  mutate(Row = "Overall")

# ≥ 75
high <- def_counts %>%
  filter(grade_group == "PFF ≥ 75") %>%
  count(pos_group) %>%
  pivot_wider(names_from = pos_group, values_from = n) %>%
  mutate(Row = "PFF ≥ 75")

# < 75
low <- def_counts %>%
  filter(grade_group == "PFF < 75") %>%
  count(pos_group) %>%
  pivot_wider(names_from = pos_group, values_from = n) %>%
  mutate(Row = "PFF < 75")

# Combine
final_table <- bind_rows(overall, high, low) %>%
  select(Row, DL, SEC, LB) %>%
  replace(is.na(.), 0)

final_table


# TRAIN/TEST SETS
train_set <- sample(length(defense), length(defense)*0.8)
X.train <- defense[train_set, ]
X.test <- defense[-train_set,]

# dline
X.train.dline <- X.train %>%
  filter(`1st_position` == "ED" | `1st_position` == "DI") %>%
  select(player, draft_grade_over_75, any_of(dline_features))

X.test.dline <- X.test %>%
  filter(`1st_position` == "ED" | `1st_position` == "DI") %>%
  select(player, draft_grade_over_75, any_of(dline_features))

dline.means <- X.train.dline %>%
  summarize(across(all_of(dline_features), ~mean(.x, na.rm = T)))

dline.sds <- X.train.dline %>%
  summarize(across(all_of(dline_features), ~sd(.x, na.rm = T)))

train.dline <- X.train.dline %>%
  mutate(across(all_of(dline_features),
                ~ (.x - dline.means[[cur_column()]]) / dline.sds[[cur_column()]])) %>%
  as.data.frame()

test.dline <- X.test.dline %>%
  mutate(across(all_of(dline_features),
                ~ (.x - dline.means[[cur_column()]]) / dline.sds[[cur_column()]])) %>%
  as.data.frame()

# linebacker
X.train.lb <- X.train %>%
  filter(`1st_position` == "LB") %>%
  select(player, draft_grade_over_75, any_of(lb_features))

X.test.lb <- X.test %>%
  filter(`1st_position` == "LB") %>%
  select(player, draft_grade_over_75, any_of(lb_features))

lb.means <- X.train.lb %>%
  summarize(across(all_of(lb_features), ~mean(.x, na.rm = T)))

lb.sds <- X.train.lb %>%
  summarize(across(all_of(lb_features), ~sd(.x, na.rm = T)))

train.lb <- X.train.lb %>%
  mutate(across(all_of(lb_features),
                ~ (.x - lb.means[[cur_column()]]) / lb.sds[[cur_column()]])) %>%
  as.data.frame()

test.lb <- X.test.lb %>%
  mutate(across(all_of(lb_features),
                ~ (.x - lb.means[[cur_column()]]) / lb.sds[[cur_column()]])) %>%
  as.data.frame()

# secondary
X.train.sec <- X.train %>%
  filter(`1st_position` == "CB" | `1st_position` == "S") %>%
  select(player, draft_grade_over_75, any_of(sec_features))

X.test.sec <- X.test %>%
  filter(`1st_position` == "CB" | `1st_position` == "S") %>%
  select(player, draft_grade_over_75, any_of(sec_features))

sec.means <- X.train.sec %>%
  summarize(across(all_of(sec_features), ~mean(.x, na.rm = T)))

sec.sds <- X.train.sec %>%
  summarize(across(all_of(sec_features), ~sd(.x, na.rm = T)))

train.sec <- X.train.sec %>%
  mutate(across(all_of(sec_features),
                ~ (.x - sec.means[[cur_column()]]) / sec.sds[[cur_column()]])) %>%
  as.data.frame()

test.sec <- X.test.sec %>%
  mutate(across(all_of(sec_features),
                ~ (.x - sec.means[[cur_column()]]) / sec.sds[[cur_column()]])) %>%
  as.data.frame()


# LR FITS
log.reg.dline <- glm(draft_grade_over_75 ~ ., data = train.dline %>% select(-player), family = binomial)
log.reg.lb <- glm(draft_grade_over_75 ~ ., data = train.lb %>% select(-player), family = binomial)
log.reg.sec <- glm(draft_grade_over_75 ~ ., data = train.sec %>% select(-player), family = binomial)

# predictions (test set)
test.dline.probs <- predict(log.reg.dline, test.dline %>% select(-player), type="response")
test.dline.pred <- ifelse(test.dline.probs >= 0.2, 1, 0)

# train set
train.dline.probs <- predict(log.reg.dline, train.dline %>% select(-player), type="response")
train.dline.pred <- ifelse(train.dline.probs >= 0.2, 1, 0)

# predictions (test set)
test.lb.probs <- predict(log.reg.lb, test.lb %>% select(-player), type="response")
test.lb.pred <- ifelse(test.lb.probs >= 0.2, 1, 0)

# train set
train.lb.probs <- predict(log.reg.lb, train.lb %>% select(-player), type="response")
train.lb.pred <- ifelse(train.lb.probs >= 0.2, 1, 0)

# predictions (test set)
test.sec.probs <- predict(log.reg.sec, test.sec %>% select(-player), type="response")
test.sec.pred <- ifelse(test.sec.probs >= 0.2, 1, 0)

# train set
train.sec.probs <- predict(log.reg.sec, train.sec %>% select(-player), type="response")
train.sec.pred <- ifelse(train.sec.probs >= 0.2, 1, 0)


# EVALUATE
# dline
cm.test.dline <- table(actual = test.dline$draft_grade_over_75,
                       pred = test.dline.pred)

cm.train.dline <- table(actual = train.dline$draft_grade_over_75,
                       pred = train.dline.pred)

dline.train.acc <- sum(diag(cm.train.dline)) / sum(cm.train.dline)
dline.test.acc <- sum(diag(cm.test.dline)) / sum(cm.test.dline)
dline.test.acc
cm.test.dline

# linebacker
cm.test.lb <- table(actual = test.lb$draft_grade_over_75,
                       pred = test.lb.pred)

cm.train.lb <- table(actual = train.lb$draft_grade_over_75,
                        pred = train.lb.pred)

lb.train.acc <- sum(diag(cm.train.lb)) / sum(cm.train.lb)
lb.test.acc <- sum(diag(cm.test.lb)) / sum(cm.test.lb)
lb.test.acc
cm.test.lb

# secondary
cm.test.sec <- table(actual = test.sec$draft_grade_over_75,
                       pred = test.sec.pred)

cm.train.sec <- table(actual = train.sec$draft_grade_over_75,
                        pred = train.sec.pred)

sec.train.acc <- sum(diag(cm.train.sec)) / sum(cm.train.sec)
sec.test.acc <- sum(diag(cm.test.sec)) / sum(cm.test.sec)
sec.test.acc
cm.test.sec

# 5-fold CV on 80% training set
set.seed(1)
cv.error.5.dline <- cv.glm(train.dline %>% select(-player),log.reg.dline,K=5)$delta[1]
cv.error.5.lb <- cv.glm(train.lb %>% select(-player),log.reg.lb,K=5)$delta[1]
cv.error.5.sec <- cv.glm(train.sec %>% select(-player),log.reg.sec,K=5)$delta[1]



# PROB TABLES
dline.train.probs.tbl <- train.dline %>%
  mutate(prob_over_75 = train.dline.probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

dline.test.probs.tbl <- test.dline %>%
  mutate(prob_over_75 = test.dline.probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

dline.train.probs.tbl


lb.train.probs.tbl <- train.lb %>%
  mutate(prob_over_75 = train.lb.probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

lb.test.probs.tbl <- test.lb %>%
  mutate(prob_over_75 = test.lb.probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

lb.train.probs.tbl


sec.train.probs.tbl <- train.sec %>%
  mutate(prob_over_75 = train.sec.probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

sec.test.probs.tbl <- test.sec %>%
  mutate(prob_over_75 = test.sec.probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

sec.train.probs.tbl


# FEATURE WEIGHTS
dline.coef.tbl <- broom::tidy(log.reg.dline) %>%
  filter(term != "(Intercept)") %>%
  mutate(direction = if_else(estimate > 0, "positive", "negative")) %>%
  arrange(desc(abs(estimate)))

dline.coef.tbl


lb.coef.tbl <- broom::tidy(log.reg.lb) %>%
  filter(term != "(Intercept)") %>%
  mutate(direction = if_else(estimate > 0, "positive", "negative")) %>%
  arrange(desc(abs(estimate)))

lb.coef.tbl

sec.coef.tbl <- broom::tidy(log.reg.sec) %>%
  filter(term != "(Intercept)") %>%
  mutate(direction = if_else(estimate > 0, "positive", "negative")) %>%
  arrange(desc(abs(estimate)))

sec.coef.tbl


library(pROC)
# ROC/PRC
# DL
roc_dline <- roc(test.dline$draft_grade_over_75, test.dline.probs)
auc_dline <- auc(roc_dline)
auc_dline

# LB
roc_lb <- roc(test.lb$draft_grade_over_75, test.lb.probs)
auc_lb <- auc(roc_lb)
auc_lb

# SEC
roc_sec <- roc(test.sec$draft_grade_over_75, test.sec.probs)
auc_sec <- auc(roc_sec)
auc_sec


#Combined AUROC
all_probs <- c(test.dline.probs, test.lb.probs, test.sec.probs)

all_actual <- c(
  test.dline$draft_grade_over_75,
  test.lb$draft_grade_over_75,
  test.sec$draft_grade_over_75
)
roc_all <- roc(all_actual, all_probs)
auc_all <- auc(roc_all)

auc_all
cat("Overall AUROC:", auc_all, "\n")
plot(roc_all, col = "black", main = "ROC Curve - Overall Defense")



# CV

### =========================
### 5-FOLD CROSS VALIDATION
### =========================
library(boot)
cost_error <- function(r, pi = 0) mean(abs(r - pi) > 0.5)
set.seed(1)

# DL
cv.dline <- cv.glm(
  train.dline %>% select(-player),
  log.reg.dline,
  cost = cost_error,
  K = 5
)
cv.dline.acc <- 1 - cv.dline$delta[1]

# LB
cv.lb <- cv.glm(
  train.lb %>% select(-player),
  log.reg.lb,
  cost = cost_error,
  K = 5
)
cv.lb.acc <- 1 - cv.lb$delta[1]


# SEC
cv.sec <- cv.glm(
  train.sec %>% select(-player),
  log.reg.sec,
  cost = cost_error,
  K = 5
)
cv.sec.acc <- 1 - cv.sec$delta[1]

# Print CV errors and accuracy
cat("DL CV Error:", cv.dline$delta[1], "\n", "DL Acc:", cv.dline.acc, "\n")
cat("LB CV Error:", cv.lb$delta[1], "\n", "LB Acc:", cv.lb.acc, "\n")
cat("SEC CV Error:", cv.sec$delta[1], "\n", "SEC Acc:", cv.sec.acc, "\n")


### =========================
### EXTRA IMBALANCE METRICS
### precision, recall, F1, AUPRC, class counts
### =========================


install.packages("PRROC")
library(PRROC)

get_metrics <- function(actual, pred, probs, name) {
  actual <- as.integer(actual)
  pred   <- as.integer(pred)
  
  tp <- sum(actual == 1 & pred == 1, na.rm = TRUE)
  tn <- sum(actual == 0 & pred == 0, na.rm = TRUE)
  fp <- sum(actual == 0 & pred == 1, na.rm = TRUE)
  fn <- sum(actual == 1 & pred == 0, na.rm = TRUE)
  
  precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
  f1        <- ifelse(is.na(precision) | is.na(recall) | (precision + recall == 0),
                      NA,
                      2 * precision * recall / (precision + recall))
  
  pr <- pr.curve(
    scores.class0 = probs[actual == 1],
    scores.class1 = probs[actual == 0],
    curve = TRUE
  )
  
  cat("\n=========================\n")
  cat(name, "\n")
  cat("=========================\n")
  cat("Class counts:\n")
  cat("Under 75 (0):", sum(actual == 0, na.rm = TRUE), "\n")
  cat("Over 75  (1):", sum(actual == 1, na.rm = TRUE), "\n\n")
  
  cat("Confusion Matrix:\n")
  print(table(actual = actual, pred = pred))
  
  cat("\nPrecision:", precision, "\n")
  cat("Recall:   ", recall, "\n")
  cat("F1 Score: ", f1, "\n")
  cat("AUPRC:    ", pr$auc.integral, "\n")
  
  plot(pr, main = paste("PR Curve -", name))
  
  return(data.frame(
    Position = name,
    Under_75 = sum(actual == 0, na.rm = TRUE),
    Over_75 = sum(actual == 1, na.rm = TRUE),
    Precision = precision,
    Recall = recall,
    F1 = f1,
    AUPRC = pr$auc.integral
  ))
}

dline_extra <- get_metrics(test.dline$draft_grade_over_75, test.dline.pred, test.dline.probs, "DL")
lb_extra <- get_metrics(test.lb$draft_grade_over_75, test.lb.pred, test.lb.probs, "LB")
sec_extra <- get_metrics(test.sec$draft_grade_over_75, test.sec.pred, test.sec.probs, "SEC")

all_extra_metrics <- bind_rows(dline_extra, lb_extra, sec_extra)
all_extra_metrics



# =========================
# OVERALL OFFENSE AUROC + AUPRC
# =========================

install.packages("pROC")
install.packages("PRROC")

library(pROC)
library(PRROC)

# combine all offensive test actuals + predicted probabilities
def_actual <- c(
  test.dline$draft_grade_over_75,
  test.lb$draft_grade_over_75,
  test.sec$draft_grade_over_75
)

def_probs <- c(
  test.dline.probs,
  test.lb.probs,
  test.sec.probs
)

# drop any missing values just in case
def_df <- data.frame(actual = def_actual, prob = def_probs) %>%
  drop_na()

# AUROC
def_roc <- roc(def_df$actual, def_df$prob, quiet = TRUE)
def_auroc <- as.numeric(auc(def_roc))

# AUPRC
def_pr <- pr.curve(
  scores.class0 = def_df$prob[def_df$actual == 1],
  scores.class1 = def_df$prob[def_df$actual == 0],
  curve = TRUE
)
def_auprc <- def_pr$auc.integral

cat("Overall Defense AUROC:", def_auroc, "\n")
cat("Overall Defense AUPRC:", def_auprc, "\n")

# optional small table for slide
defense_summary_metrics <- data.frame(
  Group = "Defense",
  AUROC = round(def_auroc, 3),
  AUPRC = round(def_auprc, 3)
)

defense_summary_metrics


# =========================
# COMBINED DEFNESE CURVES
# =========================

library(pROC)
library(PRROC)
library(dplyr)

# combine all offense predictions
def_actual <- c(
  test.dline$draft_grade_over_75,
  test.lb$draft_grade_over_75,
  test.sec$draft_grade_over_75
)

def_probs <- c(
  test.dline.probs,
  test.lb.probs,
  test.sec.probs
)

def_df <- data.frame(actual = def_actual, prob = def_probs) %>%
  drop_na()

# =========================
# ROC CURVE (AUROC)
# =========================

def_roc <- roc(def_df$actual, def_df$prob, quiet = TRUE)
plot(def_roc, main = "ROC Curve - Overall Defense")

auc(def_roc)  # prints AUROC

# =========================
# PR CURVE (AUPRC)
# =========================

def_pr <- pr.curve(
  scores.class0 = def_df$prob[def_df$actual == 1],
  scores.class1 = def_df$prob[def_df$actual == 0],
  curve = TRUE
)

plot(def_pr, main = "PR Curve - Overall Defense")

def_pr$auc.integral  # prints AUPRC



### =========================
### FIND OPTIMAL CUTOFF (F1) FOR ALL POSITIONS
### =========================

find_best_cutoff <- function(probs, actual, name) {
  thresholds <- seq(0.1, 0.9, by = 0.01)
  best_f1 <- 0
  best_t <- NA
  
  for (t in thresholds) {
    pred <- ifelse(probs >= t, 1, 0)
    
    tp <- sum(actual == 1 & pred == 1)
    fp <- sum(actual == 0 & pred == 1)
    fn <- sum(actual == 1 & pred == 0)
    
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    
    f1 <- ifelse((precision + recall) == 0, 0,
                 2 * precision * recall / (precision + recall))
    
    if (f1 > best_f1) {
      best_f1 <- f1
      best_t <- t
    }
  }
  
  cat("\n=====================\n")
  cat(name, "\n")
  cat("Best cutoff:", best_t, "\n")
  cat("Best F1:", best_f1, "\n")
  
  return(data.frame(Position = name, Best_Cutoff = best_t, Best_F1 = best_f1))
}

# Run for each position
dline.best <- find_best_cutoff(test.dline.probs, test.dline$draft_grade_over_75, "DL")
lb.best <- find_best_cutoff(test.lb.probs, test.lb$draft_grade_over_75, "LB")
sec.best <- find_best_cutoff(test.sec.probs, test.sec$draft_grade_over_75, "SEC")

# Combine results
all_best_cutoffs <- bind_rows(dline.best, lb.best, sec.best)
all_best_cutoffs


### =========================
### DOWNSAMPLING TRY (SEPARATE FROM ORIGINAL MODELS)
### =========================

library(PRROC)

get_simple_metrics <- function(actual, pred, probs, name) {
  actual <- as.integer(actual)
  pred   <- as.integer(pred)
  
  tp <- sum(actual == 1 & pred == 1, na.rm = TRUE)
  fp <- sum(actual == 0 & pred == 1, na.rm = TRUE)
  fn <- sum(actual == 1 & pred == 0, na.rm = TRUE)
  
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1        <- ifelse((precision + recall) == 0, 0,
                      2 * precision * recall / (precision + recall))
  
  pr <- pr.curve(
    scores.class0 = probs[actual == 1],
    scores.class1 = probs[actual == 0],
    curve = TRUE
  )
  
  data.frame(
    Position = name,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    AUPRC = pr$auc.integral
  )
}

downsample_fit <- function(train_df, test_df, cutoff = 0.2, drop_cols = character()) {
  train_use <- train_df %>% drop_na()
  test_use  <- test_df %>% drop_na()
  
  pos <- train_use %>% filter(draft_grade_over_75 == 1)
  neg <- train_use %>% filter(draft_grade_over_75 == 0)
  
  set.seed(1)
  neg_down <- neg %>% slice_sample(n = nrow(pos))
  
  train_down <- bind_rows(pos, neg_down)
  
  model <- glm(
    draft_grade_over_75 ~ .,
    data = train_down %>% select(-all_of(drop_cols)),
    family = binomial
  )
  
  probs <- predict(model, test_use %>% select(-all_of(drop_cols)), type = "response")
  pred  <- ifelse(probs >= cutoff, 1, 0)
  
  list(
    model = model,
    probs = probs,
    pred = pred,
    actual = test_use$draft_grade_over_75
  )
}

# DL
dline.down <- downsample_fit(train.dline, test.dline, cutoff = 0.2, drop_cols = "player")
dline.down.metrics <- get_simple_metrics(dline.down$actual, dline.down$pred, dline.down$probs, "DL_down")

# LB
lb.down <- downsample_fit(train.lb, test.lb, cutoff = 0.2, drop_cols = "player")
lb.down.metrics <- get_simple_metrics(lb.down$actual, lb.down$pred, lb.down$probs, "LB_down")

# SEC
sec.down <- downsample_fit(train.sec, test.sec, cutoff = 0.2, drop_cols = "player")
sec.down.metrics <- get_simple_metrics(sec.down$actual, sec.down$pred, sec.down$probs, "SEC_down")

# Combine downsampled results
downsample_results <- bind_rows(
  dline.down.metrics,
  lb.down.metrics,
  sec.down.metrics
)

downsample_results
