library(tidyverse)
library(boot)

options(scipen = 999)  # stop scientific notation

data <- read_csv("last_set.csv")
data <- data %>%
  filter(player != "jacoby stevens")

data <- data %>%
  separate(`1st_combine__ht`,
           into = c("feet", "inches"),
           sep = "-",
           convert = TRUE,
           fill = "right") %>%
  mutate(`1st_combine_ht_in` = (feet * 12) + replace_na(inches, 0)) %>%
  mutate(`1st_combine_ht_in` = replace_na(`1st_combine_ht_in`, 0)) %>%
  mutate(draft_grade_over_75 = if_else(overall_nfl_grade >= 75, 1L, 0L))

off_pos <- c("QB","HB","WR","TE","T","G","C")

offense <- data %>%
  filter(`1st_position` %in% off_pos) %>%
  select(-contains("defense"))

set.seed(1)
train_set <- sample(nrow(offense), nrow(offense)*0.8)
X.train <- offense[train_set, ]
X.test  <- offense[-train_set, ]

qb_x <- c( "1st_draft__age", 
            "last_passing_summary_ncaa__accuracy_percent", 
           "last_passing_summary_ncaa__ypa", "1st_combine_ht_in", 'last_rushing_summary_ncaa__scramble_yards')
# keep player so joins are clean
qb_train_raw <- X.train %>%
  filter(`1st_position` == "QB") %>%
  select(player, draft_grade_over_75, any_of(qb_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

qb_test_raw <- X.test %>%
  filter(`1st_position` == "QB") %>%
  select(player, draft_grade_over_75, any_of(qb_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

# learn scaling params from TRAIN only
qb_means <- qb_train_raw %>%
  summarise(across(all_of(qb_x), ~mean(.x, na.rm = TRUE)))

qb_sds <- qb_train_raw %>%
  summarise(across(all_of(qb_x), ~sd(.x, na.rm = TRUE)))

# apply train scaling to BOTH train and test
qb_train <- qb_train_raw %>%
  mutate(across(all_of(qb_x),
                ~ (.x - qb_means[[cur_column()]]) / qb_sds[[cur_column()]])) %>%
  as.data.frame()

qb_test <- qb_test_raw %>%
  mutate(across(all_of(qb_x),
                ~ (.x - qb_means[[cur_column()]]) / qb_sds[[cur_column()]])) %>%
  as.data.frame()

# fit
log.reg.qb <- glm(draft_grade_over_75 ~ ., data = qb_train %>% select(-player), family = binomial)

# predict
qb_probs <- predict(log.reg.qb, qb_test %>% select(-player), type = "response")
qb_pred  <- ifelse(qb_probs >= 0.2, 1, 0)

train_probs <- predict(log.reg.qb, qb_train %>% select(-player), type = "response")
train_pred  <- ifelse(train_probs >= 0.2, 1, 0)

# evaluate
cm_test <- table(actual = qb_test$draft_grade_over_75, pred = qb_pred)
cm_train <- table(actual = qb_train$draft_grade_over_75, pred = train_pred)

cm_train
acc_train <- sum(diag(cm_train)) / sum(cm_train)
acc_train

cm_test
acc_test <- sum(diag(cm_test)) / sum(cm_test)
acc_test



# ---- Stats by Grade table ----

stats_by_grade <- data %>%
  mutate(
    grade_group = case_when(
      overall_nfl_grade >= 75 ~ "PFF ≥ 75",
      overall_nfl_grade < 75  ~ "PFF < 75"
    )
  ) %>%
  summarise(
    `Stats by Grade` = "Overall",
    `#`   = sum(!is.na(overall_nfl_grade)),
    Mean  = mean(overall_nfl_grade, na.rm = TRUE),
    SD    = sd(overall_nfl_grade, na.rm = TRUE),
    Min   = min(overall_nfl_grade, na.rm = TRUE),
    Max   = max(overall_nfl_grade, na.rm = TRUE)
  ) %>%
  bind_rows(
    data %>%
      mutate(
        grade_group = case_when(
          overall_nfl_grade >= 75 ~ "PFF ≥ 75",
          overall_nfl_grade < 75  ~ "PFF < 75"
        )
      ) %>%
      group_by(grade_group) %>%
      summarise(
        `#`   = sum(!is.na(overall_nfl_grade)),
        Mean  = mean(overall_nfl_grade, na.rm = TRUE),
        SD    = sd(overall_nfl_grade, na.rm = TRUE),
        Min   = min(overall_nfl_grade, na.rm = TRUE),
        Max   = max(overall_nfl_grade, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(`Stats by Grade` = grade_group)
  ) %>%
  mutate(
    Mean = round(Mean, 2),
    SD   = round(SD, 2),
    Min  = round(Min, 2),
    Max  = round(Max, 2)
  )

stats_by_grade

# ---- # of Players (OFFENSE ONLY, grouped to QB/RB/WR/TE/OL) ----

off_pos <- c("QB","HB","WR","TE","T","G","C")

off_counts <- data %>%
  filter(`1st_position` %in% off_pos) %>%
  mutate(
    pos_group = case_when(
      `1st_position` == "QB" ~ "QB",
      `1st_position` == "HB" ~ "RB",
      `1st_position` == "WR" ~ "WR",
      `1st_position` == "TE" ~ "TE",
      `1st_position` %in% c("T","G","C") ~ "OL"
    ),
    grade_group = if_else(overall_nfl_grade >= 75, "PFF ≥ 75", "PFF < 75")
  )

# Overall
overall <- off_counts %>%
  count(pos_group) %>%
  pivot_wider(names_from = pos_group, values_from = n) %>%
  mutate(Row = "Overall")

# ≥ 75
high <- off_counts %>%
  filter(grade_group == "PFF ≥ 75") %>%
  count(pos_group) %>%
  pivot_wider(names_from = pos_group, values_from = n) %>%
  mutate(Row = "PFF ≥ 75")

# < 75
low <- off_counts %>%
  filter(grade_group == "PFF < 75") %>%
  count(pos_group) %>%
  pivot_wider(names_from = pos_group, values_from = n) %>%
  mutate(Row = "PFF < 75")

# Combine
final_table <- bind_rows(overall, high, low) %>%
  select(Row, QB, RB, WR, TE, OL) %>%
  replace(is.na(.), 0)

final_table

# probability tables
qb_train_probs_tbl <- qb_train %>%
  mutate(prob_over_75 = train_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

qb_test_probs_tbl <- qb_test %>%
  mutate(prob_over_75 = qb_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

qb_train_probs_tbl
qb_test_probs_tbl

# coefficients (feature weights)
coef(log.reg.qb)
coef_tbl <- broom::tidy(log.reg.qb) %>%
  filter(term != "(Intercept)") %>%
  mutate(direction = if_else(estimate > 0, "positive", "negative")) %>%
  arrange(desc(abs(estimate)))

coef_tbl

### =========================
### RB model (train/test)
### =========================
options(scipen = 999)

rb_x <- c(
  "1st_draft__age",
  'last_rushing_summary_ncaa__yards_after_contact', 'last_rushing_summary_ncaa__yprr', 'last_rushing_summary_ncaa__elu_rush_mtf',
'last_rushing_summary_ncaa__grades_run_block', 'last_rushing_summary_ncaa__first_downs', 'last_rushing_summary_ncaa__elu_rush_mtf'
,'1st_combine__vertical')

rb_train_raw <- X.train %>%
  filter(`1st_position` == "HB") %>%
  select(player, draft_grade_over_75, any_of(rb_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

rb_test_raw <- X.test %>%
  filter(`1st_position` == "HB") %>%
  select(player, draft_grade_over_75, any_of(rb_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

# scaling params from TRAIN only
rb_means <- rb_train_raw %>%
  summarise(across(all_of(rb_x), ~mean(.x, na.rm = TRUE)))

rb_sds <- rb_train_raw %>%
  summarise(across(all_of(rb_x), ~sd(.x, na.rm = TRUE)))

# apply scaling (train params) to train + test
rb_train <- rb_train_raw %>%
  mutate(across(all_of(rb_x),
                ~ (.x - rb_means[[cur_column()]]) / rb_sds[[cur_column()]])) %>%
  as.data.frame()

rb_test <- rb_test_raw %>%
  mutate(across(all_of(rb_x),
                ~ (.x - rb_means[[cur_column()]]) / rb_sds[[cur_column()]])) %>%
  as.data.frame()

# fit
log.reg.rb <- glm(draft_grade_over_75 ~ ., data = rb_train %>% select(-player), family = binomial)

# predict
rb_probs <- predict(log.reg.rb, rb_test %>% select(-player), type = "response")
rb_pred  <- ifelse(rb_probs >= 0.2, 1, 0)

train_rb_probs <- predict(log.reg.rb, rb_train %>% select(-player), type = "response")
train_rb_pred  <- ifelse(train_rb_probs >= 0.2, 1, 0)

# evaluate
cm_train_rb <- table(actual = rb_train$draft_grade_over_75, pred = train_rb_pred)
cm_test_rb  <- table(actual = rb_test$draft_grade_over_75,  pred = rb_pred)

cm_train_rb
acc_train_rb <- sum(diag(cm_train_rb)) / sum(cm_train_rb)
acc_train_rb

cm_test_rb
acc_test_rb <- sum(diag(cm_test_rb)) / sum(cm_test_rb)
acc_test_rb

# probability tables
rb_train_probs_tbl <- rb_train %>%
  mutate(prob_over_75 = train_rb_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

rb_test_probs_tbl <- rb_test %>%
  mutate(prob_over_75 = rb_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

rb_train_probs_tbl
rb_test_probs_tbl

# coefficients (feature weights)
coef(log.reg.rb)

# optional: clean coef table with +/- direction
# install.packages("broom")  # if needed
rb_coef_tbl <- broom::tidy(log.reg.rb) %>%
  filter(term != "(Intercept)") %>%
  mutate(direction = if_else(estimate > 0, "positive", "negative")) %>%
  arrange(desc(abs(estimate)))

rb_coef_tbl


### =========================
### WR model (train/test)
### =========================

### WR FEATURES (adjust if needed)
wr_x <- c(
  "1st_draft__age",
  "1st_combine_ht_in",
  '1st_combine__wt',
  'last_receiving_summary_ncaa__yprr',
  'last_receiving_summary_ncaa__contested_catch_rate', 'last_receiving_summary_ncaa__yards_after_catch_per_reception', 'last_receiving_summary_ncaa__slot_rate'

)

# Raw WR datasets
wr_train_raw <- X.train %>%
  filter(`1st_position` == "WR") %>%
  select(player, draft_grade_over_75, any_of(wr_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

wr_test_raw <- X.test %>%
  filter(`1st_position` == "WR") %>%
  select(player, draft_grade_over_75, any_of(wr_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

# Learn scaling from TRAIN only
wr_means <- wr_train_raw %>%
  summarise(across(all_of(wr_x), ~mean(.x, na.rm = TRUE)))

wr_sds <- wr_train_raw %>%
  summarise(across(all_of(wr_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

# Apply scaling
wr_train <- wr_train_raw %>%
  mutate(across(all_of(wr_x),
                ~ (.x - wr_means[[cur_column()]]) / wr_sds[[cur_column()]]))

wr_test <- wr_test_raw %>%
  mutate(across(all_of(wr_x),
                ~ (.x - wr_means[[cur_column()]]) / wr_sds[[cur_column()]]))

# Drop rows with NA
wr_train_cc <- wr_train %>% drop_na(draft_grade_over_75, all_of(wr_x))

nrow(wr_train_cc)  # sanity check

# Fit
log.reg.wr <- glm(
  draft_grade_over_75 ~ .,
  data = wr_train_cc %>% select(-player),
  family = binomial
)

# Predictions
wr_probs <- predict(log.reg.wr, wr_test %>% select(-player), type = "response")
wr_pred  <- ifelse(wr_probs >= 0.2, 1, 0)

train_wr_probs <- predict(log.reg.wr, wr_train_cc %>% select(-player), type = "response")
train_wr_pred  <- ifelse(train_wr_probs >= 0.2, 1, 0)

# Evaluation
cm_train_wr <- table(actual = wr_train_cc$draft_grade_over_75, pred = train_wr_pred)
cm_test_wr  <- table(actual = wr_test$draft_grade_over_75, pred = wr_pred)

cm_train_wr
sum(diag(cm_train_wr)) / sum(cm_train_wr)

cm_test_wr
sum(diag(cm_test_wr)) / sum(cm_test_wr)

# Probability tables
wr_train_probs_tbl <- wr_train_cc %>%
  mutate(prob_over_75 = train_wr_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

wr_test_probs_tbl <- wr_test %>%
  mutate(prob_over_75 = wr_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

print(wr_train_probs_tbl, n = Inf)
print(wr_test_probs_tbl, n = Inf)

# Coefficients
coef(log.reg.wr)


### =========================
### TE model (train/test)
### =========================


### TE FEATURES (adjust if your column names differ) ## Adding more features to tight end just made it worse
te_x <- c(
  "1st_draft__age",
'last_receiving_summary_ncaa__targets', 'last_receiving_summary_ncaa__yards'
)

# Raw TE datasets
te_train_raw <- X.train %>%
  filter(`1st_position` == "TE") %>%
  select(player, draft_grade_over_75, any_of(te_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

te_test_raw <- X.test %>%
  filter(`1st_position` == "TE") %>%
  select(player, draft_grade_over_75, any_of(te_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

# Learn scaling from TRAIN only
te_means <- te_train_raw %>%
  summarise(across(all_of(te_x), ~mean(.x, na.rm = TRUE)))

te_sds <- te_train_raw %>%
  summarise(across(all_of(te_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

# Apply scaling
te_train <- te_train_raw %>%
  mutate(across(all_of(te_x),
                ~ (.x - te_means[[cur_column()]]) / te_sds[[cur_column()]]))

te_test <- te_test_raw %>%
  mutate(across(all_of(te_x),
                ~ (.x - te_means[[cur_column()]]) / te_sds[[cur_column()]]))

# Drop rows with NA (otherwise glm silently drops a ton and can error)
te_train_cc <- te_train %>% drop_na(draft_grade_over_75, all_of(te_x))

nrow(te_train_cc)  # sanity check (should be > 0)

# Fit
log.reg.te <- glm(
  draft_grade_over_75 ~ .,
  data = te_train_cc %>% select(-player),
  family = binomial
)

# Predictions
te_probs <- predict(log.reg.te, te_test %>% select(-player), type = "response")
te_pred  <- ifelse(te_probs >= 0.2, 1, 0)

train_te_probs <- predict(log.reg.te, te_train_cc %>% select(-player), type = "response")
train_te_pred  <- ifelse(train_te_probs >= 0.2, 1, 0)

# Evaluation
cm_train_te <- table(actual = te_train_cc$draft_grade_over_75, pred = train_te_pred)
cm_test_te  <- table(actual = te_test$draft_grade_over_75, pred = te_pred)

cm_train_te
sum(diag(cm_train_te)) / sum(cm_train_te)

cm_test_te
sum(diag(cm_test_te)) / sum(cm_test_te)

# Probability tables (no scientific notation)
options(scipen = 999)

te_train_probs_tbl <- te_train_cc %>%
  mutate(prob_over_75 = train_te_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

te_test_probs_tbl <- te_test %>%
  mutate(prob_over_75 = te_probs) %>%
  select(player, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

te_train_probs_tbl
te_test_probs_tbl

# Coefficients (feature direction + strength)
coef(log.reg.te)

### =========================
### TE model (train/test)
### =========================



### OL FEATURES (adjust names if needed) #Hard for stats to be better than mostly using grades for oline men
ol_x <- c(
  "1st_draft__age",
  # last season college pass/run blocking
  "last_offense_blocking_ncaa__grades_offense",
  "last_offense_blocking_ncaa__grades_pass_block",
  "last_offense_blocking_ncaa__grades_run_block",
  "last_offense_blocking_ncaa__pressures_allowed"
  

)

# Raw OL datasets (T/G/C)
ol_train_raw <- X.train %>%
  filter(`1st_position` %in% c("T","G","C")) %>%
  select(player, `1st_position`, draft_grade_over_75, any_of(ol_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

ol_test_raw <- X.test %>%
  filter(`1st_position` %in% c("T","G","C")) %>%
  select(player, `1st_position`, draft_grade_over_75, any_of(ol_x)) %>%
  mutate(draft_grade_over_75 = as.integer(draft_grade_over_75))

# Learn scaling from TRAIN only
ol_means <- ol_train_raw %>%
  summarise(across(all_of(ol_x), ~mean(.x, na.rm = TRUE)))

ol_sds <- ol_train_raw %>%
  summarise(across(all_of(ol_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

# Apply scaling
ol_train <- ol_train_raw %>%
  mutate(across(all_of(ol_x),
                ~ (.x - ol_means[[cur_column()]]) / ol_sds[[cur_column()]]))

ol_test <- ol_test_raw %>%
  mutate(across(all_of(ol_x),
                ~ (.x - ol_means[[cur_column()]]) / ol_sds[[cur_column()]]))

# Drop NA rows so glm doesn’t drop silently / error
ol_train_cc <- ol_train %>% drop_na(draft_grade_over_75, all_of(ol_x))
nrow(ol_train_cc)   # sanity check

# Fit
log.reg.ol <- glm(
  draft_grade_over_75 ~ .,
  data = ol_train_cc %>% select(-player, -`1st_position`),
  family = binomial
)

# Predictions
ol_probs <- predict(log.reg.ol, ol_test %>% select(-player, -`1st_position`), type = "response")
ol_pred  <- ifelse(ol_probs >= 0.2, 1, 0)

train_ol_probs <- predict(log.reg.ol, ol_train_cc %>% select(-player, -`1st_position`), type = "response")
train_ol_pred  <- ifelse(train_ol_probs >= 0.2, 1, 0)

# Evaluation
cm_train_ol <- table(actual = ol_train_cc$draft_grade_over_75, pred = train_ol_pred)
cm_test_ol  <- table(actual = ol_test$draft_grade_over_75, pred = ol_pred)

cm_train_ol
sum(diag(cm_train_ol)) / sum(cm_train_ol)

cm_test_ol
sum(diag(cm_test_ol)) / sum(cm_test_ol)

# Probability tables (no scientific notation)
options(scipen = 999)

ol_train_probs_tbl <- ol_train_cc %>%
  mutate(prob_over_75 = train_ol_probs) %>%
  select(player, `1st_position`, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

ol_test_probs_tbl <- ol_test %>%
  mutate(prob_over_75 = ol_probs) %>%
  select(player, `1st_position`, draft_grade_over_75, prob_over_75) %>%
  arrange(desc(prob_over_75))

ol_train_probs_tbl
ol_test_probs_tbl

# Coefficients
coef(log.reg.ol)





### =========================
### LINEAR REGRESSION MODELS
### same features by position
### target = overall_nfl_grade
### =========================

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
rsq  <- function(actual, pred) 1 - sum((actual - pred)^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)

### -------------------------
### QB linear regression
### -------------------------
qb_lin_train_raw <- X.train %>%
  filter(`1st_position` == "QB") %>%
  select(player, overall_nfl_grade, any_of(qb_x))

qb_lin_test_raw <- X.test %>%
  filter(`1st_position` == "QB") %>%
  select(player, overall_nfl_grade, any_of(qb_x))

qb_lin_means <- qb_lin_train_raw %>% summarise(across(all_of(qb_x), ~mean(.x, na.rm = TRUE)))
qb_lin_sds   <- qb_lin_train_raw %>% summarise(across(all_of(qb_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

qb_lin_train <- qb_lin_train_raw %>%
  mutate(across(all_of(qb_x), ~(.x - qb_lin_means[[cur_column()]]) / qb_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(qb_x)) %>%
  as.data.frame()

qb_lin_test <- qb_lin_test_raw %>%
  mutate(across(all_of(qb_x), ~(.x - qb_lin_means[[cur_column()]]) / qb_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(qb_x)) %>%
  as.data.frame()

lm.qb <- lm(overall_nfl_grade ~ ., data = qb_lin_train %>% select(-player))

qb_train_pred <- predict(lm.qb, qb_lin_train %>% select(-player))
qb_test_pred  <- predict(lm.qb, qb_lin_test  %>% select(-player))

cat("QB Train RMSE:", rmse(qb_lin_train$overall_nfl_grade, qb_train_pred), "\n")
cat("QB Train R2:  ", rsq(qb_lin_train$overall_nfl_grade, qb_train_pred), "\n")
cat("QB Test RMSE: ", rmse(qb_lin_test$overall_nfl_grade, qb_test_pred), "\n")
cat("QB Test R2:   ", rsq(qb_lin_test$overall_nfl_grade, qb_test_pred), "\n")

qb_lin_train_tbl <- qb_lin_train %>%
  mutate(pred_grade = qb_train_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

qb_lin_test_tbl <- qb_lin_test %>%
  mutate(pred_grade = qb_test_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

qb_lin_train_tbl
qb_lin_test_tbl


### -------------------------
### RB linear regression (HB)
### -------------------------
rb_lin_train_raw <- X.train %>%
  filter(`1st_position` == "HB") %>%
  select(player, overall_nfl_grade, any_of(rb_x))

rb_lin_test_raw <- X.test %>%
  filter(`1st_position` == "HB") %>%
  select(player, overall_nfl_grade, any_of(rb_x))

rb_lin_means <- rb_lin_train_raw %>% summarise(across(all_of(rb_x), ~mean(.x, na.rm = TRUE)))
rb_lin_sds   <- rb_lin_train_raw %>% summarise(across(all_of(rb_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

rb_lin_train <- rb_lin_train_raw %>%
  mutate(across(all_of(rb_x), ~(.x - rb_lin_means[[cur_column()]]) / rb_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(rb_x)) %>%
  as.data.frame()

rb_lin_test <- rb_lin_test_raw %>%
  mutate(across(all_of(rb_x), ~(.x - rb_lin_means[[cur_column()]]) / rb_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(rb_x)) %>%
  as.data.frame()

lm.rb <- lm(overall_nfl_grade ~ ., data = rb_lin_train %>% select(-player))

rb_train_pred <- predict(lm.rb, rb_lin_train %>% select(-player))
rb_test_pred  <- predict(lm.rb, rb_lin_test  %>% select(-player))

cat("RB Train RMSE:", rmse(rb_lin_train$overall_nfl_grade, rb_train_pred), "\n")
cat("RB Train R2:  ", rsq(rb_lin_train$overall_nfl_grade, rb_train_pred), "\n")
cat("RB Test RMSE: ", rmse(rb_lin_test$overall_nfl_grade, rb_test_pred), "\n")
cat("RB Test R2:   ", rsq(rb_lin_test$overall_nfl_grade, rb_test_pred), "\n")

rb_lin_train_tbl <- rb_lin_train %>%
  mutate(pred_grade = rb_train_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

rb_lin_test_tbl <- rb_lin_test %>%
  mutate(pred_grade = rb_test_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

rb_lin_train_tbl
rb_lin_test_tbl


### -------------------------
### WR linear regression
### -------------------------
wr_lin_train_raw <- X.train %>%
  filter(`1st_position` == "WR") %>%
  select(player, overall_nfl_grade, any_of(wr_x))

wr_lin_test_raw <- X.test %>%
  filter(`1st_position` == "WR") %>%
  select(player, overall_nfl_grade, any_of(wr_x))

wr_lin_means <- wr_lin_train_raw %>% summarise(across(all_of(wr_x), ~mean(.x, na.rm = TRUE)))
wr_lin_sds   <- wr_lin_train_raw %>% summarise(across(all_of(wr_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

wr_lin_train <- wr_lin_train_raw %>%
  mutate(across(all_of(wr_x), ~(.x - wr_lin_means[[cur_column()]]) / wr_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(wr_x)) %>%
  as.data.frame()

wr_lin_test <- wr_lin_test_raw %>%
  mutate(across(all_of(wr_x), ~(.x - wr_lin_means[[cur_column()]]) / wr_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(wr_x)) %>%
  as.data.frame()

lm.wr <- lm(overall_nfl_grade ~ ., data = wr_lin_train %>% select(-player))

wr_train_pred <- predict(lm.wr, wr_lin_train %>% select(-player))
wr_test_pred  <- predict(lm.wr, wr_lin_test  %>% select(-player))

cat("WR Train RMSE:", rmse(wr_lin_train$overall_nfl_grade, wr_train_pred), "\n")
cat("WR Train R2:  ", rsq(wr_lin_train$overall_nfl_grade, wr_train_pred), "\n")
cat("WR Test RMSE: ", rmse(wr_lin_test$overall_nfl_grade, wr_test_pred), "\n")
cat("WR Test R2:   ", rsq(wr_lin_test$overall_nfl_grade, wr_test_pred), "\n")

wr_lin_train_tbl <- wr_lin_train %>%
  mutate(pred_grade = wr_train_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

wr_lin_test_tbl <- wr_lin_test %>%
  mutate(pred_grade = wr_test_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

wr_lin_train_tbl
wr_lin_test_tbl


### -------------------------
### TE linear regression
### -------------------------
te_lin_train_raw <- X.train %>%
  filter(`1st_position` == "TE") %>%
  select(player, overall_nfl_grade, any_of(te_x))

te_lin_test_raw <- X.test %>%
  filter(`1st_position` == "TE") %>%
  select(player, overall_nfl_grade, any_of(te_x))

te_lin_means <- te_lin_train_raw %>% summarise(across(all_of(te_x), ~mean(.x, na.rm = TRUE)))
te_lin_sds   <- te_lin_train_raw %>% summarise(across(all_of(te_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

te_lin_train <- te_lin_train_raw %>%
  mutate(across(all_of(te_x), ~(.x - te_lin_means[[cur_column()]]) / te_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(te_x)) %>%
  as.data.frame()

te_lin_test <- te_lin_test_raw %>%
  mutate(across(all_of(te_x), ~(.x - te_lin_means[[cur_column()]]) / te_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(te_x)) %>%
  as.data.frame()

lm.te <- lm(overall_nfl_grade ~ ., data = te_lin_train %>% select(-player))

te_train_pred <- predict(lm.te, te_lin_train %>% select(-player))
te_test_pred  <- predict(lm.te, te_lin_test  %>% select(-player))

cat("TE Train RMSE:", rmse(te_lin_train$overall_nfl_grade, te_train_pred), "\n")
cat("TE Train R2:  ", rsq(te_lin_train$overall_nfl_grade, te_train_pred), "\n")
cat("TE Test RMSE: ", rmse(te_lin_test$overall_nfl_grade, te_test_pred), "\n")
cat("TE Test R2:   ", rsq(te_lin_test$overall_nfl_grade, te_test_pred), "\n")

te_lin_train_tbl <- te_lin_train %>%
  mutate(pred_grade = te_train_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

te_lin_test_tbl <- te_lin_test %>%
  mutate(pred_grade = te_test_pred) %>%
  select(player, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

te_lin_train_tbl
te_lin_test_tbl


ol_lin_train_raw <- X.train %>%
  filter(`1st_position` %in% c("T","G","C")) %>%
  select(player, `1st_position`, overall_nfl_grade, any_of(ol_x))

ol_lin_test_raw <- X.test %>%
  filter(`1st_position` %in% c("T","G","C")) %>%
  select(player, `1st_position`, overall_nfl_grade, any_of(ol_x))

ol_lin_means <- ol_lin_train_raw %>% summarise(across(all_of(ol_x), ~mean(.x, na.rm = TRUE)))
ol_lin_sds   <- ol_lin_train_raw %>% summarise(across(all_of(ol_x), ~sd(.x, na.rm = TRUE))) %>%
  mutate(across(everything(), ~ifelse(is.na(.x) | .x == 0, 1, .x)))

ol_lin_train <- ol_lin_train_raw %>%
  mutate(across(all_of(ol_x), ~(.x - ol_lin_means[[cur_column()]]) / ol_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(ol_x)) %>%
  as.data.frame()

ol_lin_test <- ol_lin_test_raw %>%
  mutate(across(all_of(ol_x), ~(.x - ol_lin_means[[cur_column()]]) / ol_lin_sds[[cur_column()]])) %>%
  drop_na(overall_nfl_grade, all_of(ol_x)) %>%
  as.data.frame()

lm.ol <- lm(overall_nfl_grade ~ ., data = ol_lin_train %>% select(-player, -`1st_position`))

ol_train_pred <- predict(lm.ol, ol_lin_train %>% select(-player, -`1st_position`))
ol_test_pred  <- predict(lm.ol, ol_lin_test  %>% select(-player, -`1st_position`))

cat("OL Train RMSE:", rmse(ol_lin_train$overall_nfl_grade, ol_train_pred), "\n")
cat("OL Train R2:  ", rsq(ol_lin_train$overall_nfl_grade, ol_train_pred), "\n")
cat("OL Test RMSE: ", rmse(ol_lin_test$overall_nfl_grade, ol_test_pred), "\n")
cat("OL Test R2:   ", rsq(ol_lin_test$overall_nfl_grade, ol_test_pred), "\n")

ol_lin_train_tbl <- ol_lin_train %>%
  mutate(pred_grade = ol_train_pred) %>%
  select(player, `1st_position`, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

ol_lin_test_tbl <- ol_lin_test %>%
  mutate(pred_grade = ol_test_pred) %>%
  select(player, `1st_position`, overall_nfl_grade, pred_grade) %>%
  arrange(desc(pred_grade))

ol_lin_train_tbl
ol_lin_test_tbl


library(pROC)
# QB
roc_qb <- roc(qb_test$draft_grade_over_75, qb_probs)
auc_qb <- auc(roc_qb)
auc_qb

# RB
roc_rb <- roc(rb_test$draft_grade_over_75, rb_probs)
auc_rb <- auc(roc_rb)
auc_rb

# WR
roc_wr <- roc(wr_test$draft_grade_over_75, wr_probs)
auc_wr <- auc(roc_wr)
auc_wr

# TE
roc_te <- roc(te_test$draft_grade_over_75, te_probs)
auc_te <- auc(roc_te)
auc_te

# OL
roc_ol <- roc(ol_test$draft_grade_over_75, ol_probs)
auc_ol <- auc(roc_ol)
auc_ol



#Combined AUROC
all_probs <- c(qb_probs, rb_probs, wr_probs, te_probs, ol_probs)

all_actual <- c(
  qb_test$draft_grade_over_75,
  rb_test$draft_grade_over_75,
  wr_test$draft_grade_over_75,
  te_test$draft_grade_over_75,
  ol_test$draft_grade_over_75
)
roc_all <- roc(all_actual, all_probs)
auc_all <- auc(roc_all)

auc_all
cat("Overall AUROC:", auc_all, "\n")
plot(roc_all, col = "blue", main = "Overall ROC Curve (All Positions)")





### =========================
### 5-FOLD CROSS VALIDATION
### =========================
library(boot)

set.seed(1)

# QB
cv.qb <- cv.glm(
  qb_train %>% select(-player),
  log.reg.qb,
  K = 5
)$delta[1]

# RB
cv.rb <- cv.glm(
  rb_train %>% select(-player),
  log.reg.rb,
  K = 5
)$delta[1]

# WR
cv.wr <- cv.glm(
  wr_train_cc %>% select(-player),
  log.reg.wr,
  K = 5
)$delta[1]

# TE
cv.te <- cv.glm(
  te_train_cc %>% select(-player),
  log.reg.te,
  K = 5
)$delta[1]

# OL
cv.ol <- cv.glm(
  ol_train_cc %>% select(-player, -`1st_position`),
  log.reg.ol,
  K = 5
)$delta[1]

# Print CV errors
cat("QB CV Error:", cv.qb, "\n")
cat("RB CV Error:", cv.rb, "\n")
cat("WR CV Error:", cv.wr, "\n")
cat("TE CV Error:", cv.te, "\n")
cat("OL CV Error:", cv.ol, "\n")


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

qb_extra <- get_metrics(qb_test$draft_grade_over_75, qb_pred, qb_probs, "QB")
rb_extra <- get_metrics(rb_test$draft_grade_over_75, rb_pred, rb_probs, "RB")
wr_extra <- get_metrics(wr_test$draft_grade_over_75, wr_pred, wr_probs, "WR")
te_extra <- get_metrics(te_test$draft_grade_over_75, te_pred, te_probs, "TE")
ol_extra <- get_metrics(ol_test$draft_grade_over_75, ol_pred, ol_probs, "OL")

all_extra_metrics <- bind_rows(qb_extra, rb_extra, wr_extra, te_extra, ol_extra)
all_extra_metrics

# =========================
# OVERALL OFFENSE AUROC + AUPRC
# =========================

install.packages("pROC")
install.packages("PRROC")

library(pROC)
library(PRROC)

# combine all offensive test actuals + predicted probabilities
off_actual <- c(
  qb_test$draft_grade_over_75,
  rb_test$draft_grade_over_75,
  wr_test$draft_grade_over_75,
  te_test$draft_grade_over_75,
  ol_test$draft_grade_over_75
)

off_probs <- c(
  qb_probs,
  rb_probs,
  wr_probs,
  te_probs,
  ol_probs
)

# drop any missing values just in case
off_df <- data.frame(actual = off_actual, prob = off_probs) %>%
  drop_na()

# AUROC
off_roc <- roc(off_df$actual, off_df$prob, quiet = TRUE)
off_auroc <- as.numeric(auc(off_roc))

# AUPRC
off_pr <- pr.curve(
  scores.class0 = off_df$prob[off_df$actual == 1],
  scores.class1 = off_df$prob[off_df$actual == 0],
  curve = TRUE
)
off_auprc <- off_pr$auc.integral

cat("Overall Offense AUROC:", off_auroc, "\n")
cat("Overall Offense AUPRC:", off_auprc, "\n")

# optional small table for slide
offense_summary_metrics <- data.frame(
  Group = "Offense",
  AUROC = round(off_auroc, 3),
  AUPRC = round(off_auprc, 3)
)

offense_summary_metrics



# =========================
# COMBINED OFFENSE CURVES
# =========================

library(pROC)
library(PRROC)
library(dplyr)

# combine all offense predictions
off_actual <- c(
  qb_test$draft_grade_over_75,
  rb_test$draft_grade_over_75,
  wr_test$draft_grade_over_75,
  te_test$draft_grade_over_75,
  ol_test$draft_grade_over_75
)

off_probs <- c(
  qb_probs,
  rb_probs,
  wr_probs,
  te_probs,
  ol_probs
)

off_df <- data.frame(actual = off_actual, prob = off_probs) %>%
  drop_na()

# =========================
# ROC CURVE (AUROC)
# =========================

off_roc <- roc(off_df$actual, off_df$prob, quiet = TRUE)
plot(off_roc, main = "ROC Curve - Overall Offense")

auc(off_roc)  # prints AUROC

# =========================
# PR CURVE (AUPRC)
# =========================

off_pr <- pr.curve(
  scores.class0 = off_df$prob[off_df$actual == 1],
  scores.class1 = off_df$prob[off_df$actual == 0],
  curve = TRUE
)

plot(off_pr, main = "PR Curve - Overall Offense")

off_pr$auc.integral  # prints AUPRC

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
qb_best <- find_best_cutoff(qb_probs, qb_test$draft_grade_over_75, "QB")
rb_best <- find_best_cutoff(rb_probs, rb_test$draft_grade_over_75, "RB")
wr_best <- find_best_cutoff(wr_probs, wr_test$draft_grade_over_75, "WR")
te_best <- find_best_cutoff(te_probs, te_test$draft_grade_over_75, "TE")
ol_best <- find_best_cutoff(ol_probs, ol_test$draft_grade_over_75, "OL")

# Combine results
all_best_cutoffs <- bind_rows(qb_best, rb_best, wr_best, te_best, ol_best)
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

# QB
qb_down <- downsample_fit(qb_train, qb_test, cutoff = 0.2, drop_cols = "player")
qb_down_metrics <- get_simple_metrics(qb_down$actual, qb_down$pred, qb_down$probs, "QB_down")

# RB
rb_down <- downsample_fit(rb_train, rb_test, cutoff = 0.2, drop_cols = "player")
rb_down_metrics <- get_simple_metrics(rb_down$actual, rb_down$pred, rb_down$probs, "RB_down")

# WR
wr_down <- downsample_fit(wr_train_cc, wr_test, cutoff = 0.2, drop_cols = "player")
wr_down_metrics <- get_simple_metrics(wr_down$actual, wr_down$pred, wr_down$probs, "WR_down")

# TE
te_down <- downsample_fit(te_train_cc, te_test, cutoff = 0.2, drop_cols = "player")
te_down_metrics <- get_simple_metrics(te_down$actual, te_down$pred, te_down$probs, "TE_down")

# OL
ol_down <- downsample_fit(ol_train_cc, ol_test, cutoff = 0.2, drop_cols = c("player", "1st_position"))
ol_down_metrics <- get_simple_metrics(ol_down$actual, ol_down$pred, ol_down$probs, "OL_down")

# Combine downsampled results
downsample_results <- bind_rows(
  qb_down_metrics,
  rb_down_metrics,
  wr_down_metrics,
  te_down_metrics,
  ol_down_metrics
)

downsample_results

