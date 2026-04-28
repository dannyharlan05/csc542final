# NFL Draft Grade Prediction

Predicts NFL success using Pro Football Focus (PFF) player grades on offensive and defensive NFL draft prospects. The prediction is based on whether a player will earn a PFF grade of 75 or above using college statistics and combine measurements. Position-specific logistic regression models were trained on historical draft data.

---

## Project Structure

```
.
├── data
  ├──last_set.csv     
├── src
  ├── nfl_draft.R
  ├── nfl_draft_defense.R
└── README.md
```

---

## Data

The input file `last_set.csv` contains one row per draft prospect and includes:

- **Player metadata** — name, position (`1st_position`), draft age
- **College stats** — offensive and defensive performance summaries by first year, 2nd year, ..., last year in college (NCAA)
- **Combine measurements** — height, weight, vertical jump, etc.
- **Target variable** — `overall_nfl_grade` (continuous PFF grade)

One derived binary target is created at preprocessing:

```
draft_grade_over_75 = 1 if overall_nfl_grade >= 75, else 0
```

The player `jacoby stevens` is excluded from all models.

---

## Positions Covered

Only offensive skill positions and linemen are modeled:

| Group | Positions |
|-------|-----------|
| QB | QB |
| RB | HB |
| WR | WR |
| TE | TE |
| OL | T, G, C |
| DL | DI, ED |
| LB | LB |
| SEC | S, CB |

---

## Features by Position

### QB
- Draft age
- Completion percentage (`accuracy_percent`)
- Yards per attempt (`ypa`)
- Height (inches, converted from `feet-inches` format)
- Scramble yards

### RB
- Draft age
- Yards after contact
- Yards per route run (`yprr`)
- Elusive rating / missed tackles forced (`elu_rush_mtf`)
- Run block grade
- First downs
- Vertical jump

### WR
- Draft age
- Height (inches)
- Weight
- Yards per route run (`yprr`)
- Contested catch rate
- Yards after catch per reception
- Slot rate

### TE
- Draft age
- Targets
- Receiving yards

### OL
- Draft age
- Overall offense grade
- Pass block grade
- Run block grade
- Pressures allowed

### DL
- Draft Age
- Combine 3 cone
- Combine bench press
- Height
- Weight
- Last Season Game Count
- Last Season Sacks
- Last Season Batted Passes
- Last Season Tackles for Loss
- Last Season Hurries
- Last Season Total Pressures

### LB
- Draft Age
- Combine Bench Press
- Combine Broad Jump
- Combine Shuttle
- Height
- Weight
- Last Season Game Count
- Last Season Snap Count
- Last Season Sacks
- Last Season Tackles for Loss
- Last Season Hits

### SEC
- Draft Age
- Combine Vertical
- Combine 40-yard dash
- Height
- Last Season Game Count
- Last Season Receptions
- Last Season Touchdowns
- Last Season Pass Break Ups
- Last Season Catch Rate
- Last Season Missed Tackle Rate
- Last Season Targets
- Last Season Forced Incompletion Rate
---

## Methodology

### Train / Test Split
An 80/20 random split is applied to both the offensive and defensive subsets.

### Feature Scaling
All numeric features are standardized using **training set** means and standard deviations only. The same parameters are applied to the test set.

### Models

A logistic regression model was fit per position:

**Logistic Regression (binary classification)**  
Predicts `draft_grade_over_75` (PFF ≥ 75 vs. < 75).  
Default classification threshold: **0.20** (tuned downward to improve recall on the minority class).

**Experimental Linear Regression also fit**  
Predicts the continuous `overall_nfl_grade` directly.

---

## Evaluation Metrics

### Classification (Logistic Regression)
- Accuracy (train and test confusion matrices)
- **AUROC** — area under the ROC curve (per position and combined offense)
- **AUPRC** — area under the precision-recall curve (better suited for class imbalance)
- Precision, Recall, F1 Score
- Optimal F1 threshold search (0.10 – 0.90 grid)
- 5-fold cross-validation error

### Regression (Linear Regression)
- RMSE (train and test)
- R² (train and test)

---

## Handling Class Imbalance

Because PFF ≥ 75 prospects are a minority class, three strategies are applied:

1. **Low classification threshold (0.20)** — increases recall at some cost to precision
2. **Optimal threshold search** — finds the threshold maximizing F1 on test data
3. **Downsampling experiment** — majority class (PFF < 75) is randomly downsampled to match the minority class size; results are reported separately and do not replace the primary models

---

## Key Dependencies

```r
library(tidyverse)   # data wrangling
library(boot)        # cross-validation (cv.glm)
library(pROC)        # AUROC (roc, auc)
library(PRROC)       # AUPRC (pr.curve)
library(broom)       # tidy coefficient tables
```

Install any missing packages with:

```r
install.packages(c("tidyverse", "boot", "pROC", "PRROC", "broom"))
```

---

## Running the Script

1. Place `last_set.csv` in your working directory.
2. Open `nfl_draft.R` and `nfl_draft_defense.R` in RStudio or run from the terminal:

The script runs sequentially — preprocessing → position models → evaluation → diagnostics — and prints all results to the console. ROC and PR curve plots are rendered inline.

---

## Output Summary

| Output | Description |
|--------|-------------|
| `stats_by_grade` | Descriptive stats for PFF ≥ 75 vs. < 75 groups |
| `final_table` | Player counts by position and grade group |
| `*_probs_tbl` | Per-player predicted probabilities (train and test) |
| `*_lin_*_tbl` | Per-player predicted PFF grades (linear regression) |
| `all_extra_metrics` | Precision / Recall / F1 / AUPRC by position |
| `all_best_cutoffs` | Optimal F1 threshold per position |
| `downsample_results` | Metrics from downsampled model variants |
| `_summary_metrics` | Combined offensive/defensive AUROC and AUPRC |
