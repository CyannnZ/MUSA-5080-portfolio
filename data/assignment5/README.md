# Assignment 5: official NS portfolio version

Copied from the assignment repository's dev commit recorded in source_version.json.
The published source is assignments/assignment 5.qmd. Model formulas, text, folds,
and predictions are preserved; only project-relative paths and website metadata
were adapted. The final model is NS, with m1-m10 and E1-E4 comparisons.

From the portfolio root:

```powershell
$env:R_LIBS_USER = 'C:/Users/Lenovo/AppData/Local/R/win-library/4.6'
quarto render "assignments/assignment 5.qmd"
quarto render assignments/index.qmd
& 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe' data/assignment5/verify_assignment5.R
```

prepare_features.R constructs the features from the supplied train/test CSVs and
cached 2023 Wards, 2023 ACS five-year tracts, and 2023-2025 robbery responses.
The caches allow reproduction without a Census key or another network request.
validation_design.rds retains the original coordinate-grouped folds and spatial
weights. Stored reference predictions are used only to check reproduction after
fitting; they are never predictors. The original R scripts in the source repository
record their construction. source_version.json identifies the exact adopted version.

The assignment template requires each teammate to publish on their own portfolio
and submit that report URL on Canvas. One teammate uploads the team CSV separately.
The date is Friday October 30; the precise deadline time is on Canvas.
The downloadable CSV is assignments/team_Mahjong_predictions.csv (id, pred),
5,789 rows in the supplied test order. Hidden test prices are unavailable.
