
pkgs <- c(
  "brms",
  "broom",
  "car",
  "compareGroups",
  "data.table",
  "gtsummary",
  "lme4",
  "lubridate",
  "pwr",
  "rcompanion",
  "survival",
  "FSA",
  "simstudy"
)

invisible(
  sapply(
    pkgs,
    function(x) {
      if (!x %in% installed.packages()[, 1])
        install.packages(x, Ncpus = 6, dep = TRUE)
      suppressPackageStartupMessages(require(x, character.only = TRUE))
    }
  )
)

rm(pkgs)
