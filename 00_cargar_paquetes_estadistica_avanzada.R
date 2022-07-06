
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
  "survival"
)

invisible(
  sapply(
    pkgs,
    function(x) {
      if (!x %in% installed.packages()[, 1])
        install.packages(x, dep = TRUE)
      suppressPackageStartupMessages(require(x, character.only = TRUE))
    }
  )
)

rm(pkgs)
