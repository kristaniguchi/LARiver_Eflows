# Written beginning September 4, 2020 by Daniel Philippus for the LAR Environmental Flows
# project at Colorado School of Mines.
# 
# This script is intended to predict flow information based on flow rate using a provided
# rating curve Function Table.
# 
# Function Tables have the format:
# Function, Gage, `1`, `2`, sqrt, `3rt`, `4rt`, const, HF.1, HF.2, HF.sqrt, HF.3rt, HF.4rt, HF.const, Threshold
#
# Numbers refer to powers; _rt refers to a root (e.g. 3rt = cube root), and const to a constant addition, all
# as functions of the flow rate.  HF._ means to apply that value to high flows, and Threshold is the flow rate
# at which high flows begin.  All values are in ft and lb units (e.g. cfs, psf, ft).
#
# This script provides functions to calculate:
#   A single value for a single flow at a single location (velocity at 12 cfs at LA1);
#   All values for a single flow at a single location (velocity, depth, etc at 12 cfs at LA1); and
#   All values for a data frame of flows and locations (input: Gage, Flow; output: Gage, Flow, Function, Value)
#
# I do not recommend using these predictions when the channel depth is below about 0.3 ft (~0.5-16 cfs depending
# on location), as the HEC-RAS model predictions have proven to be extremely unreliable in this range during
# calibration, presumably due to minor channel imperfections that do not show up in the model.

library(tidyverse)

MIN.DEPTH <- 0.1
DEPTH.VAR <- "Max.Chl.Dpth..ft."
THRES.PATH <- file.path("C:", "Users", "Daniel", "LocalDocuments", "MinesWork", "Hydraulics", "RatingCurves", "thresholds.csv")

example <- function() {
  ftab <- get.ftab(DEFAULT_PATH)
  data <- data.frame(
    Gage = c("LA1", "LA1", "F319", "F57C", "F57C", "F57C"),
    Flow = 1:6
  )
  predict.table(ftab, data)
}

DEFAULT_PATH <- file.path("C:", "Users", "Daniel", "LocalDocuments", "MinesWork", "Hydraulics", "RatingCurves", "CurveTable_Final.csv")

get.ftab <- function(path) {
  as_tibble(read.csv(path, stringsAsFactors = F))
}

predict.oneval <- function(ftab, gage, func, flow) {
  # Predict one value for one flow at one location
  f.row <- ftab[ftab$Gage == gage & ftab$Function == func,][1,]
  if (flow <= f.row$Threshold || f.row$Threshold == 0) {
    flow * f.row$X1 + flow^2 * f.row$X2 + flow^0.5 * f.row$sqrt + flow^(1/3) * f.row$X3rt + flow^(1/4) * f.row$X4rt + f.row$const
  } else {
    flow * f.row$HF.1 + flow^2 * f.row$HF.2 + flow^0.5 * f.row$HF.sqrt +
      flow^(1/3) * f.row$HF.3rt + flow^(1/4) * f.row$HF.4rt + f.row$HF.const
  }
}

predict.onegage <- function(ftab, gage, flow) {
  # Predict all values for one flow at one location
  f.rows <- ftab[ftab$Gage == gage,]
  fns <- unique(f.rows$Function)
  vals <- NULL
  for (fn in fns) {
    vals <- c(vals, predict.oneval(f.rows, gage, fn, flow))
  }
  out <- rbind(tibble(), vals)
  names(out) <- fns
  as_tibble(out)
}

predict.table <- function(ftab, dtab) {
  # Predict all values for the given flows in the table
  # dtab: a data frame of Gage, Flow
  out <- as_tibble(dtab)
  fns <- unique(ftab$Function)
  for (fn in fns) out[[fn]] <- NA
  for (ri in 1:nrow(dtab)) {
    gage <- dtab$Gage[ri]
    flow <- dtab$Flow[ri]
    vals <- predict.onegage(ftab, gage, flow)
    for (fn in fns) {
      pred <- vals[[fn]]
      if (!is.null(pred)) out[[fn]][ri] <- pred
    }
  }
  out
}

thresholds <- function(in.tab, thres.tab, min.depth = MIN.DEPTH, depth.var = DEPTH.VAR, strict = F) {
  # Post-process a prediction table to flag unreliable predictions
  # Unreliable predictions are at too shallow a depth or have a flow rate exceeding
  # channel depth/modeled range
  # Sets $Flag <- TRUE if predictions are likely bad
  # 
  # in.tab: Gage, Flow, and prediction variables (must include depth.var); other columns will be preserved
  # thres.tab: Node, Threshold.cfs
  # if strict, omit bad rows entirely
  flag.depth <- in.tab[[depth.var]] < min.depth
  flag.flow <- map2_lgl(in.tab$Gage, in.tab$Flow, function(g, f) { # Flow exceeds maximum flow
    max.flow <- thres.tab$Threshold.cfs[thres.tab$Node == g][1]
    f > max.flow
  })
  flag.neg.lob <- NULL
  flag.neg.rob <- NULL
  flag.nec.mc <- NULL
  for (rn in 1:nrow(in.tab)) { # Identify areas where LOB/ROB values are negative
    flag.neg.lob <- c(flag.neg.lob,
                  0 < (in.tab[rn,] %>% select(ends_with("LOB")) %>% (function(x) x < 0) %>% sum)
                  )
    flag.neg.rob <- c(flag.neg.rob,
                      0 < (in.tab[rn,] %>% select(ends_with("ROB")) %>% (function(x) x < 0) %>% sum)
    )
    flag.neg.mc <- c(flag.neg.mc,
                      0 < (in.tab[rn,] %>% select(ends_with("MC")) %>% (function(x) x < 0) %>% sum)
    )
  }
  flag <- flag.depth | flag.flow
  if (!strict) {
    in.tab$Flag <- flag
  } else {
    in.tab <- in.tab[!flag, ]
  }
  in.tab$Flag.LOB <- flag.neg.lob
  in.tab$Flag.ROB <- flag.neg.rob
  in.tab$Flag.MC <- flag.neg.mc
  
  in.tab
}








