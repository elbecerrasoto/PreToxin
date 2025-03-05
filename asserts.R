#!/usr/bin/env Rscript

library(tidyverse)

IN <- "data/pttg.tsv"
REGS <- "data/regs.tsv"

PTTG_id <- "PF14449"
PTTG_code <- "㢒"

CODES_COLS <- cols(
  pid = "c",
  group = "f",
  code = "c",
  family = "f",
  genus = "f",
  tax_id = "c",
  ndoms = "i",
  length = "i",
  arch = "c"
)

# Read data ----

pttg <- read_tsv(IN, col_types = CODES_COLS)
regs <- read_tsv(REGS)

# Check all PT-TG ----

pttg$pfams <- map(pttg$arch, \(x) str_split_1(x, ";"))
contains_PTTG <- map_lgl(pttg$pfams, \(x) PTTG_id %in% x)

if (!all(contains_PTTG)) {
  cat("Please remove the following proteins:")
  cat(pttg$tax_id[!contains_PTTG])
  stopifnot("Protein withtout PTTG:" = all(contains_PTTG))
}

# Check group (ground truth) assignation ----

checkg <- vector(mode = "integer", length = length(pttg$group))
for (idx in seq_along(regs$reg)) {
  reg <- regs$reg[[idx]]
  matches <- which(str_detect(pttg$arch, reg))
  checkg[matches] <- idx
}

stopifnot("Assigned group differs from regular expression" = all(checkg == pttg$group))
