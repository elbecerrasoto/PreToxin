#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
})

PTTG <- "data/pttg.tsv"
REGS <- "data/regs.tsv"

PTTG_id <- "PF14449"
PTTG_code <- "㢒"

PTTG_COLS <- cols(
  pid = "c",
  code = "c",
  family = "f",
  genus = "f",
  tax_id = "c",
  ndoms = "i",
  length = "i",
  arch = "c"
)

REGS_COLS <- cols(
  group = "f",
  reg = "c"
)


# Read data ----

pttg <- read_tsv(PTTG, col_types = PTTG_COLS)
regs <- read_tsv(REGS, col_types = REGS_COLS)

# Check all PT-TG ----

pttg$pfams <- map(pttg$arch, \(x) str_split_1(x, ";"))
contains_PTTG <- map_lgl(pttg$pfams, \(x) PTTG_id %in% x)

if (!all(contains_PTTG)) {
cat("Please remove the following proteins:")
cat(pttg$tax_id[!contains_PTTG])
stopifnot("Protein withtout PTTG:" = all(contains_PTTG))
}

# Add groups ----

groups <- vector(mode = "integer", length = nrow(pttg))
for (idx in seq_along(regs$reg)) {
    reg <- regs$reg[[idx]]
    matches <- which(str_detect(pttg$arch, reg))
    groups[matches] <- idx
}

pttg$group <- as_factor(groups)
pttg <- pttg |>
    relocate(group, .after = pid) |>
    arrange(group)

pttg |>
    write_tsv("Gpttg.tsv")

# Calculate distances ----


