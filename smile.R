#!/usr/bin/env Rscript

library(tidyverse)

IN <- "data/pttg.tsv"

PTTGid <- "PF14449"
PTTGcode <- "㢒"

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

codes <- read_tsv(IN, col_types = CODES_COLS)
groups_regs <- read_tsv("data/code_re.tsv")

Ucodes <- codes |>
  distinct(codes$arch, .keep_all = TRUE)
