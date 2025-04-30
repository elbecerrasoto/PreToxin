#!/usr/bin/Rscript

suppressMessages({
  library(tidyverse)
})

ARGS <- commandArgs(trailingOnly = TRUE)
RENAMES <- ARGS[[1]]

gueno <- read_tsv(RENAMES)
regs <- read_tsv("data/regs.tsv", col_types = cols(group = "i"))

gueno <- gueno |>
  mutate(new = as.integer(str_remove(new, "G")),
         old = as.integer(str_remove(old, "G")))


regs |>
  rename(old = group) |>
  right_join(gueno, join_by(old)) |>
  arrange(new) |>
  rename(group = new) |>
  select(-old) |>
  relocate(group) |>
  distinct(reg, .keep_all = TRUE) |>
  write_tsv("data/regs.tsv")
