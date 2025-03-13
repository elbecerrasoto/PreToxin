#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  # library(ape)
  # library(taxize)
})

Restart <- function() {
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}

GPTTG <- "results/Gpttg.tsv"

GPTTG_COLS <- cols(
  pid = "c",
  group = "i",
  code = "c",
  family = "c",
  genus = "c",
  tax_id = "i",
  ndoms = "i",
  length = "i",
  arch = "c"
)

gpttg <- read_tsv(GPTTG, col_types = GPTTG_COLS) |>
  filter(!is.na(tax_id), !is.na(group))

byG <- gpttg |>
  group_by(group) |>
  summarize(n = n()) |>
  arrange(desc(n))

j <- 6
k <- length(byG$group)

bulkG <- byG$group[1:j]
tailG <- byG$group[(j + 1):k]

bulk_taxIDs <- gpttg |>
  filter(group %in% bulkG) |>
  pull(tax_id) |>
  unique()

tail_taxIDs <- gpttg |>
  filter(group %in% tailG) |>
  pull(tax_id) |>
  unique()

bulk_class <- taxize::classification(bulk_taxIDs, db = "ncbi")
bulk_phylo <- taxize::class2tree(bulk_class)$phylo
ape::write.tree(bulk_phylo, file = "results/bulk.newick")

tail_class <- taxize::classification(tail_taxIDs, db = "ncbi")
tail_phylo <- taxize::class2tree(tail_class)$phylo
ape::write.tree(tail_phylo, file = "results/tail.newick")
