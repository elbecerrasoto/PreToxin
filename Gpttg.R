#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(pheatmap)
  library(proxy)
  library(broom)
  library(ggthemes)
  library(magrittr)
  library(ggrepel)
})

PTTG <- "data/pttg.tsv"
REGS <- "data/regs.tsv"
ALL_BAC <- "data/all_bac.tsv"

PTTG_id <- "PF14449"
PTTG_code <- "㢒"

METHOD <- "jaccard"
# Available distances proxy::dist

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

ALL_BAC_COLS <- cols(
  genome = "c",
  tax_id = "c",
  phylum = "f",
  class = "f",
  order = "f",
  family = "f",
  genus = "f",
  species = "f"
)

GPTTG_COLS <- cols(
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

RESULTS <- "results"

if (!file.exists(RESULTS)) {
  dir.create(RESULTS)
}

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
  write_tsv(glue("{RESULTS}/Gpttg.tsv"))

# Calculate distances ----

# Remove duplicated architectures
pttg <- pttg |>
  distinct(arch, .keep_all = TRUE)

# Add architecture as a list
pttg$pfams <- map(pttg$arch, \(x) str_split_1(x, ";"))

all_pfams <- map(
  pttg$arch,
  \(x) str_split_1(
    x, ";"
  )
) |>
  unlist() |>
  unique()

list_matrix <- map(
  pttg$pfams,
  \(x) all_pfams %in% x
)

abspres_pfam <- list_matrix |>
  unlist() |>
  matrix(ncol = length(all_pfams), byrow = TRUE)

row.names(abspres_pfam) <- pttg$pid
colnames(abspres_pfam) <- all_pfams

# Jaccard distance matrix
jaccard <- proxy::dist(abspres_pfam, method = METHOD)
# Jaccard distance tibble
jac_tib <- broom::tidy(jaccard)

# Save distance matrix
distance_rds <- glue("{RESULTS}/{METHOD}.RDS")
saveRDS(jaccard, distance_rds)

# Save distance tibble
distance_tsv <- glue("{RESULTS}/jaccard.tsv")
write_tsv(jac_tib, distance_tsv)

# Plot heatmap
distance_pdf <- glue("{RESULTS}/{METHOD}.pdf")
pheatmap(jaccard,
  cluster_rows = TRUE, cluster_cols = TRUE, main = "PT-TG Protein Containing Distances",
  show_rownames = FALSE, show_colnames = FALSE, filename = distance_pdf
)

# Calculate Group Frecuencies ----


get_var_name <- function(var) {
  deparse(substitute(var))
}

count_rank <- function(ranks, rank, source) {
  Nbacs <- nrow(ranks)
  ranks |>
    group_by(.data[[rank]]) |>
    summarize(n = n(), r = n() / Nbacs) |>
    arrange(desc(n)) |>
    rename(name = all_of(rank)) |>
    mutate(
      source = source,
      taxon = rank
    )
}


# read ----

all_bac <- read_tsv(ALL_BAC, col_types = ALL_BAC_COLS)
pttg <- read_tsv(glue("{RESULTS}/Gpttg.tsv"), col_types = GPTTG_COLS)

taxid_ranks <- all_bac |>
  distinct(tax_id, .keep_all = TRUE)

pttg_ranks <- pttg |>
  select(-family, -genus) |>
  left_join(taxid_ranks, join_by(tax_id))

# generate frecuencies ----

by_group <- split(pttg_ranks, pttg_ranks$group)

group_freqs_fam <- map(by_group, \(x) count_rank(x, "family", unique(x$group))) |>
  bind_rows()

group_freqs_genus <- map(by_group, \(x) count_rank(x, "genus", unique(x$group))) |>
  bind_rows()

group_freqs <- bind_rows(group_freqs_fam, group_freqs_genus)

group_freqs |>
  write_tsv(glue("{RESULTS}/group_freqs.tsv"))
