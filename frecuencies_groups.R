#!/usr/bin/env Rscript

library(tidyverse)
library(ggrepel)

count_rank <- function(ranks, rank, group) {
  Nbacs <- nrow(ranks)
  ranks |>
    group_by(.data[[rank]]) |>
    summarize(n = n(), r = n() / Nbacs) |>
    arrange(desc(n)) |>
    rename(name = all_of(rank)) |>
    mutate(
      group = group,
      taxon = rank
    )
}


PTTG <- "PF14449_archs_TaxID_spaces_Groups.tsv"
ALL_BAC <- "data/all_bac.tsv"

# read ----

pttg <- read_tsv(PTTG)
all_bac <- read_tsv(ALL_BAC)

ranks <- all_bac |>
  distinct(tax_id, .keep_all = TRUE)

pttg_ranks <- pttg |>
  left_join(ranks, join_by(tax_id))

pttg_ranks <- pttg_ranks |>
  rename(group = classification)


# All PTTGs ----

by_group <- split(pttg_ranks, pttg_ranks$group)

group_freqs_fam <- map(by_group, \(x) count_rank(x, "family", unique(x$group))) |>
  bind_rows()

group_freqs_genus <- map(by_group, \(x) count_rank(x, "genus", unique(x$group))) |>
  bind_rows()

group_freqs <- bind_rows(group_freqs_fam, group_freqs_genus)

# Different architectures ----

RepeatGroupFreqs <- function(pttg_ranks) {
  by_group <- split(pttg_ranks, pttg_ranks$group)

  group_freqs_fam <- map(by_group, \(x) count_rank(x, "family", unique(x$group))) |>
    bind_rows()

  group_freqs_genus <- map(by_group, \(x) count_rank(x, "genus", unique(x$group))) |>
    bind_rows()

  bind_rows(group_freqs_fam, group_freqs_genus)
}

pttg_runique <- pttg_ranks |>
  distinct(arch, .keep_all = TRUE)


group_freqs_unique <- pttg_runique |>
  RepeatGroupFreqs()


group_freqs_unique$source <- "unique_arch"
group_freqs$source <- "all_pttg"

GroupFreqs <- bind_rows(group_freqs_unique, group_freqs)
view(GroupFreqs)
# write_tsv(GroupFreqs, "group_freqs.tsv")


# trees ----

# Existence by NCBI genome (not Abundance)

# Existence by Protein (not Abundance)

# Existence by Taxonomy

# Existence by Architecture


dim(pttg_ranks)
pttg_ranks |>
  distinct(, .keep_all = TRUE)

view(pttg_ranks)


pttg_ranks





