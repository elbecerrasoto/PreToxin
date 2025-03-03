#!/usr/bin/env Rscript

library(tidyverse)
library(proxy)

IN <- "PTTG_codes.tsv"
METHOD <- "jaccard"

codes <- read_tsv(IN)
codes$tax_id <- as.character(codes$tax_id)
codes$pfams <- map(codes$arch, \(x) str_split_1(x, " "))
# tree <- ncbi_tree(unique(codes$tax_id))

all_pfams <- map(codes$arch,
                 \(x) str_split_1(
                   x, " ")) |>
  unlist() |>
  unique()

list_matrix <- map(codes$pfams,
                   \(x) all_pfams %in% x)

abspres_pfam <- list_matrix |>
  unlist() |>
  matrix(ncol = length(all_pfams), byrow = TRUE)

row.names(abspres_pfam) <- codes$pid
colnames(abspres_pfam) <- all_pfams

# jaccard <- dist(abspres_pfam,method=METHOD)
# saveRDS(jaccard, "jaccard.RDS")

jaccard <- readRDS("jaccard.RDS")

jac_tib <- broom::tidy(jaccard)
# write_tsv(jac_tib, "jaccard.tsv")
