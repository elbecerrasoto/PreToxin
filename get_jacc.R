#!/usr/bin/env Rscript

library(tidyverse)
library(proxy)
library(glue)
# library(broom)

IN <- "PTTG_codes.tsv"

PTTG <- "PF14449"
METHOD <- "jaccard"

# Available distances proxy::dist
# > summary(pr_DB)
# * Similarity measures:
#   angular, Braun-Blanquet, Chi-squared, correlation, cosine, Cramer,
# Dice, eDice, eJaccard, Fager, Faith, Gower, Hamman, Jaccard,
# Kulczynski1, Kulczynski2, Michael, Mountford, Mozley, Ochiai, Pearson,
# Phi, Phi-squared, Russel, simple matching, Simpson, Stiles, Tanimoto,
# Tschuprow, Yule, Yule2
#
# * Distance measures:
#   Bhjattacharyya, Bray, Canberra, Chord, divergence, Euclidean,
# fJaccard, Geodesic, Hellinger, Kullback, Levenshtein, Mahalanobis,
# Manhattan, Minkowski, Podani, Soergel, supremum, Wave, Whittaker

# Distance measures can be used with simil, and similarity measures with dist. In these cases, the result is transformed accordingly using the specified coercion functions.
# pr_simil2dist(x) = 1 - abs(x)
# pr_dist2simil(x) = 1 / (1 + x)


# Read and Wrangle --------------------------------------------------------


codes <- read_tsv(IN)
codes$tax_id <- as.character(codes$tax_id)
codes$pfams <- map(codes$arch, \(x) str_split_1(x, " "))


contains_PTTG <- map_lgl(codes$pfams, \(x) PTTG %in% x)

if (!all(contains_PTTG)) {
  cat("Please remove the following protein:")
  cat(codes$tax_id[!contains_PTTG])
  stopifnot("Protein withtout PTTG:" = all(contains_PTTG))
}

# Calculate distance ------------------------------------------------------


all_pfams <- map(
  codes$arch,
  \(x) str_split_1(
    x, " "
  )
) |>
  unlist() |>
  unique()

list_matrix <- map(
  codes$pfams,
  \(x) all_pfams %in% x
)

abspres_pfam <- list_matrix |>
  unlist() |>
  matrix(ncol = length(all_pfams), byrow = TRUE)

row.names(abspres_pfam) <- codes$pid
colnames(abspres_pfam) <- all_pfams


distance_rds <- paste0(METHOD, ".RDS")
if (file.exists(distance_rds)) {
  jaccard <- readRDS(distance_rds)
} else {
  jaccard <- proxy::dist(abspres_pfam, method = METHOD)
  saveRDS(jaccard, distance_rds)
}

# jac_tib <- broom::tidy(jaccard)


# Percentages ---------------------------------------------------------------------
