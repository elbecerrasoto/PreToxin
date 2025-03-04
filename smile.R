#!/usr/bin/env Rscript

library(tidyverse)

IN <- "codes.tsv"

PTTG <- "PF14449"
PTTGcode <- "㢒"

CODES_COLS <- cols(pid = "c",
                   class = "f",
                   code = "c",
                   family = "f",
                   genus = "f",
                   tax_id = "c",
                   ndoms = "i",
                   length = "i",
                   arch = "c")


codes <- read_tsv(IN, col_types = CODES_COLS)

codes$pfams <- map(codes$arch, \(x) str_split_1(x, " "))

codes$arch <- map_chr(codes$pfams, \(x) str_flatten(x, collapse = ";"))

codes |>
  select(pid, class, code, family, genus, tax_id, ndoms, length, arch) |>
  write_tsv("codes.tsv")

Ucodes <- codes |>
  distinct(codes$arch, .keep_all = TRUE)


