library(tidyverse)
library(ggthemes)
library(magrittr)
library(ggrepel)

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


PTTG <- "data/pttg.tsv"
ALL_BAC <- "data/all_bac.tsv"

# read ----

pttg <- read_tsv(PTTG)
all_bac <- read_tsv(ALL_BAC)

taxid_ranks <- all_bac |>
  distinct(tax_id, .keep_all = TRUE)


pttg_ranks <- pttg |>
  select(-family, -genus) |>
  left_join(taxid_ranks, join_by(tax_id))


count_rank(pttg, "family", "all_bac")

by_group <- split(pttg_ranks, pttg_ranks$group)

group_freqs_fam <- map(by_group, \(x) count_rank(x, "family", unique(x$group))) |>
  bind_rows()

group_freqs_genus <- map(by_group, \(x) count_rank(x, "genus", unique(x$group))) |>
  bind_rows()

group_freqs <- bind_rows(group_freqs_fam, group_freqs_genus)

group_freqs |>
  write_tsv("group_freqs.tsv")

by_group$`45`
length(by_group)

pttg

# wrangle ----

# family

family_all$source <- "all_bac"
family_all$taxon <- "family"

family_pttg <- count_rank(pttg_ranks, family)
family_pttg$source <- "pttg"
family_pttg$taxon <- "family"

family <- rbind(family_pttg, family_all)
family %<>% rename(name = family)

# genus

genus_all <- count_rank(pttg_ranks, genus)
genus_all$source <- "all_bac"
genus_all$taxon <- "genus"

genus_pttg <- count_rank(pttg_ranks, genus)
genus_pttg$source <- "pttg"
genus_pttg$taxon <- "genus"

genus <- rbind(genus_pttg, genus_all)
genus %<>% rename(name = genus)

# both
frecuencies <- rbind(genus, family)

frecuencies |>
  filter(r >= 0.01)


# plot ----

frecuencies |>
  filter(r > 0.01) |>
  mutate(name = reorder(name, r)) |>
  ggplot(aes(label = name, x = name, y = r)) +
  facet_grid(taxon ~ source) +
  geom_point() +
  geom_text_repel() +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    title = "Relative Frecuencies bigger than 1% of PT-TG containing genomes",
    subtitle = "The Relative frecuencies of all NCBI genomes are also shown",
    caption = "author: Becerra-Soto E."
  )

ggsave("frecuencies.svg", width = 8.5, height = 11, units = "in")
