library(tidyverse)
library(ggrepel)


IN <- "BacillotaRanks.tsv"

granks <- read_tsv(IN)

N <- nrow(granks)

# frequency of genomes per family that have each domain

FAMILY <- granks |>
  group_by(family) |>
  summarise(N = n(), R = n() / N) |>
  arrange(desc(N))


nr <- function(granks_sub) {
  granks_sub |>
    group_by(family) |>
    summarise(
      n := n(),
      r := n() / N
    ) |>
    left_join(FAMILY, join_by(family)) |>
    mutate(total_family = N, r_family = n / N) |>
    select(-N, -R)
}


all_filters <- list(
  granks |> filter(lxg),
  granks |> filter(deam),
  granks |> filter(endo),
  granks |> filter(lxg & deam),
  granks |> filter(lxg & endo),
  granks |> filter(deam & endo),
  granks |> filter(lxg & deam & endo)
)

GRANKS_SUBS <- all_filters |>
  map(nr)

NAMES <- c(
  "lxg", "deam", "endo",
  "lxg_deam", "lxg_endo",
  "deam_endo", "all"
)


names(GRANKS_SUBS) <- NAMES

to_map <- function(idx) {
  oname <- NAMES[idx]
  tib <- GRANKS_SUBS[[oname]]
  tib |>
    mutate(type = oname)
}

OUT <- seq_along(NAMES) |>
  map(to_map) |>
  do.call(rbind, args = _)


OUT <- OUT |>
  relocate(type, family, r_family, n, total_family) |>
  rename(
    n_genomes = n,
    r_all = r,
    genomes_family = total_family
  ) |>
  arrange(type, desc(r_family))

# write_tsv(OUT, "families.tsv")
Opoints <- OUT |>
  group_by(family) |>
  summarise(rf_sum = sum(r_family)) |>
  arrange(rf_sum)



Opoints <- Opoints |>
  mutate(
    family_ord = factor(family,
      levels = family,
      order = TRUE
    )
  )

tib_plot <- OUT |>
  left_join(Opoints, join_by(family))

tib_plot <- tib_plot |>
  mutate(perct = r_family * 100)

library(ggthemes)


COLOR <- c(
  "All", "Deam",
  "Deam & Endo", "Endo",
  "LXG", "LXG & Deam",
  "LXG & Endo"
)

variable_labeller <- function(variable, value) {
  names(COLOR) <- c(
    "all", "deam",
    "deam_endo", "endo",
    "lxg", "lxg_deam",
    "lxg_endo"
  )
  return(COLOR[value])
}



to_label <- c(
  "Bacillaceae", "Planococcaceae",
  "Natranaerofabaceae", "Koleobacteraceae", "Gottschalkiaceae",
  "Fam_of_Bovifimicola", "Fam_Clostridia", "incertae sedis",
  "Acidaminobacteraceae", "Abyssicoccaceae", "Staphylococcaceae",
  "Vallitaleaceae", "Thermoactinomycetaceae"
)


p <- ggplot(tib_plot, aes(
  x = family_ord, y = perct,
  color = type
)) +
  geom_point() +
  geom_text_repel(aes(label = ifelse(family %in% to_label, family, ""), force = 4),
    max.overlaps = 42
  ) +
  facet_grid(
    rows = vars(type),
    labeller = variable_labeller
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )



p <- p +
  labs(
    title = "Relative Frecuencies Per Family",
    subtitle = " Deaminase, LXG & EndoV domains",
    caption = "author: Becerra-Soto E."
  ) + scale_color_hue(labels = COLOR) + guides(color = guide_legend(
    title = ""
  ))


p
ggsave("family_freqs.8.svg", plot = p, height = 30, width = 26, units = "cm")
