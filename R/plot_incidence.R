
library(tidyverse)
library(metill)
library(ggh4x)
theme_set(theme_metill())

d <- read_csv("data/gaesluvardhald_by_rikisfang.csv")
d_pop <- read_csv("data/pop_data.csv")

d |> 
  inner_join(
    d_pop
  ) |> 
  mutate(
    rate = fjoldi / n
  ) |> 
  filter(
    ar == 2023
  ) |> 
  filter(
    n > 250
  ) |> 
  mutate(
    rikisfang = fct_reorder(rikisfang, rate),
    se = rate * (1 - rate) / sqrt(n),
    lower = rate - 2 * se,
    upper = rate + 2 * se
  ) |> 
  ggplot(aes(rate, rikisfang)) +
  geom_point(
    size = 2
  ) +
  geom_segment(
    aes(x = lower, xend = upper, yend = rikisfang)
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    guide = guide_axis_truncated(),
    labels = label_number(scale = 1e3)
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Hvaðan komu flestir einstaklingar sem sóttu gæsluvarðhald árið 2023?",
    subtitle = str_c(
      "Sýnt sem fjöldi einstaklinga í gæsluvarðhaldi á 1.000 einstaklinga með viðeigandi ríkisfang",
      " | ",
      "Línur sýna skekkjumörk"
    )
  )
