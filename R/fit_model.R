library(tidyverse)
library(cmdstanr)
library(posterior)
library(scales)
library(ggh4x)
library(metill)
library(glue)
library(bayesplot)
library(patchwork)
theme_set(theme_metill())

d <- read_csv("data/gaesluvardhald_by_rikisfang.csv")
d_pop <- read_csv("data/pop_data.csv")

d <- d |> 
  right_join(
    d_pop
  ) |> 
  mutate(
    fjoldi = coalesce(fjoldi, 0)
  ) |> 
  filter(
    any(fjoldi > 0),
    any(n > 50),
    .by = rikisfang
  ) |> 
  filter(
    ar >= 2016,
    n > 0
  )

d |> 
  filter(is.na(fjoldi))

country <- d$rikisfang |> 
  as_factor() |> 
  as.numeric()
N_countries <- length(unique(country))
N_obs <- nrow(d)

pop <- d$n
cases <- d$fjoldi

stan_data <- list(
  N_countries = N_countries,
  N_obs = N_obs,
  country = country,
  pop = pop,
  cases = cases
)

model <- cmdstan_model("Stan/model.stan")

results <- model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4, 
  iter_sampling = 4000
)



summ <- results |> 
  summarise_draws() |> 
  filter(
    str_detect(variable, "^theta")
  )

results$summary(c("phi", "mu_theta", "sigma_theta"))


p1 <- summ |> 
  mutate(
    country = parse_number(variable)
  ) |> 
  inner_join(
    d |> 
      mutate(
        country = as.numeric(as_factor(rikisfang))
      ) |> 
      filter(ar == 2023, .by = rikisfang)
  ) |> 
  mutate(
    rikisfang = glue("{rikisfang} ({n})") |> 
      fct_reorder(mean)
  ) |> 
  filter(q95 < 0.5) |> 
  ggplot(aes(mean, rikisfang)) +
  geom_point() +
  geom_segment(
    aes(x = q5, xend = q95, yend = rikisfang)
  ) +
  scale_x_continuous(
    labels = label_number(scale = 1e3, big.mark = ".", decimal.mark = ","),
    breaks = breaks_log(8),
    trans = "log10",
    guide = guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Árlegur fjöldi einstaklinga í gæsluvarðhaldi á hverja 1.000 íbúa",
    y = "Ríkisfang (Íbúafjöldi 2023)",
    subtitle = "Lograkvarði (1 og 100 eru jafnlangt frá 10)"
  )




p2 <- summ |> 
  mutate(
    country = parse_number(variable)
  ) |> 
  inner_join(
    d |> 
      mutate(
        country = as.numeric(as_factor(rikisfang))
      ) |> 
      filter(ar == 2023, .by = rikisfang)
  ) |> 
  mutate(
    rikisfang = glue("{rikisfang} ({n})") |> 
      fct_reorder(mean)
  ) |> 
  filter(q95 < 0.5) |> 
  ggplot(aes(mean, rikisfang)) +
  geom_point() +
  geom_segment(
    aes(x = q5, xend = q95, yend = rikisfang)
  ) +
  scale_x_continuous(
    labels = label_number(scale = 1e3, big.mark = ".", decimal.mark = ","),
    breaks = breaks_extended(8),
    # trans = "log10",
    guide = guide_axis_truncated()
  ) +
  scale_y_discrete(
    guide = guide_axis_truncated()
  ) +
  labs(
    x = "Árlegur fjöldi einstaklinga í gæsluvarðhaldi á hverja 1.000 íbúa",
    y = "Ríkisfang (Íbúafjöldi 2023)",
    subtitle = "Línulegur kvarði (1 og 3 eru jafnlangt frá 2)"
  )

p <- p1 + p2 +
  plot_layout(
    axes = "collect"
    ) +
  plot_annotation(
    title = "Tíðni gæsluvarðhalds eftir ríkisfangi (2018 - 2023)",
    subtitle = str_c(
      "Sýnt sem fjöldi einstaklinga í gæsluvarðhaldi á 1.000 einstaklinga með viðeigandi ríkisfang",
      " | ",
      "Línur sýna skekkjumörk"
    )
  )

p


ggsave(
  plot = p,
  file = "Figures/gaesluvardhald_tidni.png",
  width = 8, height = 0.64 * 8, scale = 1.5
)




