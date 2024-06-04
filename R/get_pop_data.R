library(tidyverse)
library(metill)
library(hagstofa)
library(gganimate)
library(ggh4x)
theme_set(theme_metill())

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/3_bakgrunnur/Rikisfang/MAN04103.px"

d <- hg_data(url) |>
  filter(
    Aldur != "Alls",
    parse_number(Aldur) >= 18,
    parse_number(Aldur) < 60,
    Kyn == "Alls"
  ) |>
  collect() |>
  janitor::clean_names() |>
  janitor::remove_constant() |>
  rename(n = 4)

d <- d |> 
  summarise(
    n = sum(n),
    .by = c(rikisfang, ar)
  ) |> 
  filter(
    rikisfang != "Alls"
  ) |> 
  mutate(
    ar = parse_number(ar),
    rikisfang = case_when(
      str_detect(rikisfang, "Lettland") ~ "Lettland",
      str_detect(rikisfang, "Litáen") ~ "Litáen",
      str_detect(rikisfang, "Hvíta") ~ "Hvíta-Rússland",
      TRUE ~ rikisfang
    )
  ) 


d |> 
  write_csv("data/pop_data.csv")





