library(rvest)
library(tidyverse) 


library(rvest)
library(purrr)
library(ggstream)
library(ggh4x)
url <- "https://www.althingi.is/altext/154/s/1761.html"


d <- read_html(url)

tabs <- d |> 
  html_table()


fix_cols <- function(x, cols = list(1:2, 4:5, 7:8)) {
  names(x) <- x[2, ]
  x <- x[-(1:2), ]
  
  out <- list()
  
  for (i in seq_along(cols)) {
    out[[i]] <- x[, cols[[i]]] |> 
      mutate(ar = i)
  }
  
  bind_rows(
    out
  )
}



d <- bind_rows(
  tabs[[3]] |> 
    fix_cols() |> 
    janitor::clean_names() |> 
    mutate(
      ar = ar + 2012
    ),
  tabs[[4]] |> 
    fix_cols() |> 
    janitor::clean_names() |> 
    mutate(
      ar = ar + 2015
    ),
  tabs[[5]] |> 
    fix_cols() |> 
    janitor::clean_names() |> 
    mutate(
      ar = ar + 2018
    ),
  tabs[[6]] |> 
    fix_cols(cols = list(1:2, 4:5)) |> 
    janitor::clean_names() |> 
    mutate(
      ar = 2021 + ar
    )
)


d |> 
  write_csv("data/gaesluvardhald_by_rikisfang.csv")
