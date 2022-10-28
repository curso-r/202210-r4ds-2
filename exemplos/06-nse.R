
dplyr
library(dplyr)

(1 + 2) == (3)

filter <- function(tabela, expressoes) {
  homeworld <- starwars$homeworld
  species <- starwars$species
  .................

  expressoes
}

starwars <- dplyr::starwars

starwars %>%
  group_by(gender) %>%
  summarise(
    media = mean(height, na.rm = TRUE),
    maximo = max(height, na.rm = TRUE)
  )

summarise_by <- function(df, agrupar, ...) {
  df %>%
    group_by( {{agrupar}} ) %>%
    summarise(...)
}

starwars %>%
  summarise_by(
    agrupar = gender,
    media = mean(height, na.rm = TRUE),
    maximo = max(height, na.rm = TRUE)
  )

# Um '...' por função
across(
  .cols = c(col1, col2, col3),
  .fns = funcao,
  ...
)

# |     x
# | x
# |_______
#
# > Escolha a coluna 1
# > homeworld
# > species
#
# > Escolha a coluna 2
# > homeworld
# > species

col1 <- "homeworld"
col2 <- "species"

starwars %>%
  mutate(
    col2 = paste(col1, "!")
  )

starwars %>%
  summarise(media = mean(height, na.rm = TRUE))


summarise_mean <- function(df, nome, col) {
  df %>%
    summarise({{nome}} := mean(.data[[col]], na.rm = TRUE))
}

starwars %>%
  summarise_mean("media", "height")

# Prática ----------------------------------------------------

library(dplyr)
library(tidyr)

imdb <- readr::read_rds("materiais/data/imdb.rds")

imdb %>%
  drop_na(receita)

imdb %>%
  filter(!is.na(receita))

filtrar_na <- function(tabela, coluna) {
  tabela %>%
    filter(!is.na(coluna))
}

filtrar_na(tabela = imdb, coluna = receita)

filtrar_na <- function(tabela, coluna) {
  tabela %>%
    filter(!is.na({{coluna}}))
}

imdb %>%
  filtrar_na(receita)

imdb %>%
  filtrar_na("receita")

is.na("receita")

imdb[["receita"]]

filtrar_na_coluna_com_aspas <- function(tabela, coluna) {
  tabela %>%
    filter(!is.na(.data[[coluna]]))
}

imdb %>%
  filtrar_na_coluna_com_aspas("receita")

imdb %>%
  filtrar_na_coluna_com_aspas(receita)

# Outros assuntos ----------------------------------------

# Tratar erros

if (file.exists("data/casas.rds")) {
  asdfasdf
} else {
  alsdfasdf
}

tryCatch(
  readr::read_rds("data/casas.rds"),
  error = print("ERRO")
)

safe_read_rds <- purrr::possibly(readr::read_rds, "ERRO")
safe_read_rds("data/casas.rds")
safe_read_rds("data/casas2.rds")

# Novo pipe

library(dplyr)
# %>%

# Tools > Global options... > Code > Use native pipe

starwars |>
  filter(homeworld == "Naboo", species == "Human") |>
  summarise(media = mean(height, na.rm = TRUE))

lm(mpg ~ cyl, mtcars)

mtcars %>%
  lm(mpg ~ cyl, .)

. <- 20
.
mtcars %>%
  lm(mpg ~ ., .)

1 %>%
  list(., .)

_ <- 20
_

mtcars |>
  lm(mpg ~ cyl, data = _)

1 |>
  list(... = _)


f <- function(n, x, y) {
  n + x + y
}

.x <- 20
~ f(3, .x, .y)

map(asdfasdf, ~ 3 + .x + .y)
map(asdfasdf, \(n, x, y) n + x + y)

1 |>
  {\(x) list(x, x)}()
