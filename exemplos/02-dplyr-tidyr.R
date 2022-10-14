
# Municipio    | 0-1         | 2-10        | ..... (Idade)
# São Paulo    | 1M          | 2M          | ..... (Habitantes)
#
# Municipio    | Idade       | Hab         |
# São Paulo    | 0-1         | 1M          |
# São Paulo    | 2-10        | 2M          |
# .
# .
# .
# .
#
# Hab ~ Idade + Municipio

#                                                                 com [values_fn]
#                                                                 _______________
# Municipio    | 0-1         | 2-10        | ..... (Idade)      | % Alfabet.
# São Paulo    | 1M          | 2M          | ..... (Habitantes) | 15% (média)

# Municipio    | 0-1         | 2-10        | ..... (Idade)      | Hab
# São Paulo    | 0%          | 20%         | ..... (% Alfabet.) | 3M (soma)

# -------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(dados)

# Motivação: substituir todos os NAs das variáveis
# categóricas por "sem informação" ***
# tidyr::replace_na
dados_starwars %>%
  mutate(across(
    .cols = where(is.character),
    .fns = replace_na, replace = "sem informação"
  ))

# -----
# Motivação: ver quais colunas possuem NAs e quais colunas possuem mais NAs.

dados_starwars %>%
  summarise(across(.fns = ~sum(is.na(.x)))) %>%
  glimpse()

# Deixando no formato longo
dados_starwars %>%
  summarise(across(.fns = ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "num_na") %>%
  arrange(desc(num_na))

# -------------------------------------------------------------------------
# Motivação: ver o número de categorias
# distintas em cada variável categórica ***

dados_starwars %>%
  summarise(across(where(is.character), n_distinct)) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "num_cat") %>%
  arrange(desc(num_cat))

# Remover nome (é a chave da base)
dados_starwars %>%
  summarise(across(
    .cols = c(where(is.character), -nome),
    .fns = n_distinct
  )) %>%
  pivot_longer(everything(), names_to = "coluna", values_to = "num_cat") %>%
  arrange(desc(num_cat))

# Inteiros e doubles
is.integer(c(1, 2, 3, 4))
is.integer(c(1L, 2L, 3L, 4L))
is.double(c(1.2, 0.99))
is.double(c(1L, 2L, 3L, 4L))
is.numeric(c(1.2, 0.99))
is.numeric(c(1L, 2L, 3L, 4L))


# -------------------------------------------------------------------------

# Motivação: Descobrir o ator com o maior lucro médio na base IMDB,
# considerando as 3 colunas de elenco.

imdb <- readRDS("materiais/data/imdb.rds")

imdb %>%
  mutate(lucro = receita - orcamento) %>%
  select(titulo, lucro, starts_with("ator")) %>%
  pivot_longer(starts_with("ator"), names_to = "protagonismo", values_to = "ator") %>%
  group_by(ator) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  arrange(desc(lucro_medio))

# Número de filmes
imdb %>%
  mutate(lucro = receita - orcamento) %>%
  select(titulo, lucro, starts_with("ator")) %>%
  pivot_longer(starts_with("ator"), names_to = "protagonismo", values_to = "ator") %>%
  group_by(ator) %>%
  summarise(
    lucro_medio = mean(lucro, na.rm = TRUE),
    n_filmes = n()
  ) %>%
  filter(n_filmes > 5) %>%
  arrange(desc(lucro_medio))

# -------------------------------------------------------------------------

# Motivação: fazer uma tabela do lucro médio anual dos filmes
# de comédia, ação e romance (2000 a 2016) ***

imdb %>%
  mutate(lucro = receita - orcamento) %>%
  select(titulo, ano, generos, lucro) %>%
  separate_rows(generos, sep = "\\|") %>%
  filter(generos %in% c("Commedy", "Action", "Romance")) %>%
  filter(between(ano, 2000, 2016)) %>%
  group_by(ano, generos) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>%
  arrange(desc(lucro_medio))
