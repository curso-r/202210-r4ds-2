# Criando projeto

install.packages("usethis")
usethis::use_blank_slate()
usethis::create_project("~/Downloads/exemplo")

# Fluxo de limpeza

data-raw/base.xlsx

data-raw/leitura.R
data-raw/limpeza.R
data-raw/arrumação.R
data-raw/salvar.R
  * saveRDS(base_perfeita, "data/base.rds")

R/modelo.R
  * readRDS("data/base.rds")
R/gráfico.R
  * readRDS("data/base.rds")

# Organização dos arquivos

R/
- sant_...
- itau_...

R/01_itau_leitura.R
  * ler_base()
  * ajustar_nomes()

R/02_itau_limpeza.R
  * remover_colunas()
  * ...

R/99_itau_executar.R
* source("R/01_itau_leitura.R")
* source("R/02_itau_limpeza.R")
* ler_base()
* ajustar_nomes()

# Criando uma função

minha_f <- function(df) {
  df %>%
    mutate() %>%
    filter() %>%
    arrange()
}

minha_f(base)
minha_f(base2)
minha_f(base3)
