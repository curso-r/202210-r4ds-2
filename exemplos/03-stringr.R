
vetor <- c("isso \"aquilo\"", "\"outro\"")
paste0(vetor, "!")
cat(paste0(vetor, "!"))

paste("a", "b", "c")
paste0("a", "b", "c")

library(stringr)
str_c("a", "b", "c")

v <- c("a", "b", "c")
str_c(v, " é uma letra")
str_c("uma letra: ", v)
str_c("Comando:", " uma letra = ", v)
str_c("a", "b", "c")

1234

str_extract("Esse texto é importante! O meu número é 1234", "(1234)")
str_remove("Esse texto é importante! O meu número é 1234", "(1234)")

str_detect("Esse texto é importante! O meu número é 1234", "ando")
str_extract("Esse texto é importante! O meu número é 1234", "ando")
str_remove("Esse texto é importante! O meu número é 1234", "ando")

00000-0000

"teste1" == "teste"

"  asdf \"texto\" asdfasdf "
"\\."


# pratica -----------------------------------------------------------------

library(tidyverse)
library(stringr)

# -------------------------------------------------------------------------

# tenho uma lista de emails

texto = c("meu e-mail é fulano@bol.com.br, meu telefone é: (11) 96563-3243 e meu cep é 07642-126",
          "Bom dia!! \n E-mail: ciclana@gmail.com \n Contato: 5322-1234 \n CEP: 05544-147",
          "o telefone é +78(32) 6783-5234, cep:  35687-999, email: contato56@bla.br. VALEU",
          "Meu.nome@hotmail.com, 943421516, 54869-147")

emails <- tibble(texto)

# quero separar as informacoes em 3 colunas: email, cep e telefone

# cep:

# 5 numeros: [0-9]{5}
# traço: -
# 3 numeros: [0-9]{3}

# nao pode ter numero depois! então quero:
# ou algum caractere que nao seja numero: [^0-9]
# ou o final da string: $

regex_cep <- "[0-9]{5}-[0-9]{3}([^0-9]|$)"

# email:

# sequencia de 1 a 100 letras maiusculas ou minusculas ou numeros ou pontos: [a-zA-Z0-9\\.]{1,100}
# arroba: @
# sequencia de 1 a 20 letras minusculas: [a-z]{1,20}
# pode ou nao ter ".com": (.com)?
# pode ou nao ter ".br": (.br)?

regex_email <- "[a-zA-Z0-9\\.]{1,100}@[a-z]{1,20}(.com)?(.br)?"

# telefone:

# pode ou nao ter + seguido de 2 numeros: (\\+[0-9]{2})?
# pode ou nao ter 2 numeros entre paranteses, seguido de um espaço: (\\([0-9]{2}\\) )?
# 4 ou 5 numeros: [0-9]{4,5}
# separados ou nao por traço: -?
# 4 numeros: [0-9]{4}

regex_telefone <- "(\\+[0-9]{2})?(\\([0-9]{2}\\) )?[0-9]{4,5}-?[0-9]{4}"

# criando uma coluna para cada informacao:

emails |>
  mutate(
    telefone = str_extract(texto, regex_telefone),
    email = str_extract(texto, regex_email),
    cep = str_extract(texto, regex_cep)
  ) |> View()


# duvida: se os textos viessem com "cep" antes, teria um jeito mais facil?

ceps <- c("kajsduibqefbef iuqhqiuf cep 05798-927 obg",
          "   JHGDOQIEF haefh cep 00000-111")

ceps |> str_extract("cep .+") |>
  str_remove("cep ") |>
  str_extract("[0-9-]+")


# e se a pessoa desse 1 ou mais numeros de telefone?

texto2 <- "meu e-mail é fulano@bol.com.br, meu telefone é: (11) 96563-3243, meu outro telefone é (12) 94545-1369 e meu cep é 07642-126"

# retorna só a primeira ocorrencia
str_extract(texto2, regex_telefone)

# retorna todas as ocorrencias
str_extract_all(texto2, regex_telefone)

# -------------------------------------------------------------------------

# Base IMDB

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")


# Motivação: descobrir a quantos gêneros cada filme pertence

?str_count

imdb |>
  mutate(
    num_generos = str_count(generos, ",") + 1
  ) |>
  select(generos, num_generos) |>
  head()

# Motivação: extrair o subtítulos dos filmes

imdb |>
  mutate(
    subtitulo = str_extract(titulo, ": .+") |> str_remove(": ")
  ) |>
  select(titulo, subtitulo) |>
  filter(!is.na(subtitulo)) |>
  head()

# Motivação: criar uma tabela apenas com filmes cujo
# título comece com um número

imdb |>
  filter(str_detect(titulo, "^[0-9]")) |>
  select(titulo) |>
  head()


# Motivação: criar uma tabela apenas com filmes em que
# Nicolas Cage faz parte do elenco

imdb |>
  filter(str_detect(elenco, "Nicolas Cage")) |>
  select(titulo, elenco) |>
  View()
