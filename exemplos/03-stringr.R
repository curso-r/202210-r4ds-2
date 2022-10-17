
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
