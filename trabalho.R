#
# INF-611 Herding Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

# Read data from cepagri url
connection <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Parse cepagri data to a dataframe
cpa_data <- read.csv(connection, header = FALSE, sep = ";",
                     col.names = c("horario","temperatura","vento","umidade","sensacao"))

# Parse time to the format YYYY-MM-DD
cpa_data[[1]] <- strptime(cpa_data[[1]], "%d/%m/%Y")

cpa_data <- data.frame(Horario=cpa_data[1], as.numeric((Temperatura=cpa_data[2])))
#colnames(cpa_data) <- c("Horario", "Temperatura")

serie <- list()

#FIXME aqui so estou pegando as primeiras 100 linhas pra testar
for(i in 1:100) {
  # pegando o primeiro dia
  day <- as.character(cpa_data[i, ][[1]])
  
  # atribuindo o vetor/lista de temperaturas na lista serie[dia]
  #serie[as.character(cpa_data[i, ][[1]])] <- c()
}
