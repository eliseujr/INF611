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

# Parse time to the format YYYY-MM-DD HH:MM:SS
cpa_data[[1]] <- strptime(cpa_data[[1]], "%d/%m/%Y-%H:%M")

cpa_data <- data.frame(Horario=cpa_data[1], Temperatura=cpa_data[2])
colnames(cpa_data) <- c("Horario", "Temperatura")