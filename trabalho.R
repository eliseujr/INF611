#
# INF-611 Herding Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

# Read data from cepagri url
connection <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Parse cepagri data to a dataframe
cpa_data <- read.csv(connection, header = FALSE, sep = ";", stringsAsFactors = F,
                     col.names = c("horario","temperatura","vento","umidade","sensacao"))

# Parse time to the format YYYY-MM-DD
cpa_data[[1]] <- strptime(cpa_data[[1]], "%d/%m/%Y")

cpa_data <- data.frame(Horario=cpa_data[1], Temperatura=cpa_data[2])

# Parse the dataframe to a data structure where the date is the index
# of the vector with the measures.
time_series <- list()
measured_dates <- unique(cpa_data[[1]])

for(i in 1:length(measured_dates)) {
    time_series <- c(time_series, list(measured_dates[i]))
}

