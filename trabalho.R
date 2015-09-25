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
measured_dates <- unique(as.character(cpa_data[[1]]))

for(i in 1:length(measured_dates)) {
    time_series <- c(time_series, list(measured_dates[i]))
}

for(i in 1:nrow(cpa_data)) {
    day <- as.character(cpa_data[[1]][i])
    temp <- as.numeric(cpa_data[[2]][i])
    #FIXME -> Temos que corrigir o NA no for abaixo... não aqui
    # If temp is NA, get the last measure
    if(is.na(temp)){
        temp <- tail(time_series[[day]], n=1)
    }
    time_series[[day]] <- c(time_series[[day]], temp)
}

for(i in 1:length(time_series)) {
    # If soma == NA, there's a NA temp in this day
    soma <- sum(time_series[[time_series[[i]]]])
    if(is.na(soma)) {
        num_nas = sum(is.na(time_series[[time_series[[i]]]]))
        # If there's more then 10 NAs, we'll discard this serie
        if(num_nas > 10){
            time_series[[time_series[[i]]]] <- NULL
        }
        cat("day[", i, "] = ", time_series[[i]],"num_nas = ", num_nas, "\n")
        cat("time_series:\n", time_series[[time_series[[i]]]], "\n\n")
    }
}


