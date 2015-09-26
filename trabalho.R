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
    time_series[[day]] <- c(time_series[[day]], temp)
}

# Create a file in working directory to store console output
cat("Console Output", file = "console_output.txt")
cat("\n\n", file = "console_output.txt", append = TRUE)

for(i in 1:length(time_series)) {
    # If soma == NA, there's a NA temp in this day
    soma <- sum(time_series[[time_series[[i]]]])
    if(is.na(soma)) {
        num_nas = sum(is.na(time_series[[time_series[[i]]]]))
        # If there's more then 10 NAs, we'll discard this serie
        if(num_nas > 10){
            time_series[[time_series[[i]]]] <- NULL
        }
        else {
            temp_aux <- time_series[[time_series[[i]]]]
            for(j in 1:length(temp_aux)) {
              if(is.na(temp_aux[j])){
                  temp_aux[j] <- temp_aux[j-1]
              }
            }
            time_series[[time_series[[i]]]] <- temp_aux
        }
        cat("day[", i, "] = ", time_series[[i]],"num_nas = ", num_nas, "\n",
            file = "console_output.txt", append = TRUE)
        cat("time_series:\n", time_series[[time_series[[i]]]], "\n\n",
            file = "console_output.txt", append = TRUE)
    }
}

# Get the number of measures there are in each day
mycount <- 0
mysize <- length(time_series)
for(i in 1:mysize) {
    size <- length(time_series[[time_series[[i]]]])
    cat("Day ", i, " = ", size, "\n", file = "console_output.txt", append = TRUE)
}

cat("End of the code...")