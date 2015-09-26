#
# INF-611 Herding Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

#
# DEFINING FUNCTIONS
#

# Calculate the Euclidian distance between two series
euclidian_dist <- function(serie1, serie2) {
  # Checking if the two series have the same size/dimesion
  if(length(serie1) != length(serie2)) return("ERROR")

  somatory <- 0
  for(i in 1:length(serie1)) {
    somatory <- somatory + ((serie1[i] - serie2[i]) ^ 2)
  }

  return(sqrt(somatory))
}



#
# MAIN EXECUTION STARTS HERE
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

# Delete previous console output if exists
file.remove("console_output.txt")

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
    }

    # Delete the days where the number of measures are less than 130.
    # Complete the days that have missing measures.
    n_measures <- length(time_series[[time_series[[i]]]])
    if(n_measures < 130) {
        time_series[[time_series[[i]]]] <- NULL
    }
    else if((n_measures >= 130) && (n_measures < 144)) {
        temp_aux <- time_series[[time_series[[i]]]]
        k <- 144 - length(temp_aux) # Get number of missing values
        last_temp <- tail(temp_aux, n=1) # Get last element of the series
        # Use the last element of the series to complete it.
        time_series[[time_series[[i]]]] <- c(temp_aux, rep(last_temp, k))
    }
    # Print time series into log file
    cat("day[", i, "]\n", "size = ", length(time_series[[time_series[[i]]]]), "\n",
        time_series[[time_series[[i]]]], "\n\n", file = "console_output.txt", append = TRUE)

}

# Process total data
complete_days <- 0
deleted_days <- 0
mysize <- length(time_series)
for(i in 1:mysize) {
    size <- length(time_series[[time_series[[i]]]])
    if(size == 144) {
        complete_days <- complete_days + 1
    }
    else if(size == 0) {
        deleted_days <- deleted_days + 1
    }
}
cat("Series Size: ", mysize)
cat("Valid Days: ", complete_days)
cat("Deleted Days: ", deleted_days)


cat("End of the code...")