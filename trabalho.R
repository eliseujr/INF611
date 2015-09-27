#
# INF-611 Herding Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

#
# DEFINING FUNCTIONS
#

# Debugging flag
DEBUG <- FALSE

# Calculate the Euclidian distance between two series
euclidian_dist <- function(serie1, serie2) {
  # Checking if the two series have the same size/dimesion
  if(length(serie1) != length(serie2)) return("ERROR")

  distance <- sqrt(sum( (serie1-serie2)^2) )

  return(distance)
}

# Calculate the Cosine similarity between two series
cosine_sim <- function(serie1, serie2) {
  # Checking if the two series have the same size/dimesion
  if(length(serie1) != length(serie2)) return("ERROR")

  similarity <- sum(serie1*serie2) / (sqrt(sum(serie1*serie1)) *  sqrt(sum(serie2*serie2)))

  return(similarity)
}

# Calculate the distances between a input serie and the whole dataset
# taking a distance function as parameter
calculate_distances <- function(input_serie, set_of_series, distance_function) {
    query_result <- list()
    set_series_size <- length(measured_dates)

    for(i in 1:set_series_size) {
        if(DEBUG) cat("Serie ", i, "\n", file = "console_output.txt", append = TRUE)
        current_serie <- set_of_series[[set_of_series[[i]]]]
        current_day <- sapply(set_of_series[i], function(x) {x[[1]]})
        if(length(input_serie) != length(current_serie)) {
          if(DEBUG) cat("\n\n", file = "console_output.txt", append = TRUE)
          next # skip this iteration
        }
        else {
          similarity <- distance_function(input_serie, current_serie)
          if(DEBUG) cat("Day = ", current_day, file = "console_output.txt", append = TRUE)
          if(DEBUG) cat("\nSimilarity = ", similarity, "\n\n", file = "console_output.txt", append = TRUE)
          query_result[current_day] <- list(dist = similarity)
        }
    }

    return(query_result)
}

#TODO need to ordenate the distances list now to be able to calculate the precision
# Return P@30  precision
p30_precision <- function(distance_list) {
  precision <- 0
  distances <- c()
  
  for(i in 1:length(distance_list)) {
    distances <- c(distances, distance_list[[i]])
  }
  
  return(cat("P@30 precision is ", precision, "\n"))
}

#
# MAIN EXECUTION STARTS HERE
#

# Read data from cepagri url
connection <- url("http://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

# Parse cepagri data to a dataframe
cpa_data <- read.csv(connection, header = FALSE, sep = ";", stringsAsFactors = F,
                     col.names = c("horario","temperatura","vento","umidade","sensacao"))

# Update date to the format YYYY-MM-DD
cpa_data[[1]] <- strptime(cpa_data[[1]], "%d/%m/%Y")
cpa_data <- data.frame(Horario=cpa_data[1], Temperatura=cpa_data[2])

# Parse the dataframe to a list() where the date is the index of a vector with the measures 
# eg.: time_series["2014-03-03"] == c(21.2, 21.2, 21.0, 20.9, 20.8, ...) 
time_series <- list()
measured_dates <- unique(as.character(cpa_data[[1]]))

# Initializing the list indexes -> dates
for(i in 1:length(measured_dates)) {
    time_series <- c(time_series, list(measured_dates[i]))
}

# Reading all measures from the data.frame/csv and populating our list()
for(i in 1:nrow(cpa_data)) {
    day <- as.character(cpa_data[[1]][i])
    temp <- as.numeric(cpa_data[[2]][i])
    time_series[[day]] <- c(time_series[[day]], temp)
}

# DEBUG : Create a file in working directory to store console output
if(DEBUG) file.remove("console_output.txt")
if(DEBUG) cat("Console Output", file = "console_output.txt")
if(DEBUG) cat("\n\n", file = "console_output.txt", append = TRUE)

# This next for is our heuristic to deal with NAs and missing measures
# For each day, we'll check the number of NAs. If there's more than 10 NAs, we'll discard this day
# Otherwise, we'll populate these NAs with the last available measure
for(i in 1:length(measured_dates)) {
    # If total_sum == NA, there's a NA temp in this day
    total_sum <- sum(time_series[[time_series[[i]]]])
    if(is.na(total_sum)) {
        num_nas = sum(is.na(time_series[[time_series[[i]]]]))
        # If there's more then 10 NAs, we'll discard this serie
        if(num_nas > 10){
            time_series[[time_series[[i]]]] <- NULL
        }
        # Otherwise, populating with last measure
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

    # Delete the days where the number of measures are less than 130
    n_measures <- length(time_series[[time_series[[i]]]])
    if(n_measures < 130) {
        time_series[[time_series[[i]]]] <- NULL
    }
    # Complete the days that have missing measures
    else if((n_measures >= 130) && (n_measures < 144)) {
        temp_aux <- time_series[[time_series[[i]]]]
        k <- 144 - length(temp_aux) # Get number of missing values
        last_temp <- tail(temp_aux, n=1) # Get last element of the series
        # Use the last element of the series to complete it.
        time_series[[time_series[[i]]]] <- c(temp_aux, rep(last_temp, k))
    }
    # DEBUG : Print time series into log file
    if(DEBUG) cat("day[", i, "] -> ", time_series[[i]] , " \n", "size = ", length(time_series[[time_series[[i]]]]),
                  "\n", time_series[[time_series[[i]]]], "\n\n", file = "console_output.txt", append = TRUE)

}

#  DEBUG : Write the output to text file
if(DEBUG) file.remove("console_output.txt") # Delete previous console output if exists
if(DEBUG) cat("Console Output", file = "console_output.txt")
if(DEBUG) cat("\n\n", file = "console_output.txt", append = TRUE)


# Test the code
my_test_input <- time_series[[time_series[[20]]]]
my_euclidian_distances <- calculate_distances(my_test_input, time_series, euclidian_dist)
my_cosine_distances <- calculate_distances(my_test_input, time_series, cosine_sim)
