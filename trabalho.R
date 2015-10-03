#
# INF-611 Herding Data
# Atividade a Distancia
# Aluno: Eliseu Massing Junior
# Aluno: Marcos Aurelio Freitas de Almeida Costa
#

#
# DEFINING FUNCTIONS
#

# Constants
EUCLIDEAN_FUNCTION <- "EUCLIDEAN"
COSINE_FUNCTION <- "COSINE"
DEBUG <- FALSE

# Calculate the Euclidian distance between two series
euclidean_dist <- function(serie1, serie2) {
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
    idx <- 1

    if(is.null(input_serie)) return(NULL)

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
          query_result[[idx]] <- list(day = current_day, dist = similarity)
          idx <- idx + 1
        }
    }

    return(query_result)
}

sort_query_result <- function(query_to_sort, distance_function = EUCLIDEAN_FUNCTION) {
  if(is.null(query_to_sort)) return(NULL)

  query_dates <- sapply(query_to_sort, "[[", "day")
  query_similarity <- sapply(query_to_sort, "[[", "dist")

  if(distance_function == EUCLIDEAN_FUNCTION) {
    return(query_dates[order(query_similarity)])
  }
  else if(distance_function == COSINE_FUNCTION) {
    return(query_dates[order(query_similarity, decreasing = TRUE)])
  }
  else {
    return("UNKNOWN DISTANCE FUNCTION")
  }
}

# Return P@30  precision
p30_precision <- function(input_date, ordered_distance_list) {
  if(is.null(ordered_distance_list)) return(-1)

  same_month <- 0
  input_month <- substring(input_date, 1, 7)

  # Delete the first element which is the day under query
  for(i in 2:31) {
    month <- substring(ordered_distance_list[i], 1, 7)
    if(month == input_month) same_month <- same_month + 1
  }
  
  precision <- signif((same_month / 30)*100, 4)

  return(precision)
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

    # Delete the days where the number of measures are less than 130 or more then 144
    n_measures <- length(time_series[[time_series[[i]]]])
    if(n_measures < 130 || n_measures > 144) {
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



#
# CALCULATING THE P@30 PRECISIONS FOR EUCLIDIAN AND COSINE FOR EACH DAY
#

# Get how many days per month
days_per_month <- list()
for(i in 1:length(measured_dates)) {
  current_month <- substring(measured_dates[[i]], 1, 7)
  if(is.null(days_per_month[[current_month]])) {
    days_per_month[current_month] <- 1
  }
  else {
    days_per_month[current_month] <- days_per_month[[current_month]] + 1
  }
}

p30_precisions_euclidean <- c()
p30_precisions_cosine <- c()
p30_per_month_euclidean <- list()
p30_per_month_cosine <- list()
for(i in 1:length(measured_dates)) {
  day_i <- time_series[[i]]

  # Calculating the distances for day_i
  euclidean_distances <- calculate_distances(time_series[[day_i]], time_series, euclidean_dist)
  cosine_similarities <- calculate_distances(time_series[[day_i]], time_series, cosine_sim)

  # Ranking to get the @30 best results
  sort_euclidean <- sort_query_result(euclidean_distances)
  sort_cosine    <- sort_query_result(cosine_similarities, distance_function = COSINE_FUNCTION)

  # Calculating the P@30 for each day
  daily_p30_euclidean <- p30_precision(day_i, sort_euclidean)
  daily_p30_cosine <- p30_precision(day_i, sort_cosine)
  p30_precisions_euclidean <- c(p30_precisions_euclidean, daily_p30_euclidean)
  p30_precisions_cosine    <- c(p30_precisions_cosine,    daily_p30_cosine)
  
  # Monthly amount for each distance function
  # Euclidean
  if(is.null(p30_per_month_euclidean[[substring(day_i, 1, 7)]])) {
      p30_per_month_euclidean[substring(day_i, 1, 7)] <- daily_p30_euclidean
  }
  else {
      p30_per_month_euclidean[substring(day_i, 1, 7)] <- 
              p30_per_month_euclidean[[substring(day_i, 1, 7)]] + daily_p30_euclidean
  }
  # Cosine
  if(is.null(p30_per_month_cosine[[substring(day_i, 1, 7)]])) {
    p30_per_month_cosine[substring(day_i, 1, 7)] <- daily_p30_cosine
  }
  else {
    p30_per_month_cosine[substring(day_i, 1, 7)] <- 
      p30_per_month_cosine[[substring(day_i, 1, 7)]] + daily_p30_cosine
  }
}



#
# CALCULATING THE MONTHLY P@30 MEDIAN
#
graph_euclidean <- c()
graph_cosine <- c()
for(i in 1:length(p30_per_month_cosine)) {
    euclidean_month_avg <- p30_per_month_euclidean[[i]] / days_per_month[[i]]
    graph_euclidean <- c(graph_euclidean, euclidean_month_avg)
    
    cosine_month_avg <- p30_per_month_cosine[[i]] / days_per_month[[i]]
    graph_cosine <- c(graph_cosine, cosine_month_avg)
}

# Plotting the graph
g_range <- c(0, 26)
plot(graph_euclidean, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE) ; box()
lines(graph_cosine, type="o", col="red", pch=22)
axis(1, at=1:20, lab=labels(days_per_month), las=2)
axis(2, las=1, at=2*0:g_range[2])
title(main="Montly P@30 mean", col.main="blue", font.main=4)
#title(xlab="Month", col.lab=rgb(0,0.5,0))
title(ylab="P@30 mean", col.lab=rgb(0,0.5,0))
legend(1, g_range[2], c("Euclidian","Cosine"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:1)



#
# CALCULATING THE OVERALL P@30 MEDIAN
#
elem_number <- 0
summatory_cosine <- 0
summatory_euclidian <- 0

for(i in 1:length(measured_dates)) {
  if(p30_precisions_euclidean[i] == -1) next
  
  summatory_euclidian <- summatory_euclidian + p30_precisions_euclidean[i]
  summatory_cosine <- summatory_cosine + p30_precisions_cosine[i]
  
  elem_number <- elem_number + 1
}

cat("EUCLIDIAN OVERALL P@30 MEDIAN = ", (summatory_euclidian/elem_number), "\n")
cat("COSINE OVERALL P@30 MEDIAN = ", (summatory_cosine/elem_number), "\n")

