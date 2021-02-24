id_into_character <- function(id){ #id is an integer vector
  id_char <- c() #creating an empty vector for id in character form
  for (i in seq_along(id)){
    if (id[i] < 10){ #if the integer has only 1 digit
      id_char <- c(id_char, paste("00", as.character(id[i]), sep = ""))
    }
    else if (id[i] < 100){ #if the integer has 2 digit
      id_char <- c(id_char, paste("0", as.character(id[i]), sep = ""))
    }
    else if (id[i] <= 332) { #if the integer has 3 digit
      id_char <- c(id_char, as.character(id[i]))
    }
  }
  return(id_char)
}

url_list <- function(id){ # id is an integer vector
  id <- id_into_character(id) #turning id into a character vector
  url <- c() #creating an empty vector for the urls
  for (i in seq_along(id)){
    url <- c(url, paste("./specdata/", id[i], ".csv",sep = ""))
  }
  return(url)
}

complete <- function(directory, id){
  nobs <- c() #creating an empty vector to store complete cases count
  
  for (i in seq_along(id)){
    data <- read.csv(url_list(id)[i])
    complete_cases <- !is.na(data$nitrate) & !is.na(data$sulfate)
    nobs <- c(nobs, sum(complete_cases))
  }
  return(as.data.frame(cbind(id, nobs)))
}

df <- complete(specdata, 1:332)

corr <- function(directory, threshold = 0){
  above_threshold <- which(df$nobs > threshold) #location of data above threshold
  above_threshold <- id_into_character(above_threshold)
  correlation <- c()
  
  for (i in seq_along(above_threshold)){ #obtaining all the correlations
    data <- read.csv(paste("./specdata/", above_threshold[i], ".csv",sep = ""))
    complete_cases <- !is.na(data$nitrate) & !is.na(data$sulfate)
    correlation <- c(correlation, cor(data$sulfate[complete_cases],
                                      data$nitrate[complete_cases]))
  }
  
  #printing the correlations
  if (length(correlation) > 0){
    return(correlation)
  }
  if (length(correlation) == 0){
    return(as.numeric(correlation))
  }
}