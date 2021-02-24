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

url_list <- function(id){ # id is a character vector
  url <- c() #creating an empty vector for the urls
  for (i in seq_along(id)){
    url <- c(url, paste("./specdata/", id[i], ".csv",sep = ""))
  }
  return(url)
}

pollutantmean <- function(directory, pollutant, id = 1:332){
  id <- id_into_character(id) #turning id into a character vector
  sums <- c() #creating an empty vector for the sums of all the data
  data_count <- 0 #how many data we have that is not NA
  
  for (i in seq_along(id)){
    data <- read.csv(url_list(id)[i])
    sums <- c(sums, sum(data[pollutant][!is.na(data[pollutant])]))
    data_count <- data_count + sum(!is.na(data[pollutant]))
  }
  
  return(sum(sums) / data_count)
}