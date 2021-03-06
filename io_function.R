# FUNCTION TO READ THE PPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME
read_rparam <- function(path){
  
  fileName <- path
  param <- read_file(fileName)
  
  
  param <- strsplit(param, "#")
  dataset_init <- NULL
  for(k in c(2:length(param[[1]]))){
    spl <- strsplit(param[[1]][k], "\n")
    type <- ""
    name <- ""
    for(i in c(1:length(spl[[1]]))){
      temp <- spl[[1]][i]
      pos <- regexpr("//", temp)
      if(pos != -1) temp <- substr(temp, 0, pos-1)
      if(nchar(temp) > 0){
        temp <- strsplit(temp, "\t")
        temp2 <- data.frame("type" = character(0), "name" = character(0), 
                            "param" = character(0), "val1" = numeric(0),
                            #Addition of val4
                            "val2" = numeric(0), "val3" = numeric(0), "val4" = numeric(0), stringsAsFactors = F)
        
        if(temp[[1]][1] == "type"){ type <- temp[[1]][2]
        } else if(temp[[1]][1] == "name"){ name <- temp[[1]][2]
        } else if(grepl("Param", temp[[1]][1])){
        } else if(temp[[1]][1] == "tropism") {
          temp2[[1,3]] <- "n_tropism"
          temp2$val1 <- temp[[1]][3]
          temp2$type <- type
          temp2$name <- name
          dataset_init <- rbind(dataset_init, temp2)
          temp2$param <- "sigma_tropism"
          temp2$val1 <- temp[[1]][4]
          temp2$type <- type
          temp2$name <- name
          dataset_init <- rbind(dataset_init, temp2)  
          temp2$param <- "tropism"
          temp2$val1 <- temp[[1]][2]
          temp2$type <- type
          temp2$name <- name
          dataset_init <- rbind(dataset_init, temp2)  
        } else {
          for(j in c(1:5)){
            temp2[[1,j+2]] <- temp[[1]][j]
            temp2$type <- type
            temp2$name <- name
          }
          dataset_init <- rbind(dataset_init, temp2)
        }
      }
    }
  } 
  
  return(dataset_init)
}


# FUNCTION TO READ THE PPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME
read_pparam <- function(path){
  ## READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
  data <- read_file(path)
  # READ THE PARAMETER FILE AND STORE THE DATA IN A DATAFRAME
  plant_init <- NULL
  spl <- strsplit(data, "\n")
  for(i in c(1:length(spl[[1]]))){
    temp <- spl[[1]][i]
    if(nchar(temp) > 0){
      temp <- strsplit(temp, "\t")
      temp2 <- data.frame( "param" = character(0), "val1" = numeric(0), stringsAsFactors = F)
      for(j in c(1:2)){
        temp2[[1,j]] <- temp[[1]][j]
      }
      plant_init <- rbind(plant_init, temp2)
    }
  }      
  
  colnames(plant_init) <- c("param", "val1")  
  return(plant_init)
}




# FUNCTION TO WRITE THE RPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME

write_rparam <- function(dataset, files){
  
  types <- unique(dataset$type)
  text <- NULL
  for(t in types){
    if(is.null(text)){text <- "# Parameter set for type"
    }else{
      text <- paste(text, "# Parameter set for type", sep="\n")
    }
    
    temp <- dataset[dataset$type == t,]
    
    str <- paste("type", temp$type[1], sep="\t")
    text <- paste(text, str, sep="\n")
    
    str <- paste("name", temp$name[1], sep="\t")
    text <- paste(text, str, sep="\n")
    
    for(i in c(1:nrow(temp))){
      if(temp[i, 3] == "n_tropism"){
        str <- paste("tropism", temp[i+2, 4], temp[i, 4], temp[i+1, 4], sep="\t")
        text <- paste(text, str, sep="\n")
      }else if(temp[i, 3] == "sigma_tropism" | temp[i, 3] == "tropism"){
      }else if(temp[i, 3] == "dx"){
        str <- paste(temp[i, 3], temp[i, 4], sep="\t")
        text <- paste(text, str, sep="\n")
      }else{
        str <- paste(temp[i, 3], temp[i, 4], temp[i, 5], temp[i, 6], temp[i, 7], sep="\t")
        text <- paste(text, str, sep="\n")
      }
    }
    
  }
  text <- gsub("\tNA", "", text)
  for(f in files){
    cat(text, file=f)
  }
  
}

# FUNCTION TO WRITE THE PPARAM FILES FROM CROOTBOX AND STORE IT INTO A DATAFRAME

write_pparam <- function(plant, files){
  text <- NULL
  for(i in c(1:nrow(plant))){
    str <- paste(plant[i, 1], plant[i, 2], sep="\t")
    text <- paste(text, str, sep="\n")
  }
  
  text <- gsub("\tNA", "", text)
  
  for(f in files){
    cat(text, file=f)
  }
}