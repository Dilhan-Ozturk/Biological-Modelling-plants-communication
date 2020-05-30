# LOAD LIBRARIES (might need to install them first)

library(cowplot)
library(archiDART)
library(viridis)
library(dplyr)
library(readr)

# LOAD FILES 
path <- "C:/Users/demo/OneDrive - UCL/LBRAI2219 Modelisation de systemes biologiques/Get_time_before_mycorhize/"
ls <- list.files(path) # Get all the file names in the selected folder
ls <- ls[grepl(".txt", ls)] # Select on the .txt files

# Loop over the files and load them
roots <- NULL  # this dataframe will contain the simulation results
for(l in ls){
  temp <- read_delim(file = paste0(path, l), delim = " ") %>% # Load the file 
    mutate(file = l) %>% # Add the file name as a newcolumn, to disciminate the different files in the final dataframe
    mutate(length = sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)) # Add the length of each segement as a newcolumn
  roots <- rbind(roots, temp)  # Add the file to the main datafile
}


# POSITION THE TWO ROOT SYSTEMS IN THE XZ SPACE

dist <- 10.8 # Set the distance between the two plants

xMin <- min (min(roots$x1), min(roots$x2)) # Find the minimal initial x value
zMax <- max (max(roots$z1), max(roots$z2)) # Find the maximal initial z value

files <- unique(roots$file) # Get the names of the two files
roots <- roots %>%
  mutate(z1 = z1-zMax)%>%
  mutate(z2 = z2-zMax)%>%
  mutate(x1 = ifelse(file == files[2], x1-xMin+dist, x1-xMin),
         x2 = ifelse(file == files[2], x2-xMin+dist, x2-xMin)) # Apply the translation to root systems

# Plot the two root systems
roots %>%
  ggplot()+
  geom_segment(aes(x1, z1, xend = x2, yend=z2, colour = factor(type))) +  # We use the 'segment' representation for each root segment
  coord_fixed()


# DISCRETIZE THE SPACE INTO CELLS

region <- dist/10 # Size of a cell

system_width <- max (max(roots$x1), max(roots$x2)) # Width of the system
system_height = abs(min (min(roots$z1), min(roots$z2))) # Height of the system

# Create 2 matrices of discretized space (one for each plant)
MatrixSeg1 <- matrix(0, ceiling(system_height/region), ceiling(system_width/region))
MatrixSeg2 <- matrix(0, ceiling(system_height/region), ceiling(system_width/region))


root2D <- roots %>% 
  mutate(z = floor(z1/region), 
         x = ceiling(x1/region)) # Round the z and x values, to limit the number of layers

# Separate data per plant
root2D_1 <- root2D %>%
  filter(file == files[1])
root2D_2 <- root2D %>%
  filter(file == files[2])

# Fill the matrices of discretized space with the number of segments in each cell
for (i in 1:dim(root2D_1)[1]){
  MatrixSeg1[abs(root2D_1$z[i]), root2D_1$x[i]] = MatrixSeg1[abs(root2D_1$z[i]), root2D_1$x[i]] + 1
}

for (i in 1:dim(root2D_2)[1]){
  MatrixSeg2[abs(root2D_2$z[i]), root2D_2$x[i]] = MatrixSeg2[abs(root2D_2$z[i]), root2D_2$x[i]] + 1
}


contact <- which(MatrixSeg1 & MatrixSeg2, arr.ind = TRUE) # Get the indices of cells with segments of plant 1 and 2

if (dim(contact)[1] == 0){
  print('There is no mychorhise communication between the two plants')
} else {
  # For plant 1, keep only the segments in a cell where there is a contact
  int1 <- NULL
  for (j in 1:dim(contact)[1]){
    for (n in 1:dim(root2D_1)[1]){
      if (abs(root2D_1$z[n]) == contact[j,1] & root2D_1$x[n] == contact[j,2]){
        int1 <- rbind(int1, root2D_1[n,])
      }
    }
  }
  
  # For plant 1, keep only the youngest segment in each cell where there is a contact
  for (i in dim(int1)[1]:2){
    if (int1$x[i] == int1$x[i-1] & int1$z[i] == int1$z[i-1] & int1$time[i] >= int1$time[i-1]) {
      int1 <- int1[-i,]
    } else if (int1$x[i] == int1$x[i-1] & int1$z[i] == int1$z[i-1] & int1$time[i] < int1$time[i-1]){
      int1 <- int1[-(i-1),]
    }
  }
  
  # For plant 2, keep only the segments in a cell where there is a contact
  int2 <- NULL
  for (j in 1:dim(contact)[1]){
    for (n in 1:dim(root2D_2)[1]){
      if (abs(root2D_2$z[n]) == contact[j,1] & root2D_2$x[n] == contact[j,2]){
        int2 <- rbind(int2, root2D_2[n,])
      }
    }
  }
  
  # For plant 2, keep only the youngest segment in each cell where there is a contact
  for (i in dim(int2)[1]:2){
    if (int2$x[i] == int2$x[i-1] & int2$z[i] == int2$z[i-1] & int2$time[i] >= int2$time[i-1]) {
      int2 <- int2[-i,]
    } else if (int2$x[i] == int2$x[i-1] & int2$z[i] == int2$z[i-1] & int2$time[i] < int2$time[i-1]){
      int2 <- int2[-(i-1),]
    }
  }
  
  # Add the time needed for contact between mycorhizes of plant 1 and 2 as new column of "contact"
  time <- NULL
  for (t in 1:dim(contact)[1]){
    time[t] <- max(int1$time[t],int2$time[t])
  }
  (contact <- cbind(contact, time))
  
  # Get the minimal time needed before a mychorhize communication between plant 1 and 2 
  (min_time <- min(contact[,3]))
}