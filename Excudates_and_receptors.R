Exudate1 <- array(0, dim = c(ceiling(system_height/region), ceiling(system_width/region), 
                             max(ceiling(root2D_1$time))))

for (t in 1:max(ceiling(root2D_1$time))){
  for (i in 1:dim(root2D_1)[1]){
    if ((ceiling(root2D_1$time)[i] == t | ceiling(root2D_1$time)[i] == t-1 | ceiling(root2D_1$time)[i] == t-2)& root2D_1$type[i] != 20 & root2D_1$type[i] != 50){
      Exudate1[abs(root2D_1$z[i]), root2D_1$x[i],t] = Exudate1[abs(root2D_1$z[i]), root2D_1$x[i],t] + 1
    }
  }
}

isRoot2 <- array(0, dim = c(ceiling(system_height/region), ceiling(system_width/region), 
                            max(ceiling(root2D_2$time))))

for (t in 1:max(ceiling(root2D_2$time))){
  for (i in 1:dim(root2D_2)[1]){
    if (ceiling(root2D_1$time)[i] <= t & root2D_1$type[i] != 20 & root2D_1$type[i] != 50) {
      isRoot2[abs(root2D_2$z[i]), root2D_2$x[i],t] = 1
    }
  }
}