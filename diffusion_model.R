#diffusion Model 

Ly = ceiling (hauteur_totale/region)             #  domain length along y  (Cm)
Lx = ceiling(largeur_totale/region)              #   domain length along x  (cm)
K  = 0.018                                       # diffusion coefficient((cm?/day))
q  = 0.0                                         # linear reaction rate (unit of mol?cule / day)
Ncolonne = dim(isRoot2)[2]   #Nx                 # number of elements along x [=> Nx+1 nodes)     
Nligne = dim(isRoot2)[1]     #Ny                 # number of elements along y (=> Ny+1 nodes)
Dx = Lx/Ncolonne                                 # grid size along xded
Dy = Ly/Nligne                                   # grid size along y
T  = max(ceiling(root2D_2$time))                 # integration time    (day)
dt = 1 
Nt = T*ceiling(T/(T*dt))                           


# % Initialization
# % --------------
t=seq (from=0, to = Nt, by=dt) 
xi=seq(from=1 ,to = Lx, by = 1)                                  # coordinates of grid nodes
yi=seq(from=1 ,to = Ly, by = 1)
#x<-matrix(xi,nrow=length(yi),ncol=length(xi),byrow=TRUE)        
#y<-matrix(xi,nrow=length(yi),ncol=length(xi),byrow=FALSE)



Cold =  matrix(0,nrow=Nligne,ncol=Ncolonne,byrow=TRUE) 
Cold=Cold*0
Cnew  = Cold;




# % Integration of the equation
# % ---------------------------
for (k in 1 : Nt){
  for (i in 1 :  Nligne){
    for (j in 1 : Ncolonne){
      Cij = Cold[i,j]
      
      if (j>1){     Cl = Cold[i,j-1] } else{ Cl = Cij}    # end left
      if (j<Ncolonne){  Cr = Cold[i,j+1] } else{ Cr = Cij}# end right
      if (i>1){     Cd = Cold[i-1,j] } else{ Cd = Cij}    # end % down
      if (i<Nligne){  Cu = Cold[i+1,j] } else{ Cu = Cij}  # end % up
      # diffusion
      difx_ij = K*dt*(Cr-2*Cij+Cl)/(Dx*Dx);
      dify_ij = K*dt*(Cu-2*Cij+Cd)/(Dy*Dy);
      # new solution
      Cnew[i,j] = Cij  + difx_ij + dify_ij 
      
    }
  }
  
  Cold = Cnew;
  Cold=Cold+Exudate1[,,k]
  
  
  
  if (! all((round(isRoot2[,,k]*Cold,5)==0))) {   # we keep 5 significative number, so the isgnal have to be bigger than unit of chemical/10000
    print('limit reached')
    break}                            #produit terme Ã  terme de diffusion et position de racine permet de savoir si il y a contacte entre eux
  print(round(Cnew*100,3))
  print(c('jour=',k*dt,'nti=',k))  }