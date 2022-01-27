source("R/bimax.R")
source("R/BiBit.R")
source("R/iBBiG.R")


#' Random binary matrix
#'
#' @description Create a random binary matrix with biclusters
#' @param n the desired number of rows
#' @param p the desired number of columns
#' @param nbClust the desired number of biclusters
#' @param d1 probability inside the biclusters
#' @param d0 probability outside the biclusters
#' @return a list with matI (visible biclusters) and matR
binaryMatrix <- function(n, p, nbClust = 10, d1 = 0.5, d0 = 0.5)
{
  # Construction d'une matrice (n,p) avec des 0 et 1, probabilité = d0
  mat <- matrix(rbinom(n = n*p, size = 1, prob = d0), n, p)
  
  ratio <- floor(n/nbClust)
  
  # Simule les lignes des biclusters, divX = intervalles des biclusters sur les lignes
  divX <- sample(1:floor(n/nbClust), size =nbClust, replace = TRUE)
  divX <- c(0, floor(cumsum(divX) * n / sum(divX)))
  
  # Simule les colonnes des biclusters, divY = intervalles des biclusters sur les colonnes
  divY <- sample(1:floor(p/nbClust), size =nbClust, replace = TRUE)
  divY <- c(0, floor(cumsum(divY)* p / sum(divY)))
  
  # Modifie les valeurs de la matrice pour chaque bicluster avec probabilité = d1
  for(i in 1:nbClust)
  {
    mat[(divX[i]+1):divX[i+1],(divY[i]+1):divY[i+1]] <- rbinom(n = (divX[i+1]-divX[i])*(divY[i+1]-divY[i]), size = 1, prob = d1)
  }
  
  return(list(matI = mat, matR = mat[sample(n),sample(p)]))
}


rotate <- function(x) t(apply(x, 2, rev))


#' Function reconst
#'
#' @description Plot an image of the biclusters
#' @param E a binary matrix with values 0 and 1
#' @param M the result of the BiMax algorithm
#' @return an image of E with biclusters
reconst<-function(E, M)
{
  ord_col <- c()
  ord_row <- c()
  for(i in 1:length(M))
  {
    if (i %% 2 == 1)
    {
      ord_row <- c(ord_row, M[[i]])
    }
    else
    {
      ord_col <- c(ord_col, M[[i]])
    }
  }
  image(rotate(E[ord_row, ord_col]))
}



# Test de l'algorithme BiMax

## 2 biclusters disjoints
Z <- list()
E <- matrix(c(rep(c(1,1,0,0,0),2),rep(c(0,0,1,1,1),3)),5,5)
print(E)
image(rotate(E), axes = FALSE)
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- conquer(E, list(1:5,1:5), Z)
print(M)
M <- bimax(E)
reconst(E, M)

E <- matrix(c(rep(c(1,0,0,0,0),2),rep(c(0,1,1,1,1),3)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- conquer(E, list(1:5,1:5), Z)
print(M)
reconst(E, M)

E <- matrix(c(rep(c(1,1,0,0,0),2),c(0,0,0,0,0),rep(c(0,0,0,1,1),2)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- conquer(E, list(1:5,1:5), Z)
print(M)
reconst(E, M)

## 3 biclusters disjoints
E <- matrix(c(rep(c(1,1,0,0,0),2),c(0,0,1,0,0),rep(c(0,0,0,1,1),2)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- conquer(E, list(1:5,1:5), Z)
print(M)
reconst(E, M)


## 5 biclusters disjoints
E <- matrix(c(c(1,0,0,0,0),c(0,1,0,0,0),c(0,0,1,0,0),c(0,0,0,1,0),c(0,0,0,0,1)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- conquer(E, list(1:5,1:5), Z)
print(M)
M <- bimax(E)
reconst(E, M)

E <- matrix(c(rep(c(1,1,1,0,0,0,0,0),3),c(0,0,0,1,0,0,0,0),rep(c(0,0,0,0,1,1,0,0),2),rep(c(0,0,0,0,0,0,1,1),2)),8,8)
print(E)
image(rotate(E))
E <- E[sample(8),sample(8)]
image(rotate(E))
M <- conquer(E, list(1:8,1:8), Z)
print(M)
reconst(E, M)

## Exemple de l'article BiSim
E <- matrix(c(rep(c(1,1,1,0,0),2),c(0,0,1,0,0),rep(c(0,0,0,1,1),2)),5,5)
print(E)
image(rotate(E), axes=FALSE)
E <- E[sample(5),sample(5)]
image(rotate(E), axes = FALSE)
M <- bimax(E)
print(M)
reconst(E, M)

E <- matrix(c(c(0,0,0,0,0), rep(c(0,0,1,1,0),3), c(0,0,0,0,0)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- bimax(E)
print(M)
reconst(E, M)

E <- matrix(c(c(0,0,0,0,0),c(0,1,0,1,0),c(0,1,1,1,0), c(0,1,1,1,0), c(0,0,0,0,0)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- bimax(E)
print(M)
reconst(E, M)

E <- matrix(c(c(0,0,0,0,0),c(0,1,1,0,0),c(0,0,0,0,0), c(0,1,1,0,0), c(0,0,0,0,0)),5,5)
print(E)
image(rotate(E))
E <- E[sample(5),sample(5)]
image(rotate(E))
M <- bimax(E)
print(M)
reconst(E, M)

## Matrice avec biclusters imparfaits
res <- binaryMatrix(70,20,5,0.9,0.1)
image(res$matI)
image(res$matR)
bimax(res$matR)

#########################################################################################
#########################################################################################

# Test de l'algorithme Bibit

n = 50
p = 10
res <- binaryMatrix(n,p,nbClust = 2)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(res$matI))
image(rotate(res$matR))
res.bibit = bibit(res$matR)
for (i in 1:length(res.bibit)) {
  #print(res.bibit[[i]]$row)
  #print(res.bibit[[i]]$col)
  #print(res$matR[res.bibit[[i]]$row, res.bibit[[i]]$col])
  image(res$matR[res.bibit[[i]]$row, res.bibit[[i]]$col])
}

#########################################################################################
#########################################################################################

# Données LaMME : iBBiG

data <- read.delim("Vincent_matrice55motif_genes.txt", header = TRUE, sep = "\t")[,-1]
colnames(data) <- NULL
data[] <- lapply(data, function(x) as.numeric(x))
data <- as.matrix(data)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(data))
dim(data)

res.iBBiG <- iBBiG(data,10)
for (i in 1:10) {
  #print(res.iBBiG[[i]]$row)
  #print(res.iBBiG[[i]]$col)
  #print(res$matR[res.iBBiG[[i]]$row, res.iBBiG[[i]]$col])
  #print(sum(res.iBBiG[[i]]$row))
  #print(sum(res.iBBiG[[i]]$col))
  image(data[res.iBBiG[[i]]$row, res.iBBiG[[i]]$col])
}
       
                 
