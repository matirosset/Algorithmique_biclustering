#' BiMax algorithm
#'
#' @description Find all inclusion-maximal biclusters in a binary matrix using the BiMax algorithm
#' @param E a binary matrix with values 0 and 1
#' @return a vector of biclusters (G,C), G bicluster'rows, C bicluster'columns
bimax<-function(E)
{
  Z <- list()
  
  M <- conquer(E, list(1:nrow(E), 1:ncol(E)), Z)
  
  return(M)
  
}



#' Conquer function of BiMax algorithm
#'
#' @description Find all inclusion-maximal biclusters in a binary submatrix using the BiMax algorithm
#' @param E a binary matrix with values 0 and 1
#' @param submat a list [G,C], the vector G corresponds to the submatrix'rows, C corresponds to the submatrix'columns
#' @param Z list of vector of columns CV
#' @return a vector of biclusters (G,C), G bicluster'rows, C bicluster'columns
conquer<-function(E, submat, Z)
{
  # Check if the submatrix is a bicluster
  submatrix <- as.matrix(E[submat[[1]], submat[[2]]])
  
  if(sum(submatrix) == nrow(submatrix)*ncol(submatrix))
  {
    return(submat)
  }

  div <- divide(E, submat, Z)
  
  gu <- div$gu
  gv <- div$gv
  gw <- div$gw
  cu <- div$cu
  cv <- div$cv
  
  mu <- list()
  mv <- list()
  
  if(!is.null(gu))
  {
    mu <- conquer(E, list(union(gu, gw), cu), Z)
  }
  
  if(!is.null(gv) & is.null(gw))
  {
    mv <- conquer(E, list(gv, cv), Z)
  }
  
  else if(!is.null(gw))
  {
    Zprim <- Z
    Zprim[[length(Zprim)+1]] <- cv
    mv <- conquer(E, list(union(gw,gv), union(cu,cv)), Zprim)
  }
  
  return(c(mu,mv))
  
}



#' Divide function of BiMax algorithm
#'
#' @description Find U and V submatrices
#' @param submat a list [G,C], the vector G corresponds to the submatrix'rows, C corresponds to the submatrix'columns
#' @param Z list of vector of columns CV
#' @return a list with GU, GV, GW, CU, CV
divide<-function(E, submat, Z)
{
  Gprim <- reduce(E, submat, Z)
  i <- -1
  
  for(j in Gprim)
  {
    if(sum(E[j, submat[[2]]]) < length(submat[[2]]) & sum(E[j, submat[[2]]]) > 0)
    {
      i <- j
      break
    }
  }
  
  if (i != -1)
  {
    cu <- intersect(which(E[i,]==1), submat[[2]])
  }
  else
  {
    cu <- submat[[2]]
  }
  
  cv <- setdiff(submat[[2]], cu)
  if(length(cv) == 0)
  {
    cv <- c()
  }
  
  gu <- c()
  gv <- c()
  gw <- c()
  
  for(i in Gprim)
  {
    Cstar <- intersect(which(E[i,]==1), submat[[2]])
    
    if(all(Cstar %in% cu))
    {
      gu <- c(gu, i)
    }
    
    else if(all(Cstar %in% cv))
    {
      gv <- c(gv, i)
    }
    
    else
    {
      gw <- c(gw, i)
    }
  }
  
  return(list(gu = gu, gv = gv, gw = gw, cu = cu, cv = cv))
  
}



#' Reduce function of BiMax algorithm
#'
#' @description Index of rows that have at least value 1 among the C columns + Z condition
#' @param submat a list [G,C], the vector G corresponds to the submatrix'rows, C corresponds to the submatrix'columns
#' @param Z list of vector of columns CV
#' @return Gprim variable used in divide function
reduce<-function(E, submat, Z)
{
  Gprim<- c()
  
  # Loop for over the rows in G
  for(i in submat[[1]])
  {
    Cstar <- intersect(which(E[i,]==1), submat[[2]])
    
    
    # if Cstar is empty
    if(length(Cstar) == 0)
    {
      next
    }
    
    add <- TRUE
    
    # If no overlapping
    for(Cplus in Z)
    {
      if(length(intersect(Cstar,Cplus)) == 0)
      {
        add <- FALSE
        break
      }
    }
    
    if(add)
    {
      Gprim <- c(Gprim, i)
    }

  }
  
  return(Gprim)
}