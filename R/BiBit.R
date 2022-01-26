
product <- function(row1, row2){ return (row1 & row2) }

should_we_add_row <- function(rho, row){
  p = product(rho, row)
  return ( !sum(!( p == rho )) )
}

#' bibit algorithm
#' @description Executing bibit algorithm
#' @param matrix_binary a binary matrix with values 0 and 1
#' @return list of biclusters (row and columns indexes)
bibit_R <- function(matrix_binary){
  n = dim(matrix_binary)[1]
  p = dim(matrix_binary)[2]
  
  res = list()
  row_not_already_biclustered = rep(T,n)
  col_not_already_biclustered = rep(T,p)
  k = 1
  
  while (sum(row_not_already_biclustered) + sum(col_not_already_biclustered) > 0) {
    BCij_col = numeric(0)
    BCij_row = numeric(0)
    print(col_not_already_biclustered)
    print(row_not_already_biclustered)
    ij = sample(which(row_not_already_biclustered),2) # sans remplacement
    rhoij = product(matrix_binary[ij[1],col_not_already_biclustered],matrix_binary[ij[2],col_not_already_biclustered])
    while (sum(rhoij)==0) {
      ij = sample(which(row_not_already_biclustered),2) # sans remplacement
      rhoij = product(matrix_binary[ij[1],col_not_already_biclustered],matrix_binary[ij[2],col_not_already_biclustered])
    }
    print(ij)
    print(rhoij)
    
    BCij_row = c(BCij_row, ij)
    BCij_col = which(col_not_already_biclustered)[which(rhoij)]
    
    row_not_already_biclustered[ij] = F
    for (indxrow in which(row_not_already_biclustered)){
      row = matrix_binary[indxrow,col_not_already_biclustered]
      if (should_we_add_row(rhoij, row)){ 
        BCij_row = c(BCij_row, indxrow)
        row_not_already_biclustered[indxrow] = F
      }
      
    }
    col_not_already_biclustered[BCij_col] = F
    BCij = list("row" = sort(BCij_row), "col" = sort(BCij_col))
    res[[k]] = BCij
    k = k+1
  }
  return(res)
}
