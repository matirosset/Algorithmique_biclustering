source("src/iBBiG.cpp")


calculaterowScore <- function(colVector,binaryMatrix,alpha){
  ##calculates the rowScores for each term
  colSize = sum(colVector)
  currentCluster = binaryMatrix[,colVector==1]
  pScore = rowSums(currentCluster)
  p1 = pScore/colSize # proportion de 1 par ligne
  p0 = 1-p1
  entropy = -p1*log2(p1) - p0*log2(p0)
  entropy[is.na(entropy)] = 0
  eScore = 1-entropy
  pScore[p1<0.5] = 0
  score = pScore*( eScore**alpha )
  return(score)
}

removeRowInformation<-function(rowScore,colVector,alpha){
  ##weights the pValues in the covariate matrix, that produced the score for the
  ##module. the weight is 1 minus the entropy part of the score
  colSize<-sum(colVector)
  currentCluster<-rowScore[colVector==1]
  pScore<-sum(currentCluster)
  p1<-pScore/colSize
  if (p1>=0.5){
     p0<-1-p1
     entropy<--p1*log2(p1)-p0*log2(p0)
     entropy[is.na(entropy)]<-0
     weight<-1-(1-entropy)^alpha
     rowScore[colVector==1]<-rowScore[colVector==1]*weight
   }
   return(rowScore)
}

removeInformation<-function(colVector,binaryMatrix,alpha){
   ##weights the used rows and thereby removes the information
   ##that was used in the top cluster

   binaryMatrix<-apply(binaryMatrix,1,removeRowInformation,colVector,alpha)
   binaryMatrix<-t(binaryMatrix)
   return(binaryMatrix)
}
    

linearRanking<-function(pos,nInd,SP){
   ##determine the selection probability (linear ranking) 2-SP+2*(SP-1)*(Pos-1)/(Nind-1)
   probab<-2-SP+2*(SP-1)*(pos-1)/(nInd-1)
   return(probab)
}       


getCumProbabilities<-function(probabilities){
   ##calculates the cummulated propabilites   
   probabilities<-probabilities/sum(probabilities)
   for (i in 2:length(probabilities)){
      probabilities[i]<-probabilities[i]+probabilities[i-1]
   }
   return(probabilities)
}       


iBBiG<-function(binaryMatrix, 
                nbClust,     
                alpha=0.3,
                pop_size=100,
                mutation=0.08,
                stagnation=50,
                selection_pressure=1.2,
                max_sp=15,
                success_ratio=0.6){

   #save the raw data for later
   rawMatrix<-binaryMatrix
   
   selectionP<-sapply(1:pop_size,linearRanking,pop_size,selection_pressure)
   sp<-getCumProbabilities(selectionP)

   clusterScores<-numeric(0)

   ## Number of rows (Sigs) and columns (Covariates)
   colSize<-ncol(binaryMatrix)
   rowSize<-nrow(binaryMatrix)

   for (i in 1:nbClust){
      colVector<-rep(0,colSize)
         
      # c++ function call
      colVector <- clusterCovsCpp(as.double (binaryMatrix), colVector,
                as.integer(colSize),
                as.integer(rowSize),
                as.double (alpha),
                as.integer(pop_size),
                as.integer(stagnation),
                as.double (mutation),
                as.double (success_ratio),
                as.integer(max_sp),
                as.double (sp))

	  if (sum(colVector)==0){
           rowScore<-rep(FALSE,rowSize)
           rowVector<-rowScore
           clusterScore<-0
       }
	  else{
      #calculate row scores
      rowScore<-calculaterowScore(colVector,binaryMatrix,alpha)
      rowVector<-rowScore>0
      clusterScore<-sum(rowScore)
	    #remove information from binaryMatrix that was used in this cluster
      binaryMatrix<-removeInformation(colVector,binaryMatrix,alpha)    
	   }
	   BC = list(
      "row" = rowVector,
      "col" = colVector
    )
    print(BC)
    res[[i]] = BC
    i = i+1
   }
   return(res)
}
