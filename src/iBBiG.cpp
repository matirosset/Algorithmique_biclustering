#include <cmath>
#include <ctime>
#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object


//scoring function (entropy based score)
// [[Rcpp::export]] //mandatory to export the function
double getScore(int *positions, int actualCovs, double *covMat, int noCovs, int noSigs, double alpha){
  double score=0;
  int i,j;
  if (actualCovs>1){
    double pScore,p1,eScore;
    int covIndex;
    for (i=0;i<noSigs;i++){
      pScore=0;
      for (j=0;j<actualCovs;j++){
        covIndex=positions[j];
	      pScore+=covMat[covIndex*noSigs+i];
    	}
      p1=pScore/actualCovs;
      if (p1>0.5){
        if (p1==1){ eScore=1; }
        else{ eScore=1+p1*log2(p1)+(1-p1)*log2(1-p1);	}
        score+=pScore*pow(eScore,alpha);
      }
    }
  }
  return(score);
}

// [[Rcpp::export]] //mandatory to export the function
void initializePop(long int *pop, int noPop, double *covMat, int noCovs, int noSigs, double alpha){
   int index=0;
   int r1,r2,i,j;
   //initialize population with zeros
   for (i=0;i<noCovs;i++){
     for (j=0;j<noPop;j++){
       pop[i*noPop+j]=0;
  	 }
   }
   double score;
   int actualCovs;
   int x=0;
   while (x<(noPop*100) && index<noPop){
      actualCovs=2;
	    r1 = rand()%noCovs;
      r2 = rand()%noCovs;
      int positions[2]={r1,r2};
      if (r1==r2){ actualCovs=1; }
      score=getScore(positions,actualCovs,covMat,noCovs,noSigs,alpha);
     // printf("%f %d %d %d\n",score,r1,r2,actualCovs);
      if (score>0){
         pop[r1*noPop+index]=1;
         pop[r2*noPop+index]=1;
         index++;
      }
      x++;
   }
}


// [[Rcpp::export]] //mandatory to export the function
void mysort(double* scores, int* order, int N){
  int i, j, t1;
  double v, t;

  if(N<=1) return;

  // Partition elements
  v = scores[0];
  i = 0;
  j = N;
  for(;;){
    while(scores[++i] < v && i < N) { }
    while(scores[--j] > v) { }
    if(i >= j) break;
    t = scores[i]; scores[i] = scores[j]; scores[j] = t; // échange des 2 éléments j
    t1 = order[i]; order[i] = order[j]; order[j] = t1;
  }
  t = scores[i-1]; scores[i-1] = scores[0]; scores[0] = t;
  t1 = order[i-1]; order[i-1] = order[0]; order[0] = t1;
  mysort(scores, order, i-1);
  mysort(scores+i, order+i,  N-i);
}
// Trier les individus de la populations selon leurs scores
void sortPop(long int *pop,int noCovs,int noPop,double *scores){
   int i,j;
   int order[noPop];
   for (i=0;i<noPop;i++){ order[i]=i; }
   mysort(scores,order,noPop);
   int sortedPop[noCovs*noPop];
   for (i=0;i<noPop;i++){
      for (j=0;j<noCovs;j++){
         sortedPop[j*noPop+i]=pop[j*noPop+order[i]];
      }
   }
   // On remet la population ordonnée dans la liste de départ (pop)
   for (i=0;i<noPop*noCovs;++i){
   	  pop[i]=sortedPop[i];
   }
}


//parent selection
// [[Rcpp::export]] //mandatory to export the function
int selectParent(double *SP){
   double r=rand(); r/=RAND_MAX; // on génère une probabilité
   int index=0;
   while(SP[index]<r){ index++; }
   return(index); // tant qu'on est en dessous de cette proba, on continue -> nb d'ind en dessous de cette proba
}

//generates the next generation of children
// [[Rcpp::export]] //mandatory to export the function
void generateChildren(
long int *pop, double *covMat, double *scores,
int noPop, int noCov, int noSigs,
double SR, int maxSP, double *SP,
double mutation,double alpha){

   //new population
   double newScores[noPop];
   long int newPop[noPop*noCov];

   // the strongest individual from the old is included - one elitism
   newScores[0]=scores[noPop-1];
   int i;
   for (i=0;i<noCov;i++){ newPop[i*noPop]=pop[(i+1)*noPop-1]; }

   //population pool for new generation (children that do not outperform at least one of their parents)
   double poolScores[maxSP*noPop];
   void *poolPop[maxSP*noPop]; // poite vers un malloc

   // long int poolPop[*noCov];

   int poolIndex=0;
   int par1,par2,actualCovs,crosspoint;
   double childScore;
   int child[noCov];
   int positions[noCov];
   double pScore,p1,eScore;
   int threshold=RAND_MAX/noCov;
   int mutThreshold=(double)RAND_MAX*mutation;

   int index=1;
   int j;
   int sucChildren=1;
   while ( (index<noPop)||(sucChildren<SR*noPop && index<(maxSP*noPop-1)) ){
      index++;

      //parent selection
      par1=selectParent(SP);
      par2=selectParent(SP);

      actualCovs=0;

      //crossover & mutation
      crosspoint=rand()%(noCov-2)+1; // rand entre 1 et noCov-1
      if (rand()<=mutThreshold){ //mutation
         for (i=0;i<crosspoint;i++){
           child[i]=pop[i*noPop+par1]; // prend le parent 1 jusqu'au crosspoint
           if (rand()<=threshold){
             child[i]=(child[i]+1)%2; // bit inversion
           }
           if (child[i]==1){
             positions[actualCovs]=i;
             actualCovs++;
           }
         }
         while(i<noCov){
           child[i]=pop[i*noPop+par2]; // prend le parent 2 à partir du crosspoint
           if (rand()<=threshold){
             child[i]=(child[i]+1)%2; // bit inversion
           }
           if (child[i]==1){
             positions[actualCovs]=i;
	           actualCovs++;
           }
           i++;
         }
      }
      else{ //no mutation
        for (i=0;i<crosspoint;i++){
          child[i]=pop[i*noPop+par1];
          if (child[i]==1){
            positions[actualCovs]=i;
	          actualCovs++;
          }
        }
        while(i<noCov){
          child[i]=pop[i*noPop+par2];
          if (child[i]==1){
            positions[actualCovs]=i;
	          actualCovs++;
          }
          i++;
        }
    }

    //calculate fitness score
    childScore=0;
    if (actualCovs>1){
      for (i=0;i<noSigs;i++){
        pScore=0;
 	      for (j=0;j<actualCovs;j++){
          pScore+=covMat[positions[j]*noSigs+i];
    	  }
    	  p1=pScore/actualCovs;
        if (p1>0.5){
          if (p1==1){ eScore=1; }
          else{ eScore=1+p1*log2(p1)+(1-p1)*log2(1-p1); }
		      childScore+=pScore*pow(eScore,alpha);
        }
      }
    }

    //child is better than the worse parent -> directly into next generation
    if (childScore>scores[par1] || childScore>scores[par2]){
      newScores[sucChildren]=childScore;
		  for (i=0;i<noCov;i++){ newPop[i*noPop+sucChildren]=child[i]; }
      sucChildren++;
    }
    else if (childScore>0){ //child is not exactly brilliant but has a score >0 -> into the pool
      poolPop[poolIndex] = malloc(sizeof(int) * noCov);
      poolScores[poolIndex]=childScore;
      for (i=0;i<noCov;i++){ ((int*)poolPop[poolIndex])[i]=child[i]; }//poolPop[i*maxSP*noPop+poolIndex]=child[i];
      poolIndex++;
    }
    else{ index--; } // score = 0 (homogene) -> la boucle while qui vient dêtre faite compte pour du beurre
  } // fin while

   //the rest of the pool is filled up with unsuccessful children
   if (sucChildren<noPop){
   	  int cMissing=noPop-sucChildren;
    // on ajoute les childrens de pool pour remplir -> arriver à sucChildren=noPop
 	  for (i=0;i<cMissing;i++){
      for (j=0;j<noCov;j++){ //newPop[noPop*j+sucChildren]=poolPop[maxSP*noPop*j+i];
        newPop[noPop*j+sucChildren]=((int*)poolPop[i])[j];
        newScores[sucChildren]=poolScores[i];
      }
     	sucChildren++;
    }
   }
   // On stocke la nouvelle génération dans les variables de méméoirs
   for (i=0;i<noPop*noCov;i++){ pop[i]=newPop[i]; }
   for (i=0;i<noPop;i++){ scores[i]=newScores[i]; }
   // fin malloc
   for (i=0;i<poolIndex;i++){ free(poolPop[i]); }
}

// [[Rcpp::export]] //mandatory to export the function
NumericVector clusterCovsC(double *covMat, NumericVector group, int *noCovs, int *noSigs,
  double *alpha, int *noPop, int *maxStag, double *mutation, double *SR, int *max_SP, double *SP){

    srand((unsigned)time(NULL));
    long int pop[(*noCovs)*(*noPop)];
    initializePop(pop,*noPop,covMat,*noCovs,*noSigs,*alpha);

    double scores[*noPop];
    int i,j,actualCovs;
    int positions[*noCovs];

    // mesure des scores pour chaque individu
  	for (j=0;j<*noPop;j++){
      actualCovs=0;
      for (i=0;i<*noCovs;i++){
        if (pop[i*(*noPop)+j]==1){
          positions[actualCovs]=i;
          ++actualCovs;
        }
      }
      scores[j]=getScore(positions,actualCovs,covMat,*noCovs,*noSigs,*alpha);
    }

    //current maximum score
    double maxScore=0;
    //counter for stagnation
    int stag=0;

    sortPop(pop,*noCovs,*noPop,scores);

    //run generations for genetic algorithm
    while (stag<*maxStag) {

      // selection, fecondation (avec ou sans mutation -> les enfants dans pop & scores)
      generateChildren(pop,covMat,scores,*noPop,*noCovs,*noSigs,*SR,*max_SP, SP,*mutation,*alpha);

      // tri
      sortPop(pop,*noCovs,*noPop,scores);

	    actualCovs=0;
	    for (i=0;i<*noCovs;++i){
        if (pop[(*noPop-1)*i]==1){ ++actualCovs; }
    	}
      //stop criterion, stagner pendant stag generation (pas de maxscore battu)
      if (scores[*noPop-1]>maxScore){
        stag=0;
        maxScore=scores[*noPop-1];
      }
      else{ stag++; }

    }
    //extract grouping with maximum score
    for (i=0;i<*noCovs;i++){ group[i]=pop[(i+1)*(*noPop)-1]; }
    return group
}
