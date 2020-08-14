#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP TransitionGraph(SEXP RWedding, SEXP Rdimension, SEXP Rsize){

	int i, j, dimFat = 1, dimension, size, n, a, b;	
    	int* wedding_aux;
    	SEXP Rgraph;

	RWedding = coerceVector(RWedding, INTSXP);
	Rdimension = coerceVector(Rdimension, INTSXP);
	Rsize = coerceVector(Rsize, INTSXP);

        dimension = INTEGER(Rdimension)[0];
	size = INTEGER(Rsize)[0];
	wedding_aux = INTEGER(RWedding);

	n = dimension;    
        while(1 < n)
	{
		dimFat = n*dimFat;
		n--;
	}
	

	//allocates space for the arrays
	double** graph = (double**) malloc(dimFat * sizeof(double*));
	for(i = 0; i < dimFat; i++){
		graph[i] = (double*) malloc(dimFat * sizeof(double));
	}	
	
	PROTECT(Rgraph = allocVector(REALSXP, (dimFat*dimFat)));
    	for(i = 0; i < (dimFat*dimFat); i++){
        	REAL(Rgraph)[i] = 0;
    	}


	//filling the bidimensional arrays
	for(i = 0; i < dimFat; i++){
		for(j = 0; j < dimFat; j++){
			graph[i][j] = 0;
		}
	}

	for(i = 0; i < (size-1); i++){
		a = wedding_aux[i];
		b = wedding_aux[i+1];
   		graph[a-1][b-1] = graph[a-1][b-1] + 1;
  	}

	for(i = 0; i < dimFat; i++){
		for(j = 0; j < dimFat; j++){
			graph[i][j] = graph[i][j]/(size - 1);
		}
	}
	
	for(i = 0; i < dimFat; i++){
		for(j = 0; j < dimFat; j++){
			REAL(Rgraph)[i + dimFat*j] = graph[i][j];
			//printf("Posição: %d valor: %f \n", i + dimFat*j,  graph[i][j]);
		}
	}
	
	UNPROTECT(1);

	return Rgraph;

}
