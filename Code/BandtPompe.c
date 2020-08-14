#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


int indx;

void insertionSort(int* patterns, double* elements, int n){
    int i, j, key_patterns;
    double key_elements;
    for (i = 1; i < n; i++) {
        key_elements = elements[i];
        key_patterns = patterns[i];
        j = i - 1;
        while(j >= 0 && elements[j] > key_elements) {
            elements[j + 1] = elements[j];
            patterns[j + 1] = patterns[j];
            j = j - 1;
        }
        elements[j + 1] = key_elements;
        patterns[j + 1] = key_patterns;
    }
}

//Given an array, it returns all possible permutations of it in another array
void permute(int* element, int init, int end, int* array) { 
    
    int i, aux;

    if(init == end){
        for(i = 0; i <= end; i++){ 
            array[indx] = element[i];
            indx++;
        }   
    }

    else{ 

        for (i = init; i <= end; i++) { 
            aux = element[init];
            element[init] = element[i];
            element[i] = aux;
            permute(element, init + 1, end, array); 
            aux = element[init];
            element[init] = element[i];
            element[i] = aux;
        } 
    } 
}

//Given a pattern, searches for its symbolization
int equals(int* pattern, int*symbols, int size){

    int i, j, aux = 0;
    for(i = 0; i < size; i++){
        if(pattern[i] == symbols[i]){
            aux++;
        }
    }
    if(aux == size){
        return 1;
    }
    else{
        return 0 ;
    }
}

SEXP BandtPompe(SEXP Relements, SEXP Rdimension, SEXP Relementsize){

    int i, j, k = 0, dimFat = 1, n, aux = 0, dimension, elementsize;
    double* elements_aux;
    SEXP Rprobability;

    Rdimension = coerceVector(Rdimension, INTSXP);
    Relements = coerceVector(Relements, REALSXP);
    Relementsize = coerceVector(Relementsize, INTSXP);

    dimension = INTEGER(Rdimension)[0];
    elementsize = INTEGER(Relementsize)[0];
    n = dimension;    

    elements_aux = REAL(Relements);
    
    
    /*aux = 0;
    for(j = 0; j < elementsize*n; j++){
	printf("%f ", elements_aux[aux]);
	aux++;
    }
    printf("\n");*/

    while(1 < n)
    {
        dimFat = n*dimFat;
        n--;
    }

    //allocates space for the bidimensional arrays(symbols, elements, patterns)
    int** symbols = (int**) malloc(dimFat * sizeof(int*));
    for(i = 0; i < dimFat; i++){
        symbols[i] = (int*) malloc(dimension * sizeof(int));
    }

    int **patterns = (int**) malloc(elementsize * sizeof(int*));
    double **elements = (double**) malloc(elementsize * sizeof(double*));
    for(i = 0; i < elementsize; i++){
        patterns[i] = (int*) malloc(dimension * sizeof(int));
        elements[i] = (double*) malloc(dimension * sizeof(double));
    }

    //filling the bidimensional arrays
    int* initial_pattern = (int*) malloc(dimension * sizeof(int));
    int* permutations = (int*) malloc((dimFat*dimension) * sizeof(int));

    for(i = 0; i < dimension; i++){
        initial_pattern[i] = i;
    }

    for(i = 0; i < elementsize; i++){
        for(j = 0; j < dimension; j++){
            patterns[i][j] = initial_pattern[j];
        }
    }

    indx = 0;
    permute(initial_pattern, 0, dimension - 1, permutations);
    indx = 0;

    for(i = 0; i < dimFat; i++){
        for(j = 0; j < dimension; j++){
            symbols[i][j] = permutations[k];
            k++;
        }
    }

    aux = -1;
    for(i = 0; i < elementsize; i++){
        aux=i;
        for(j = 0; j < dimension; j++){
            elements[i][j] = elements_aux[aux];
            //printf("aux: %d element: %f\n", aux, elements_aux[aux]);
            aux+=elementsize;
        }
	//printf("%f %f %f\n", elements[i][0], elements[i][1], elements[i][2]);
	//printf("%f %f %f %f %f %f\n", elements[i][0], elements[i][1], elements[i][2], elements[i][3], elements[i][4], elements[i][5]);
    }
    
    for(i = 0; i < elementsize; i++){
       int key_patterns;
       double key_elements;
       for(k = 1; k < dimension; k++) {
		key_elements = elements[i][k];
		key_patterns = patterns[i][k];
		j = k - 1;
		while(j >= 0 && elements[i][j] > key_elements) {
		    elements[i][j + 1] = elements[i][j];
		    patterns[i][j + 1] = patterns[i][j];
		    j = j - 1;
		}
		elements[i][j + 1] = key_elements;
		patterns[i][j + 1] = key_patterns;
      }
    }
    
    /*
    for(i = 0; i < elementsize; i++){
            printf("%d %d %d\n", patterns[i][0], patterns[i][1], patterns[i][2]);
            //printf("%d %d %d %d %d %d\n", patterns[i][0], patterns[i][1], patterns[i][2], patterns[i][3], patterns[i][4], patterns[i][5]);
    }*/

    
    //Probability Distribution
    PROTECT(Rprobability = allocVector(REALSXP, dimFat));
    for(i = 0; i < dimFat; i++){
        REAL(Rprobability)[i] = 0;
    }

    for(i = 0; i < elementsize; i++){
        for(j = 0; j < dimFat; j++){
            if(equals(patterns[i], symbols[j], dimension)){
                REAL(Rprobability)[j]++;
            }
        }
    }

    for(i = 0; i < dimFat; i++){
        REAL(Rprobability)[i] = REAL(Rprobability)[i]/elementsize;
        //printf("%f\n", REAL(Rprobability)[i]);
    
    }

    UNPROTECT(1);

    return Rprobability;

}
