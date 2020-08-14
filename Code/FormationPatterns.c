#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP FormationPatterns(SEXP RTotal, SEXP Rdimension, SEXP Rdelay){

	int n, i, ii, j, jj, init, total, dimension, delay, a;	

	RTotal = coerceVector(RTotal, INTSXP);
	Rdimension = coerceVector(Rdimension, INTSXP);
	Rdelay = coerceVector(Rdelay, INTSXP);

        init = 1;
        total = INTEGER(RTotal)[0];
        dimension = INTEGER(Rdimension)[0];
        delay = INTEGER(Rdelay)[0];

	n = total - ((dimension - 1)*delay);

         //allocates space for the bidimensional arrays(indexes)
	 j = 1;
	 SEXP Rindexes = PROTECT(allocMatrix(INTSXP, dimension, n));
	 for(i = 0; i < n; i++){
		jj = 0;
		for(ii = j-1; ii < (j + ((dimension - 1)*delay)); ii = ii + delay){
			INTEGER(Rindexes)[jj + (dimension)* i] = ii;
			//printf("Posição: %d, Valor: %d, inicio: %d, fim: %d\n", jj + (dimension)* i, ii, j-1, j + ((dimension - 1)*delay) - 1);
			jj = jj + 1;
		}	
		j = j + 1;
	 }

	 UNPROTECT(1);

	 return Rindexes;

}
