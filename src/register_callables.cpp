// src/register_callables.cpp

// pull in SEXP, wrap(), etc.
#include <Rcpp.h>

// for R_RegisterCCallable, DllInfo, DL_FUNC…
#include <R_ext/Rdynload.h>

// forward declarations of the generated wrappers
extern "C" SEXP _coprimer_first_coprime(SEXP, SEXP, SEXP);
extern "C" SEXP _coprimer_stern_brocot_tree(SEXP);
extern "C" SEXP _coprimer_nearby_coprime(SEXP, SEXP, SEXP);

// this attribute makes Rcpp inject your function into R_init_coprimer()
//
// [[Rcpp::init]]
void register_coprimer_callables(DllInfo* dll) {
  R_RegisterCCallable("coprimer",
                      "first_coprime",
                      (DL_FUNC)&_coprimer_first_coprime);
  R_RegisterCCallable("coprimer",
                      "stern_brocot_tree",
                      (DL_FUNC)&_coprimer_stern_brocot_tree);
  R_RegisterCCallable("coprimer",
                      "nearby_coprime",
                      (DL_FUNC)&_coprimer_nearby_coprime);
}
