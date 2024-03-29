// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// normalise_ips
std::vector < std::string > normalise_ips(std::vector < std::string > ip_addresses, std::vector < std::string > x_forwarded_fors);
RcppExport SEXP pageviews_normalise_ips(SEXP ip_addressesSEXP, SEXP x_forwarded_forsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::vector < std::string > >::type ip_addresses(ip_addressesSEXP);
    Rcpp::traits::input_parameter< std::vector < std::string > >::type x_forwarded_fors(x_forwarded_forsSEXP);
    __result = Rcpp::wrap(normalise_ips(ip_addresses, x_forwarded_fors));
    return __result;
END_RCPP
}
