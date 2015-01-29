#include <Rcpp.h>
using namespace Rcpp;

std::string extract_origin(std::string x_forwarded_for){
  std::string output;
  size_t comma_loc = x_forwarded_for.find_last_of(",");
  if(comma_loc != std::string::npos){
    std::string holding = x_forwarded_for.substr(comma_loc);
    size_t ip_id_loc = holding.find(".:");
    if(ip_id_loc == std::string::npos){
      x_forwarded_for = extract_origin(x_forwarded_for.substr(0,comma_loc-1));
    } else {
      x_forwarded_for = holding;
    }
  }
  return x_forwarded_for;
}
// [[Rcpp::export]]
std::vector < std::string > normalise_ips(std::vector < std::string > ip_addresses,
                                          std::vector < std::string > x_forwarded_fors){
   
}