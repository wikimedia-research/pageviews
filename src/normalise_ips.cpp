#include <Rcpp.h>
#include <iostream>
#include <sstream>
using namespace Rcpp;

//Tokenize an XFF header.
std::vector < std::string > tokenize(std::string xff){
  xff.erase(remove_if(xff.begin(), xff.end(), isspace), xff.end());
  std::vector < std::string > output;
  std::string holding;
  std::stringstream strm(xff);
  while(strm.good()){
    getline(strm, holding, ',');
    output.push_back(holding);
  }
  return output;
}

//Check if a substring looks like a real IP
bool is_real_ip(std::string possible_ip){
  size_t ip_match = possible_ip.find_first_of(".:");
  if(ip_match != std::string::npos){
    return true;
  }
  return false;
}

//Combine the two! For each XFF, tokenize it and
//check if it looks like a real IP. Return the first element
//that does. If none do, return the first element.
std::string extract_origin(std::string xff){
  
  std::vector < std::string > holding = tokenize(xff);
  int hold_size = holding.size();
  if(hold_size == 1){
    return holding[0];
  }
  
  for(int i = 0; i < hold_size; i++){
    if(is_real_ip(holding[i])){
      return holding[i];
    }
  }
  return holding.front();
}

//'@title normalise IP addresses for geolocation
//'@description \code{normalise_ips} takes IP addresses and x_forwarded_for
//'values and, in the event that x_forwarded_for is non-null, attempts to
//'extract the "real" IP closest to the client.
//'
//'@param ip_addresses a vector of IP addresses
//'
//'@param x_forwarded_fors an equally-sized vector of XFF values, retrieved
//'from \code{\link{read_sampled_log}}.
//'
//'@return a vector of IP addresses, incorporating the XFF header value
//'where appropriate
//'
//'@export
// [[Rcpp::export]]
std::vector < std::string > normalise_ips(std::vector < std::string > ip_addresses,
                                          std::vector < std::string > x_forwarded_fors){
  if(ip_addresses.size() != x_forwarded_fors.size()){
    throw std::range_error("The two input vectors must be the same length");
  }
  for(int i = 0; i < ip_addresses.size(); i++){
    if(x_forwarded_fors[i] != "-"){
      ip_addresses[i] = extract_origin(x_forwarded_fors[i]);
    }
  }
  return ip_addresses;
}
