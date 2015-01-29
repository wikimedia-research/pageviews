#Wrappers around grepl optimised for fixed-string pattern-matching and
#fast pattern-matching, respectively.
fixed_grep <- function(field, pattern){
  grepl(x = field, pattern = pattern, fixed = TRUE, useBytes = TRUE)
}
fast_grep <- function(field, pattern){
  grepl(x = field, pattern = pattern, useBytes = TRUE, perl = TRUE)
}