#'@title normalise and extract referers
#'@description decodes, simplifies and summarises referers
#'in log lines, extracting high-level names (for example, "Google")
#'rather than inconsistently-fragmented URLs.
#'
#'@param data a data.frame outputted by \code{\link{read_sampled_log}}.
#'
#'@return a data.frame matching the input in structure, but with normalised referers.
#'
#'@importFrom urltools url_decode domain
#'@export
extract_referers <- function(data){
  holding <- domain(url_decode(data$referer))
  referers <- character(nrow(data))
  referers[holding == ""] <- "None"
  referers[fast_grep(holding, "\\.wik(i|t).*\\.org$")] <- "Internal"
  referers[fixed_grep(holding, "google.")] <- "Google"
  referers[fixed_grep(holding, "yahoo.")] <- "Yahoo"
  referers[fixed_grep(holding, "facebook.")] <- "Facebook"
  referers[fixed_grep(holding, "twitter.")] <- "Twitter"
  referers[fixed_grep(holding, "t.co")] <- "Twitter"
  referers[fixed_grep(holding, "bing.")] <- "Bing"
  referers[fixed_grep(holding, "baidu.")] <- "Baidu"
  referers[referers == ""] <- "Other"
  data$referer <- referers
  return(data)
}