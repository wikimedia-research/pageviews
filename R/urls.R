#'@title normalise and extract referers
#'@description decodes, simplifies and summarises referers
#'in log lines, extracting high-level names (for example, "Google")
#'rather than inconsistently-fragmented URLs.
#'
#'@param referers a vector of referers
#'
#'@return a vector of normalised referers
#'
#'@importFrom urltools domain
#'@export
extract_referers <- function(referers){
  holding <- tolower(domain(referers))
  referers <- character(length(holding))
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

#'@title extract the project from Wikipedia URLs
#'@description take a vector of wikimedia URLs and extract the language or project variant,
#'along with the project class.
#'
#'@param urls a vector of URLs
#'
#'@return a vector of extracted projects, in the form of project_variant.project_class.
#'
#'@export
extract_project <- function(urls){
  hosts <- tolower(domain(urls))
  hosts <- gsub(x = hosts, pattern = "\\.org$", perl = TRUE, useBytes = TRUE, replacement = "")
  hosts <- gsub(x = hosts, pattern = "\\.(m|wap|mobile|zero)\\.", perl = TRUE, useBytes = TRUE, replacement = "")
  return(hosts)
}