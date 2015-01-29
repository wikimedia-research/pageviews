fixed_grep <- function(field, pattern){
  grepl(x = field, pattern = pattern, fixed = TRUE, useBytes = TRUE)
}

fast_grep <- function(field, pattern){
  grepl(x = field, pattern = pattern, useBytes = TRUE, perl = TRUE)
}

#Is it an app pageview?
is_app_pageview <- function(x){
  is_app <- fixed_grep(x$user_agent, "WikipediaApp")
  is_pv <- fixed_grep(x$user_agent, "sections=0")
  return(x[is_app & is_pv,])
}

#'@title filter a sampled log file to pageviews
#'
#'@description \code{is_pageview} accepts a sampled log file, read
#'with \code{\link{read_sampled_log}}, and filters it to those rows
#'that meet the existing definition of "pageview", which is described
#'\href{https://meta.wikimedia.org/wiki/Research:Page_view}{here}.
#'
#'@param data a data.frame returned from \code{\link{read_sampled_log}}.
#'
#'@return a data.frame with the same attributes as that of \code{\link{read_sampled_log}},
#'but only containing "pageviews".
#'
#'@export
to_pageviews <- function(data){
  data <- data[data$mime_type %in% c("text/html; charset=iso-8859-1",
                                     "text/html; charset=ISO-8859-1",
                                     "text/html",
                                     "text/html; charset=utf-8",
                                     "text/html; charset=UTF-8",
                                     "application/json; charset=utf-8"),]
  data <- data[fast_grep(data$status_code, "(200|304)"),]
  data <- data[fast_grepl(data$url, paste0("((commons|meta|incubator|species)\\.((m|mobile|wap|zero)\\.)?wikimedia|",
                                           "(wik(ibooks|idata|inews|ipedia|iquote|isource|tionary|iversity|ivoyage)))",
                                           "\\.org")),]
  data <- data[fast_grepl(data$url, paste0("((/sr(-(ec|el))?|/w(iki)?/|/zh(-(cn|",
                                           "hans|hant|hk|mo|my|sg|tw))?)/|\\?((cur",
                                           "|old)id|title)=)")),]
  data <- data[!fast_grepl(data$url, paste0("(BannerRandom|CentralAutoLogin|MobileEditor",
                                            "|Undefined|UserLogin|ZeroRatedMobileAccess)")),]
  is_api <- fixed_grepl(data$url, "api.php")
  data <- rbind(data[!is_api,], is_app_pageview(data[is_api,]))
  return(data)
}