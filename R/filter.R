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
#'@seealso \code{\link{read_sampled_log}}, for retrieving the
#'sampled logs in the first place.
#'@examples
#'
#'#Get the latest day's pageviews
#'files <- get_files()
#'data <- to_pageviews(read_sampled_log(files[length(files)]))
#'
#'@importFrom urltools url_decode
#'@export
to_pageviews <- function(data){
  data <- data[data$mime_type %in% c("text/html; charset=iso-8859-1",
                                     "text/html; charset=ISO-8859-1",
                                     "text/html",
                                     "text/html; charset=utf-8",
                                     "text/html; charset=UTF-8",
                                     "application/json; charset=utf-8"),]
  data <- data[fast_grep(data$status_code, "(200|304)"),]
  data$url <- url_decode(data$url)
  data <- data[fast_grep(data$url, paste0("((commons|meta|incubator|species)\\.((m|mobile|wap|zero)\\.)?wikimedia|",
                                           "(wik(ibooks|idata|inews|ipedia|iquote|isource|tionary|iversity|ivoyage)))",
                                           "\\.org")),]
  
  data <- data[fast_grep(data$url, "(/sr(-(ec|el))?|\\?((cur|old)id|title)=|/w(iki)?/|/zh(-(cn|hans|hant|hk|mo|my|sg|tw))?/)"),]
  data <- data[!fast_grep(data$url, "(BannerRandom|CentralAutoLogin|MobileEditor|Undefined|UserLogin|ZeroRatedMobileAccess)"),]
  
  is_api <- fixed_grep(data$url, "api.php")
  data <- rbind(data[!is_api,], is_app_pageview(data[is_api,]))
  return(data)
}

#'@title identify the access method associated with a request
#'@description identify whether the request is via desktop, the mobile web
#'or the mobile app. Assumes pageview filtering has been applied.
#'
#'@param urls a vector of URLs from \code{\link{read_sampled_log}}.
#'
#'@return a character vector containing "mobile web", "mobile app"
#'or "desktop" for each input URL, as appropriate.
#'
#'@examples
#'files <- get_files()
#'pageviews <- to_pageviews(read_sampled_log(files[length(files)]))
#'pageviews$access_method <- identify_access_method(pageviews$url)
#'
#'@seealso \code{\link{to_pageviews}} to filter to pageviews in
#'the first place.
#'
#'@export
identify_access_method <- function(urls){
  output <- character(length(urls))
  is_mobile <- fast_grep(urls, "\\.(m|mobile|wap|zero)\\.")
  is_app <- fixed_grep(urls, "api.php")
  output[is_mobile] <- "mobile web"
  output[is_app] <- "mobile app"
  output[output == ""] <- "desktop"
  return(output)
}

#'@title check whether a request is from a spider
#'@description consumes a vector of user agents and checks whether each one
#'matches the ua-parser spider definitions. In addition, it looks for Wikimedia-specific
#'spiders that aren't included in the (generalised) spider list.
#'
#'@param user_agents a vector of user agents, which can be retrieved with
#'\code{\link{read_sampled_log}}.
#'
#'@return a boolean vector identifying whether the user agent at the equivalent indices
#'in the input vector matched that of a spider/web crawler or not.
#'
#'@seealso \code{\link{read_sampled_log}} for retrieving user agents, and
#'\code{\link{is_automata}} for identifying non-crawler automata.
#'
#'@export
is_spider <- function(user_agents){
  is_mobile_spider <- fast_grep(user_agents,
                                paste0("((Bot|Yeti)-Mobile|YRSpider|AdsBot-Google-Mobile|(jump|google|Wukong)bot",
                                       "YahooSeeker)"))
  is_generic_spider <- fast_grep(user_agents,
                                 paste0("(bot|zao|borg|DBot|oegp|silk|Xenu|zeal|^NING|CCBot|crawl|htdig|lycos|slurp|",
                                        "teoma|voila|yahoo|Sogou|CiBra|Nutch|^Java/|^JNLP/|Daumoa|Genieo|ichiro|larbin|",
                                        "pompos|Scrapy|snappy|speedy|spider|msnbot|msrbot|vortex|^vortex|crawler|",
                                        "favicon|indexer|Riddler|scooter|scraper|scrubby|WhatWeb|WinHTTP|bingbot|",
                                        "openbot|gigabot|furlbot|polybot|seekbot|^voyager|archiver|Icarus6j|mogimogi|",
                                        "Netvibes|blitzbot|altavista|charlotte|findlinks|Retreiver|TLSProber|WordPress|",
                                        "SeznamBot|ProoXiBot|wsr\\-agent|Squrl Java|EtaoSpider|PaperLiBot|SputnikBot|",
                                        "A6\\-Indexer|netresearch|searchsight|baiduspider|YisouSpider|ICC\\-Crawler|",
                                        "http%20client|Python-urllib|dataparksearch|converacrawler|Screaming Frog|",
                                        "YahooCacheSystem|fast\\-webcrawler|Sogou Pic Spider|Spider Build|",
                                        "semanticdiscovery|Innovazion Crawler|facebookexternalhit|BegunAdvertising|",
                                        "redditbot|changedetection|pic scraper|",
                                        "Google.*/\\+/web/snippet|Google-HTTP-Java-Client|crawler4j|results module)"))
  is_wikimedia_spider <- fast_grep(user_agents,
                                   paste0("(goo wikipedia|MediaWikiCrawler-Google|wikiwix-bot)"))
  return(is_mobile_spider | is_generic_spider | is_wikimedia_spider)
}

#'@title identify non-webcrawler automated traffic
#'
#'@description Not all automated traffic is from a webcrawler - much is from people running HTTP libraries
#'in a particularly stupid, selfish and lazy fashion (if you're reading this and you've ever had a service
#'making requests with the user agent "Twisted PageGetter": this means you). \code{is_automata} identifies
#'this class of traffic.
#'
#'@param user_agents a vector of user agents, which can be retrieved with
#'\code{\link{read_sampled_log}}.
#'
#'@return a boolean vector identifying whether the user agent at the equivalent indices
#'in the input vector matched that of an automated service or not.
#'
#'@seealso \code{\link{read_sampled_log}} for retrieving user agents, and
#'\code{\link{is_automata}} for identifying non-crawler automata.
#'
#'@export
is_automata <- function(user_agents){
  automata <- fast_grep(user_agents,
                        paste0("(AppEngine-Google|Pywiki(pedia)?bot|GetWiki|Google-HTTP-Java-Client)"))
  blank_automata <- fast_grep(user_agents,
                              paste0("^((Java|WordPress)/|Python-urllib|WinHTTP)"))
}