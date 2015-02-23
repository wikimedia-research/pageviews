test_file_location <- system.file("pageviews_dataset.tsv.gz", package = "pageviews")
sampled_log <- read_sampled_log(test_file_location)
context("Test filtering")

test_that("Spiders are identified", {
  agents <- c("msnbot-media/1.1 (+http://search.msn.com/msnbot.htm)",
              "Mozilla/5.0%20(compatible;%20Googlebot/2.1;%20+http://www.google.com/bot.html)",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.31 (KHTML, like Gecko) Chrome/26.0.1410.64 Safari/537.3",
              "Opera/9.80%20(Windows%20NT%206.1;%20WOW64;%20Edition%20Campaign%2021)%20Presto/2.12.388%20Version/12.11")
  expect_that(is_spider(agents), equals(c(TRUE,TRUE,FALSE,FALSE)))
})

test_that("Automata are identified", {
  
})

test_that("Pageview filtering works", {
  filtered_log <- to_pageviews(sampled_log)
  expect_that(nrow(filtered_log), equals(113))
})

test_that("Automata filtering works", {
  agents <- c("Python-urllib/2.7", "WordPress/3.2.1; http://www.greenshinto.com/wp",
              "Mozilla/5.0 (compatible; publiclibraryarchive.org/1.0; +crawl@publiclibraryarchive.org)",
              "WinHTTP Example/1.0", "BlackBerry8520/5.0.0.509 Profile/MIDP-2.1 Configuration/CLDC-1.1")
  expect_that(is_spider(agents), equals(c(TRUE,TRUE,TRUE,TRUE,FALSE)))
  
})