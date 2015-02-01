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
  expected_sequence_nos <- c("1735133514", "814422943", "768158599", "806281329", "801147205", 
                             "6231171200", "802067053", "6347723186", "6137838693", "743834974", 
                             "1978180273", "761769385", "50191735", "51975344", "811262407", 
                             "1775797617", "1953957685", "82395333", "65651878", "1947083892", 
                             "37128616", "1989236246", "780996652", "326795064", "20074186", 
                             "779919814", "55357042", "824217008", "1973711559", "108273794", 
                             "1980181408", "6081840151", "784528561", "1715189647", "2004790804")
  expect_that(nrow(filtered_log), equals(35))
  expect_that(sum(as.numeric(filtered_log$sequence_no)), equals(sum(as.numeric(expected_sequence_nos))))
})

test_that("Automata filtering works", {
  agents <- c("Python-urllib/2.7", "WordPress/3.2.1; http://www.greenshinto.com/wp",
              "Mozilla/5.0 (compatible; publiclibraryarchive.org/1.0; +crawl@publiclibraryarchive.org)",
              "WinHTTP Example/1.0", "BlackBerry8520/5.0.0.509 Profile/MIDP-2.1 Configuration/CLDC-1.1")
  expect_that(is_spider(agents), equals(c(TRUE,TRUE,TRUE,TRUE,FALSE)))
  
})