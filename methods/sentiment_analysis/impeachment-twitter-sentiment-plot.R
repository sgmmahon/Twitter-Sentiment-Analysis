## install these if not already
pkgs <- c("remotes", "syuzhet", "tfse", "rtweet", "ggplot2", "dplyr", "tidyr")
if (length(pkgs <- pkgs[!pkgs %in% installed.packages("remotes")]) > 0) {
  install.packages(pkgs)
}

## install from github
remotes::install_github("mkearney/dataviz")
remotes::install_github("mkearney/dict")

## create {fastaf} sentiment package
dict::create_dict_pkg(
  dict::dict(syuzhet:::afinn),
  file.path(tempdir(), "fastaf"),
  open = FALSE,
  rstudio = FALSE
)

## load these ones
library(rtweet)
library(tfse)
library(ggplot2)

## search tweets:
## this is similar to the code I used but it will take about 3.5 hours to run–
## and it won't work after Nov 22, 2019(ish)
if (FALSE) {
  ih <- search_tweets(
    "trump OR realdonaldtrump OR impeachment OR president " %P%
      "OR ImpeachingHearings OR ImpeachmentHearings OR ImpeachmentHearing " %P%
      "OR ImpeachmentInquiry OR witness OR hearings OR ukraine OR potus " %P%
      "OR repadamschiff OR corrupt OR corruption OR schiff OR hearing",
    n = 240000,
    max_id = "1195377148485390337",
    retryonratelimit = TRUE,
    include_rts = FALSE
  )
}

## so, intead, I've shared the status IDs, which is a bit faster (and not
## subject to date-based restrictions); read status IDs from Github
download.file("https://ndownloader.figshare.com/files/18741602", tmp <- tempfile())
sids <- tfse::read_RDS(tmp)

## lookup tweets data:
## This should take around 45 mins; use this function to wait between calls
sleeper_fun <- function() Sys.sleep(as.numeric(rate_limit("lookup_tweets")[["reset"]], "secs") + 1)

## note: this won't return information for
## tweets or users have been deleted/deactivated/suspended/etc.
ih1 <- lookup_tweets(sids[000001:090000])
sleeper_fun()
ih2 <- lookup_tweets(sids[090001:180000])
sleeper_fun()
ih3 <- lookup_tweets(sids[180001:238920])

## bind data frames
ih <- dplyr::bind_rows(ih1, ih2, ih3)

## calculate sentiment
ih %>%
  dplyr::mutate(text = gsub("(https?://|#|@)\\S+", "", text)) %>%
  dplyr::pull(text) %>%
  fastaf::score() %>%
  dplyr::bind_cols(i, .) ->
  is

##
capt <- "𝖬𝖺𝖽𝖾 𝗂𝗇 slack-hash𝗋𝗌𝗍𝖺𝗍𝗌 𝖻𝗒:    \n" %P%
  "github𝗆𝗄𝖾𝖺𝗋𝗇𝖾𝗒 twitter𝗄𝖾𝖺𝗋𝗇𝖾𝗒𝗆𝗐"

## aggregate and plot time series
## Note: your computer may not have the same font–change it to whatever you like
## or at least something like "Helvetica Neue"
is %>%
  dplyr::mutate(time = round_time(created_at, "5 secs"),
    time = time - 60 * 60 * 5) %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(pos = sum(positive),
    neg = -sum(negative),
    wc = sum(wc),
    n = dplyr::n()) %>%
  tidyr::gather(sent, score, -time, -wc, -n) %>%
  dplyr::filter(!time %in% range(time)) %>%
  ggplot(aes(x = time, y = score)) +
  annotate("text", x = as.POSIXct("2019-11-15 04:25:30"), y = 0,
    label = paste(rep("〰", 63), collapse = ""), angle = -90) +
  geom_point(aes(fill = sent), alpha = 0.45, shape = 21, size = 2.5) +
  stat_smooth(aes(color = sent), method = "loess", formula = y ~ log(x), span = 0.1) +
  labs(title = "Sentiment of tweets about \"The Impeachment Hearings\"",
    subtitle = "Positive and negative sentiment totals of " %P% nrow(is) %P%
      " tweets using the 'afinn' dictionary and aggregated in five-second intervals",
    x = NULL, y = "Total Sentiment", caption = capt) +
  dataviz::theme_mwk(18, base_family = "Avenir Next LT Pro") +
  scale_x_datetime(labels = function(x) format(x, "%I:%M%P", tz = "UTC")) +
  theme(legend.position = "none",
    plot.title = element_text(size = rel(1.5)),
    plot.caption = element_text(family = "Font Awesome 5 Brands", face = "plain")) +
  scale_color_manual(values = c(neg = "#dd2222", pos = "#2244ee")) +
  scale_fill_manual(values = c(neg = "#dd2222", pos = "#2244ee")) +
  annotate("text", x = as.POSIXct("2019-11-15 04:26:00"), y = 55, hjust = 0, size = 6,
    label = "Schiff reads", family = "Avenir Next LT Pro", fontface = "plain") +
  annotate("text", x = as.POSIXct("2019-11-15 04:26:00"), y = 20, hjust = 0, size = 6,
    label = "Trump's tweet", family = "Avenir Next LT Pro", fontface = "plain") +
  annotate("text", x = as.POSIXct("2019-11-15 04:26:00"), y = -15, hjust = 0, size = 6,
    label = "about Yovanovitch", family = "Avenir Next LT Pro", fontface = "plain") +
  annotate("text", x = as.POSIXct("2019-11-15 04:26:00"), y = -50, hjust = 0, size = 6,
    label = "to Yovanovitch", family = "Avenir Next LT Pro", fontface = "plain") +
  annotate("text", x = as.POSIXct("2019-11-15 04:26:00"), y = -85, hjust = 0, size = 10,
    label = "↵", family = "Avenir Next LT Pro", fontface = "plain") +
  ggsave("sent-impeach-tweets.png", width = 13.5, height = 7.5, units = "in")
