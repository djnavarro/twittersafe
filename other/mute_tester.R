library(twittersafe)
library(rtweet)
library(dplyr)

stock_follows <- get_followers("docstockk", n = 100000, retryonratelimit = TRUE)
my_mutes <- get_mutes(n = 100000)

to_mute <- anti_join(stock_follows, my_mutes, by = "user_id")

mute_users(users = to_mute)
