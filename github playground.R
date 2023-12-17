getwd(
)

# On a R project, only ever use relative paths, not absolute paths.

# Absolute paths: the ones we have used.
#   E.g., /Users/bchoe/Documents/websites/danl-website-template/posts/starwars/starwars_df.qmd

# Relative paths: the location of a file relative to the current working directory
#   E.g., /posts/starwars/starwars_df.qmd

library(tidyverse)
ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
ggsave("diamonds.png") # to save ggplot as a png file.
write_csv(diamonds, "diamonds.csv") # to save data.frame as a csv file


# There is a learning curve, 
# so we use only three commands---add, commit, and push!

install.packages('ggthemes')
install.packages("hrbrthemes")

data <- data %>% 
  mutate(LastCloseLag = lag(`Close/Last`),
         Chg_Close = LastCloseLag - `Close/Last`)

getwd()


# Stocks Manipulation ------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)


Stocks <- read.csv("stocks.csv")


StocksManip <- StocksUpdated %>% 
  mutate(CL_Change =Close.Last - lag(Close.Last))

StocksManip <- StocksManip %>% 
  select(Company,Date,CL_Change)

StocksManip <- StocksManip %>% 
  group_by(Company) %>% 
  arrange(Date) %>% 
  pivot_wider(names_from = Company, values_from = CL_Change)

summary(StocksManip)

StocksMAD <- StocksManip %>% 
  mutate(CSCO_mad = mean(CSCO) - CSCO,
         QCOM_mad = mean(QCOM) - QCOM,
         META_mad = mean(META) - META,
         AMZN_mad = mean(AMZN) - AMZN,
         TSLA_mad = mean(TSLA) - TSLA,
         AMD_mad = mean(AMD) - AMD,
         NFLX_mad = mean(NFLX) - NFLX)

StocksIsolated <- StocksMAD %>% 
  select(ends_with("_mad"))

ggplot(StocksIsolated, aes(x = CSCO_mad))+
  geom_point()

ggplot(StocksIsolated, aes(x = ends_with("_mad")))+
  geom_point()




ggplot() +
  geom_point(aes(x = value, y = loc, color = name), size = 2) +
  scale_color_identity() +
  geom_line(aes(x = value, y = loc)) + 
  theme_minimal()




StocksManip <- StocksManip %>% 
  mutate(ChgClose.Last = )

StocksManip <- StocksManip %>% 
  select(Company,Date,Close.Last,Close.LastLag)

Long_df <- StocksManip %>% 
  pivot_wider(names_from = Company, values_from = starts_with("Close."))

unique(Stocks$Company)
# c("AAPL","SBUX","MSFT","CSCO","QCOM","META","AMZN","TSLA","AMD","NFLX"), names_to = )

Long_dfChg <- Long_df %>% 
  relocate(ends_with(c("AAPL","SBUX","MSFT","CSCO","QCOM","META","AMZN","TSLA","AMD","NFLX")))

Long_dfChgTest <- Long_dfChg %>% 
  mutate(AAPL_Chg = Close.Last_AAPL - lag(Close.Last_AAPL))


Long_dfChgTest <- Long_dfChg %>% 
  mutate(AAPL_Chg = ifelse(
    is.na(Close.Last_AAPL) | is.na(lag(Close.Last_AAPL)),
    NA, 
    Close.Last_AAPL - lag(Close.Last_AAPL)))

Long_dfChg <- Long_df %>% 
  relocate(ends_with(c("AAPL","SBUX","MSFT","CSCO","QCOM","META","AMZN","TSLA","AMD","NFLX")))


# - - - - - - - #

Stocks1 <- Stocks %>% 
  select(Company,Date,Close.Last) %>% 
  arrange(desc(Date))

Stocks1 <- Stocks1 %>% 
  pivot_wider(names_from = Company, values_from = starts_with("Close."))

StocksClosing <- Stocks1

summarize(Stocks$Close.Last)
StocksUpdated <- read.csv("StocksUpdated.csv")


