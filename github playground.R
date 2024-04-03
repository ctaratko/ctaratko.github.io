#####
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
??ggthemes

# Stocks Manipulation ------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)


StocksUpdated2 <- read.csv("stocksUpdated2.csv")


StocksManip <- StocksUpdated2 %>% 
  mutate(CL_Change =Close.Last - lag(Close.Last))

StocksManip <- StocksManip %>% 
  select(Company,Date,CL_Change)

StocksManip <- StocksManip %>% 
  group_by(Company) %>% 
  arrange(Date) %>% 
  pivot_wider(names_from = Company, values_from = CL_Change)

summary(StocksManip)

StocksMAD <- StocksManip %>% 
  mutate(CSCO_mad = CSCO - mean(CSCO, na.rm = T),
         QCOM_mad = QCOM - mean(QCOM, na.rm = T),
         META_mad = META- mean(META, na.rm = T),
         AMZN_mad = AMZN - mean(AMZN, na.rm = T),
         TSLA_mad = TSLA - mean(TSLA, na.rm = T),
         AMD_mad = AMD - mean(AMD, na.rm = T),
         NFLX_mad = NFLX - mean(NFLX, na.rm = T),
         AAPL_mad = AAPL - mean(AAPL, na.rm = T),
         SBUX_mad = SBUX - mean(SBUX, na.rm = T),
         MSFT_mad = MSFT - mean(MSFT, na.rm = T))



mean(StocksManip$AAPL)
mean(StocksManip$MSFT)


StocksIsolated <- StocksMAD %>% 
  select(ends_with("_mad"))

Isolated_long <- StocksIsolated %>%
  pivot_longer(cols = ends_with("_MAD"),names_to = "Company", values_to = "MAD")


ggplot(Isolated_long, aes(y = Company, x = MAD)) +
  geom_hex()

ggplot(Isolated_long, aes(y = Company, x = MAD)) +
  geom_boxplot()




















colnames(StocksIsolated)

library(tidyr)

Isolated_long <- StocksIsolated %>%
  pivot_longer(cols = ends_with("_MAD"),names_to = "Company", values_to = "MAD")

ggplot(Isolated_long, aes(y = Company, x = MAD)) +
  geom_hex()


?pivot_longer




# Price Volume Trend Indicator -------------------------------------------------------

StocksUpdated2 <- read.csv("StocksUpdated2.csv")

StocksChange <- StocksUpdated %>% 
  select(Company, Date, Close.Last, Volume) %>% 
  group_by(Company) %>% 
  mutate(CL_Chg = Close.Last - lag(Close.Last),
         V_Chg = Volume - lag(Volume),
         PVT = (CL_Chg / V_Chg))

ggplot(StocksChange, aes( x = Company, y = PVT)) +
  geom_hex()
  


  
# mutate(CL_Chg = Close.Last - lag(Close.Last),
#          V_Chg = Volume - lag(Volume))

colnames(StocksUpdated)






ggplot(StocksChange, aes(x = Volume_AAPL, y = Close.Last_AAPL)) +
  geom_hex()

ggplot(StocksChange, aes(x = Volume_SBUX, y = Close.Last_SBUX)) +
  geom_hex()

ggplot(StocksChange, aes(x = Volume_MSFT, y = Close.Last_MSFT)) +
  geom_hex()

ggplot(StocksChange, aes(x = Volume_CSCO, y = Close.Last_CSCO)) +
  geom_hex()

ggplot(StocksChange, aes(x = Volume_AAPL, y = Close.Last_AAPL)) +
  geom_hex()

ggplot(StocksChange, aes(x = Volume_AAPL, y = Close.Last_AAPL)) +
  geom_hex()


ggplot(StocksChange, aes(x = Volume_SBUX, y = Close.Last_SBUX)) +
  geom_point()

ggplot(StocksChange, aes(x = Volume_MSFT, y = Close.Last_MSFT)) +
  geom_point()

ggplot(StocksChange, aes(x = Volume_AAPL, y = Close.Last_AAPL)) +
  geom_point()

ggplot(StocksChange, aes(x = Volume_AAPL, y = Close.Last_AAPL)) +
  geom_point()

ggplot(StocksChange, aes(x = Volume_AAPL, y = Close.Last_AAPL)) +
  geom_point()













# NonWorking --------------------------------------------------------------



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

getwd()

# Jack Section ------------------------------------------------------------


Volatility_Analysis <- StocksUpdated2 %>% 
  separate(Date, into = c("Month/Day","Year"), sep = 6 )

# Directory ---------------------------------------------------------------

 setwd("/Users/christophertaratko/ProjectsList/Data Analytics/Personal Website")
getwd()
