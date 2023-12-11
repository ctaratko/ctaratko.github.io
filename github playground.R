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
