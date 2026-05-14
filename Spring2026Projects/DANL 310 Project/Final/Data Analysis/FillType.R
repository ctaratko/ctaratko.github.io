dat = read.csv("2024 bee tube survey_field notes - Summary of fills by tube.csv")
dat
df = data.frame(dat$Week.., dat$Fill.type)
df = na.omit(df)
df
plot(df$dat.Week.., length(df$dat.Fill.type))
