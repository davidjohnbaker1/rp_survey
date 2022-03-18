# Delete IP from Local 
library(dplyr)

df <- read.csv("data/2022-03-16-data-incomplete.csv")

df <- select(df, -ip)

write.csv(df, "data/2022-03-16-data-incomplete.csv")
