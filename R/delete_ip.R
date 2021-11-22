# Delete IP from Local 
library(dplyr)

df <- read.csv("data/aural-skills-survey-2010-working-20211122.csv")

df <- select(df, -ip)

write.csv(df, "data/aural-skills-survey-2010-working-20211122.csv")
