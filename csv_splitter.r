# Samantha Deokinanan
# This code splits and save data by a specific group
# Last updated 9/7/2017

library(tidyverse)

df = read_csv("Report.csv")

group = " " # <----------------- state name of group column

for (name in levels(as.factor(df[group]))){
  tmp = subset(df, df[group] == name)
  fn = paste('student_',gsub(' ','', name), '.csv', sep='') # <---- edit name if needed
  write.csv(tmp, fn, row.names = FALSE)
}
