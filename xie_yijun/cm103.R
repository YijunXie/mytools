install.packages("ghit")
library(ghit)
install_github("jennybc/repurrrsive")
installed.packages("listviewer")

#loading packages
library(purrr)
library(repurrrsive)
library(listviewer)
library(jsonlite)
library(dplyr)
library(tibble)



# map(list, function)
map(c(9,16,25),sqrt)


str(gh_users, max.level = 1)
str(gh_users[[1]], list.len = 6)
listviewer::jsonedit(gh_users)


str(gh_users, max.level = 2)
str(gh_users[[1]], list.len = 2)
str(gh_users,max.level = 1, list.len = 1)

gh_users[[5]][c(1,2,4,6)]

map(gh_users,"login")
map(gh_users,2);map(gh_users,18)

gh_users %>% 
  map("login")

gh_users %>% 
  map("Login")

map_chr(gh_users,"login")
map_int(gh_users,"login")

x = map(gh_users, `[`, c("login", "name", "id", "location"))
y <- map(gh_users, magrittr::extract, c("login", "name", "id", "location"))
