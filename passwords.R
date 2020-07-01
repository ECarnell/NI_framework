#install.packages("sodium")

# create a user base then hash passwords with sodium
# then save to an rds file in app directory
library(sodium)

user_base <- data.frame(
  user = c("UKCEH", "Daera"),
  password = sapply(c("Ballynahone", "Ballynahone"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("UKCEH", "Daera"),
  stringsAsFactors = FALSE,
  row.names = NULL
)

saveRDS(user_base, "C:/Shiny_dev/user_base.rds")

user_base <- readRDS("C:/Shiny_dev/user_base.rds")
