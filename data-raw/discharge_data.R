# This script creates a sample hospital discharge dataset.

# Suppose there are 8 hospitals. 1 and 2 are in the same system,
# 3 and 4 in another system, and 5-8 independent.

# Let N=1,200 discharges
# rm(list=ls())

N <- 1200
N_drg <- 4

drg <- sample(N_drg, size = N, replace = TRUE)
age <- sample(c(30,40,50,60), size = N, replace = TRUE)
zip5 <- sample(c(53130,53131,53132), size = N, replace = TRUE, prob = c(.4,.4,.2))

H <- 8
K <- 3

# calculate utilities
util = matrix(0, nrow = N, ncol = H)

drg_val <- matrix(runif(N_drg*H) , nrow = N_drg, ncol = H)


for (h in 1:H) {
  util[,h] <-  drg_val[drg,h] + runif(N)

}


hosp_id <- max.col(util)

discharge_data <- data.frame(drg, age, zip5, hosp_id)
discharge_data$Hospital <- paste0("Hospital ",hosp_id)
discharge_data$sys_id <- hosp_id
discharge_data$sys_id[discharge_data$hosp_id==2] <- 1
discharge_data$sys_id[discharge_data$hosp_id==4] <- 3
discharge_data$System <- paste0("System ",discharge_data$sys_id)

#save(discharge_data,file="data/discharge_data.RData")
usethis::use_data(discharge_data, overwrite = TRUE)

tabulate(discharge_data$hosp_id)

discharge_data %>%
  group_by(drg,age,zip5) %>%
  summarize(freq = n())
