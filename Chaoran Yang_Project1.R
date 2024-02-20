#####################################################################################################
# Project1 PartA: Use R to Write a Formula to Calculate the Mortgage Payment
# By Chaoran Yang
# updated on 7/2/2024
##################################################################################################### 

setwd("/Users/yangchaoran/Desktop/course document/Data")
# Define variables
p <- 650000
d <- 0.1
z <- 0.028
y <- 30

# Define monthly mortgage rate
c <- z / 12

# Define total months of the loan
n <- y * 12

# Calculate monthly repayment
m <- (p * (1 - d) * (c * (1 + c) ^ n)) / (((1 + c) ^ n) - 1)

# Print the result
cat("Monthly repayment for Taylor's mortgage at 2.8% rate:", m, "\n")

# Update mortgage rate to 6.1%
z_new <- 0.061  

# new monthly mortgage rate
c_new <- z_new / 12

# new monthly repayment using the updated rate
m_new <- (p * (1 - d) * (c_new * (1 + c_new) ^ n)) / (((1 + c_new) ^ n) - 1)

# Print the new result
cat("New monthly repayment for Taylor's mortgage at 6.1% rate:", m_new, "\n")

#####################################################################################################
# Project1 PartB: Calculate the Density of Tech Job by Zip Code in Los Angeles
# By Chaoran Yang
# updated on 7/2/2024
#####################################################################################################

library(openxlsx)
library(dplyr)
library(readxl) 

# Setting Path and Reading Files
Dir = "/Users/yangchaoran/Desktop/course document/Data"
outputDir = paste0(Dir, "/Output")
setwd(Dir)
payroll_21 = data.frame(read_excel("P01_LA zipcode payroll.xlsx",sheet="2021"))

# Data cleaning
clean_data <- function(x) {
  x[is.na(x)] <- 0
  x[x == "*****"] <- 0
  x[,5:6] <- lapply(x[,5:6],as.numeric)
  x <- x
  return(x)
}
payroll_21 <- clean_data(payroll_21)

# Produce data output
info_21 <- subset(payroll_21, Industry == "Information", select = c("Zip.Code", "Employment"))
pro_21 <- subset(payroll_21, Industry == "Professional, Scientific, & Technical Services", select = c("Zip.Code", "Employment"))
total_21 <- subset(payroll_21, Industry == "0", select = c("Zip.Code", "Employment"))

# Changing the dataframe column names
info_21 <- setNames(info_21, c("zipcode", "information"))
pro_21 <- setNames(pro_21, c("zipcode", "professional"))
total_21 <- setNames(total_21, c("zipcode", "total"))

# Merge data frames and rearrange the columns
laz21tech <- info_21 %>%
  full_join(pro_21, by = "zipcode") %>%
  full_join(total_21, by = "zipcode")

laz21tech <- laz21tech %>%
  select(zipcode, total, information, professional)

# Calculate the percentage of tech job
laz21tech$combined_employment <- laz21tech$information + laz21tech$professional 
laz21tech$pre <- (laz21tech$combined_employment / laz21tech$total) 
print(laz21tech)


###################################################Check using 2018 data##################################################

#payroll_18 = data.frame(read_excel("P01_LA zipcode payroll.xlsx",sheet="2018"))

#payroll_18[is.na(payroll_18)] <- 0
#payroll_18[payroll_18 == "*****"] <- 0
#payroll_18[, 5:6] <- lapply(payroll_18[, 5:6], as.numeric)
#payroll_18$Zip.Code <- gsub(" Total", "", payroll_18$Zip.Code) # Remove the "Total" from the row name.

#info_18 <- subset(payroll_18, Industry == "Information", select = c("Zip.Code", "Employment"))
#pro_18 <- subset(payroll_18, Industry == "Professional, Scientific, & Technical Services", select = c("Zip.Code", "Employment"))
#total_18 <- subset(payroll_18, Industry == "0", select = c("Zip.Code", "Employment"))

#info_18 <- setNames(info_18, c("zipcode", "information"))
#pro_18 <- setNames(pro_18, c("zipcode", "professional"))
#total_18 <- setNames(total_18, c("zipcode", "total"))

#library(dplyr) 
#laz18tech_output <- info_18 %>%
  #full_join(pro_18, by = "zipcode") %>%
  #full_join(total_18, by = "zipcode")

#laz18tech_output <- laz18tech_output %>%
  #select(zipcode, total, information, professional)

#laz18tech_output$combined_employment <- laz18tech_output$information + laz18tech_output$professional 
#laz18tech_output$pre <- (laz18tech_output$combined_employment / laz18tech_output$total) 
#print(laz18tech_output)



# Export the result
output_path <- "/Users/yangchaoran/Desktop/course document/Output"
output_filename <- "laz21tech.xlsx"
write.xlsx(laz21tech, file.path(output_path, output_filename), rowNames = FALSE)
