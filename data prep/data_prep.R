dat <- read_xlsx("./YDPR_full.xlsx")

template <- read.csv("../YDPR_data_test.csv")
str(dat)

names(dat)
nrow(dat)

#simplify names

names(dat) <- c("id",
                "name",
                "year",
                "state_country",
                "region",
                "description",
                "status",
                "link")

#Check for empty character fields and replace with NAs.

for(name in names(dat)){
  print(paste("Empty strings:", name, "->", which(dat[[name]] == "")))
  dat[[name]][which(dat[[name]] == "")] <- NA
  
  print(paste("NAs:", which(is.na(dat[[name]]))))
}

# Check data types for year field. Coerce into integer: anything coerced 
#.  into an NA is an error that should be checked in excel.

for(name in names(dat)){
  print(paste(name, "->", class(dat[[name]])))
}

before <- sum(is.na(dat$year))

if(class(dat$year) == "character"){
  dat$year <- as.integer(dat$year)
}

after <-sum(is.na(dat$year))

if(after > before){
  message <- paste("Coercing characters into integers created an NA! Check the data to be safe. NAs found at the following indexes:", which(is.na(dat$year)))
  stop(message)
}

# Split state/country column into seperate state and country columns.

dat$state <- NA
dat$country <- NA

for(x in 1:nrow(dat)){
  value <- dat$state_country[x]
  split <- strsplit(value, ", ")
  dat$country[x] <- split[[1]][2]
  dat$state[x] <- split[[1]][1]
}

# Standardize the "status" column.
# Keep current status column as a "notes" column.

unique(dat$status)

passed_ids <- c("Enacted",
                "Passed",
                "enacted",
                "Engrossed")

pending_ids <- c("Pending",
                 "In commitee",
                 "In committee",
                 "In comitee",
                 "Introduced",
                 "Passed over",
                 "Out of committee",
                 "Proposed")
                 
failed_ids <- c("Failed",
                "tabled/died",
                "Died in committee",
                "Died",
                "Failed-Adjourned",
                "died",
                "failed",
                "Dead",
                "Failed, Reintroduced in 2025 cycle as A120",
                "Failed, Reintroduced in 2025 cycle as A150")

dat$notes <- dat$status

for(x in 1:nrow(dat)){
  if(dat$status[x] %in% passed_ids){
    dat$status[x] <- "passed"
  }
  
  if(dat$status[x] %in% pending_ids){
    dat$status[x] <- "pending"
  }
  
  if(dat$status[x] %in% failed_ids){
    dat$status[x] <- "failed"
  }
  
}

unique(dat$status)

# Write cleaned data

write.csv(dat, "../YDPR_data_current.csv")
