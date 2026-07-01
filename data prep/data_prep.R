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
  dat$country[x] <- split[[1]][1]
  dat$state[x] <- split[[1]][2]
}

write.csv(dat, "../YDPR_data_current.csv")
