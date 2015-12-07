#setwd("Google Drive/coursera/R programming/rprog-data-ProgAssignment3-data")

best <- function(state, outcome){
        dat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
        ## rename outcome variables 
        names(dat)[11] <- "heart attack"
        names(dat)[17] <- "heart failure"
        names(dat)[23] <- "pneumonia"
        ## Check that state and outcome are valid
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% valid_outcomes) {
                stop("invalid outcome")
        }
        ##subset data into columns necessary for analysis
        newdata <- dat[c("Hospital.Name", "State", outcome)]
        #print(head(newdata))
        ##subset to look just at state of interest
        stdata <- subset(newdata, State == state)
        if(nrow(stdata) == 0){
                stop("invalid state")
                }
        #print(head(stdata))
        full_data <- na.omit(stdata)
        #print(tail(full_data))
        ##change outcome to numeric
        full_data[, 3] <- as.numeric(full_data[, 3])
        #print(head(full_data[, 3] <- as.numeric(full_data[, 3])))
        ## Return hospital name in that state with lowest 30-day death
        answer <- which.min(full_data[,3])
        #answer
        full_data[answer, 1]
}