#setwd("Google Drive/coursera/R programming/rprog-data-ProgAssignment3-data")

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
        names(dat)[11] <- "heart attack"
        names(dat)[17] <- "heart failure"
        names(dat)[23] <- "pneumonia"
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% valid_outcomes) {
                stop("invalid outcome")
        }
        ##subset data into columns necessary for analysis
        newdata <- dat[c("Hospital.Name", "State", outcome)]
        newdata2 <- subset(newdata, State == state)
        #print(head(newdata2))
        #print(nrow(newdata2))
        full_data <- na.omit(newdata2)
        full_data[, 3] <- as.numeric(full_data[, 3])
        #print(nrow(full_data))
        ## Check that state and outcome are valid
        if(nrow(full_data) == 0){
               stop("invalid state")
        }
        ## For each state, find the hospital of the given rank
        orderdata <- full_data[order(full_data[,3],full_data[,1]),]
        #print(head(orderdata))
        Rank <- c(1:nrow(orderdata))
        all_data <- data.frame(orderdata, Rank)
        #print(head(all_data))
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(num == "best"){
                num <- 1
        } else if(num == "worst"){
                num <- nrow(all_data)
        } else {num = as.numeric(num)}
        all_data[num, 1]
}