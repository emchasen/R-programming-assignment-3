rankall <- function(outcome, num = "best"){
        dat <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
        names(dat)[11] <- "heart attack"
        names(dat)[17] <- "heart failure"
        names(dat)[23] <- "pneumonia"
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!outcome %in% valid_outcomes) {
                stop("invalid outcome")
        }
        newdata <- dat[c("Hospital.Name", "State", outcome)]
        newdata[, 3] <- as.numeric(newdata[, 3])
        ## We save the levels of column 2, the states' names, in the states vector
        newdata$State <- as.factor(newdata$State)
        states <- levels(newdata[,2])
        ## We generate an empty vector that we will fill later, row by row, to generate our final output
        output <- vector()
        ## For loop to get the right data on each hospital. length(states) is the number of different states in our
        ## database. In our case we have 54 states.
        for (i in 1:length(states)) {
                ## statedata subsets data by the considered state
                statedata <- newdata[grep(states[i],dat$State),]
                orderdata <- statedata[order(statedata[,2],statedata[,3],statedata[,1]),]
                ## append() adds elements at the end of a vector. We want to add the name of the hopital [num ,1],
                ## the outcome [num,3]. We don't add the name of the states, because it
                ## will be the label of the rows.
                if(num == "best"){
                        rank <- 1
                } else if(num == "worst"){
                        rank <- nrow(statedata)
                        #print(worst)
                } else {rank = as.numeric(num)}
                output <- append (output, as.character(orderdata[rank,1]))
        }
       df <- data.frame(hospital = output, state = states, row.names = states)
       df
}