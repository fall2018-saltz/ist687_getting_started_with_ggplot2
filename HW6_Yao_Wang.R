
clean_data <- raw_data
# re-use the code from before
readStates <- function(states)
{
    # remover rows that not needed 
    # -- first row is the total for the US, we do not need that 
    states <- states[-1,]
    #-- last row is Puerto Rico, it is not a states
    num.row <- nrow(states)
    states <- states[-num.row,]
    
    # remover the first for coclumns
    states <- states[,-1:-4]
    
    # change names for remaining coclumns
    colnames(states) <- c("stateName", "population","popOver18", "percentOver18")
    
    # return the results
    return(states)
}

cleanCencus <- readStates(raw_data)
str(cleanCencus)
