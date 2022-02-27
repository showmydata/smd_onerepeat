perc_rank_rm <- function(data,...){
  # This version of perc_rank was designed specifically for repeated measures data
  data <- as.data.frame(data)                     # Put into data frame (pasted data is not)
  data$id <- seq.int(nrow(data))                  # Add column of id numbers to enable reversible pivoting below
  data2 <- data %>% pivot_longer(cols = !"id", names_to = "mynames", values_to = "myvalues")    # pivot to long format
  ranked_data=rank(data2[,3],na.last="keep");                                                   # find ranks with NA's kept as NA's and not figured into the ranking
  vector_length=length(rank(data2[,3],na.last=NA))                                              # find the number of rows that were given a rank
  data2[,3]=(ranked_data/vector_length)*100                                                     # compute percentile ranks and add them back into original data frame
  data3 <- data2 %>% pivot_wider(names_from = mynames, values_from = myvalues)                  # pivot back to wide format
  data3$id <- NULL                                # Remove the id column before sending back
  data3 <- as.data.frame(data3)                   # Transform tibble back to regular data frame (tibble was causing errors elsewhere)
  return(data3)   
}