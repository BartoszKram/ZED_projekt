#````fillMissingValues``````
library(dplyr)

fillMissingValues <- function(raw_data){
  columns <- c("cfin1","cfin2","chel1","chel2","lcop1","lcop2","sst")
  result_set <- raw_data
  for(cn in 1:7){
    column <- columns[cn]
    dfToFill <- result_set%>%
      filter_at(columns[cn], all_vars(is.na(.)))
    for(i in 1:nrow(dfToFill)){
      row <- dfToFill[i,]$X+1
      low <- c(dfToFill[i,]$X-2,1)%>%max()
      top <- c(dfToFill[i,]$X+4,nrow(raw_data))%>%min()
      column_group_with_na <- slice(raw_data,low:top)%>%
        filter_at(columns[cn], all_vars(!is.na(.)))
      column_clear <- column_group_with_na%>%filter( X != as.numeric(row))
      missing_value <- median(as.numeric(unlist(column_clear[,column])),na.rm = TRUE)
      result_set[row,column] <- missing_value
    }
  }
  return(result_set)
}

add_year <- function(data, aggregated_data){
  max_year <- 1
  data <- mutate(data,year=1)
  aggregated_data <- mutate(aggregated_data, year = 0)%>%mutate(row_id = row_number())
  for(i in 1:nrow(data)){
     filtered_row <- filter(aggregated_data, recr == data[i,]$recr)
     if(as.numeric(filtered_row$year) != 0){
       data[i,]$year <- filtered_row$year
     }
     else{
       max_year <- max_year + 1
       aggregated_data[filtered_row$row_id,]$year <- max_year
       data[i,]$year <- filtered_row$year
     }
  }
  return(data)
}
