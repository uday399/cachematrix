getdata <- function(id, directory) 
{
    if(id>=1 && id<10) 
    {
        rd <- paste("00",id,sep="")
       
    } else 
    if(id>=10 && id<=99) 
     {
        rd <- paste("0",id,sep="")
        
    } else {
        rd <- id
        
    }
name0= paste( "C:/Users/Rishav/Documents/coursera",directory, sep = "/")
name1 = paste(name0, rd, sep = "/")
name2 = paste(name1, "csv", sep = ".")
test <- read.csv(file = name2)
    return(test);
}

corr <- function(directory, threshold = 0) 
{ 
  result = vector('numeric')
  for( id in 1:332) {
  c <- 0
  data_nitrate_filtered = vector('numeric')
  data_sulfate_filtered = vector('numeric')
  corr = 0;
        data_table = getdata(id, directory)
        data_sulfate_raw = data_table[[2]]
        data_nitrate_raw = data_table[[3]]        
        for( j in seq_along(data_sulfate_raw))
        {
         if( !is.na(data_sulfate_raw[j]) && !is.na(data_nitrate_raw[j]) )
           {
            c = c + 1
            data_sulfate_filtered = c(data_sulfate_filtered,data_sulfate_raw[j])
            data_nitrate_filtered = c(data_nitrate_filtered,data_nitrate_raw[j])
            }
        }
        if(c > threshold)
        { 
        corr= cor(data_sulfate_filtered , data_nitrate_filtered )
        result= c(result, corr) 
        }
       }
   return (result)
}    
