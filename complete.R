
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

complete <- function(directory, id = 1:332) 
{ 
  nobs = vector('numeric')
  c <- 0
       for( i in seq_along(id))
     {  c =0 ; 
        data_table = getdata(id[i], directory)
        data_sulfate_raw = data_table[[2]]
        data_nitrate_raw = data_table[[3]]
        for( j in seq_along(data_sulfate_raw))
        {
         if( !is.na(data_sulfate_raw[j]) && !is.na(data_nitrate_raw[j]) )
           c = c + 1
        }
        nobs[i]= c; 
      }
   output = data.frame( id , nobs );
   print(output)
}    
   