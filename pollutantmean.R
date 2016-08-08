
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

pollutantmean <- function(directory, pollutant, id= 1:332) 
{ sulfate_total = vector('numeric')
  nitrate_total = vector('numeric')
       for( i in seq_along(id))
     {   
        data_table = getdata(id[i], directory)
        data_sulfate_raw = data_table[[2]]
        data_nitrate_raw = data_table[[3]]
        data_sulfate_final = data_sulfate_raw[!is.na(data_sulfate_raw)]
        data_nitrate_final = data_nitrate_raw[!is.na(data_nitrate_raw)]
        sulfate_total = c(sulfate_total, data_sulfate_final)
        nitrate_total = c(nitrate_total, data_nitrate_final)
        }  
    if( pollutant == 'nitrate')
      return(mean(nitrate_total));
     if( pollutant == 'sulfate')
      return(mean(sulfate_total));
}    
   