

is_integer <- function(x){
  x_n <- as.numeric(x)
  if(is.na(x_n) | x_n != floor(x_n) ){
    return(FALSE)
  } else{
    return(TRUE)
  }
}