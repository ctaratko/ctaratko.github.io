countNumberOfSubarrays <- function(arr, k) {
  # Write your code here
  current_sum <- 0
  my_count <- 0
  my_hash <- new.env(hash = TRUE)
  my_hash[["0"]] <- 1
  
  for (num in arr) {
    current_sum <- current_sum + num
    
    our_k <- as.character(current_sum - k)
    
    if (exists(our_k, envir = my_hash)) {
      my_count <- my_count + my_hash[[our_k]]
    }
    
    current_key <- as.character(current_sum)
    if (exists(current_key, envir = my_hash)) {
      my_hash[[current_key]] <- my_hash[[current_key]] + 1
    } else {
      my_hash[[current_key]] <-  1
    }
  }
  return(my_count)
}
arr <- c(1,2,3,0)
k <- 3
print(countNumberOfSubarrays(arr,k))

stdin <- file('stdin')