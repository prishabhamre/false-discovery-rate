fdrmyv<-function(p_values, Q, independent = TRUE) {
  #Sort P values
  sorted_p <- sort(p_values)
  #count tests 
  m <- length(sorted_p)
  #Plot sorted Pvalues(smallest to largest) vs 1:m
  plot(1:m, sorted_p, xlab = "Test Index", ylab = "P Value",
       main = "Sorted P Values vs Test Index")
  #conditional statement to modify the threshold line calculation
  if (independent) {
    threshold_line <- Q * (1:m) / m
  } else {
    threshold_line <- Q * (1:m) / (m * sum(1 / (1:m)))
  }
  #draw line Q*c(1:m)/m vs 1:m
  lines(1:m, threshold_line)
  #find p*
  p_star <- max(sorted_p[sorted_p <= threshold_line])
  #find interesting pvalues
  interesting <- p_values <= p_star
  #find pvalues and their indecies below the q line
  below_threshold_indices <- which(sorted_p <= threshold_line)
  #plots the pval points in the color blue
  points(below_threshold_indices, sorted_p[below_threshold_indices], col = "blue", pch = 20)
  #outputs the list of hypothesis which are interesting by number in the  original unsorted list of p values.
  result <- list(
    ind = independent,
    p_values = p_values[interesting]
  )
  
  return(result)
}