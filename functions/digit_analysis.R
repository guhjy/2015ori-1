digit_analysis <- function(x, type = 'terminal')
{
  if(!(is.matrix(x) | is.vector(x)))
  {
    stop('Please only specify a vector or matrix. 
         If specifying a matrix, ensure results from one set of digits are in the 
         rows!')
  }
  if(!(type == 'terminal' | type == 'benford'))
  {
    stop("Only benford and terminal allowed as types.")
  }
  
  matrix_of_digits <- apply(t(x), 1, function(y)
  {
    splitted <- strsplit(as.character(y), "")
    terminal <- lapply(splitted, function(z) tail(z, 1))
    return(unlist(terminal))
  })
  
  mode(matrix_of_digits) <- "numeric"
  chi <- apply(matrix_of_digits, 2, function(q)
  {
    obs <- digit_counter(q, type = type)
    exp <- expected_digit_counter(q, type = type)
    testval <- sum((obs-exp)^2/exp)
    return(testval)
  })
  
  if (type == 'terminal')
  {
    df <- 10 - 1 
  } else if (type == 'benford')
  { 
    df <- 9 - 1
  } else
  {
    stop("Something went awry.")
  }
  
  pval <- pchisq(q = chi, df = df, lower.tail = FALSE)
  res <- data.frame(chi, df = df, pval)
  
  return(res)
}

digit_counter <- function(x, type)
{
  if(!is.vector(x)) stop("Currently only works with vectors.")
  tabulated <- table(x)
  
  if (type == 'terminal')
  {
    counts <- rep(0, 10)
    counts[as.numeric(names(tabulated)) + 1] <- tabulated
  } else if (type == 'benford')
  {
    counts <- rep(0, 9)
    counts[as.numeric(names(tabulated))] <- tabulated  
  } else
  {
    stop("ERROR")
  }
  
  
  counts
}

expected_digit_counter <- function(x, type = type)
{
  if (!is.vector(x)) stop("Currently only works with vectors.")
  if (!(type == 'terminal' | type == 'benford'))
  {
    stop("Only benford and terminal allowed as types.")
  }
  
  if (type == 'terminal')
  {
    cell <- length(x) / 10
    counts <- rep(cell, 10)
  } else if (type == 'benford')
  {
    d <- 1:9
    counts <- length(x) * log(x = ((d + 1) / d), base = 10)
  } else
  {
    stop("Something went awry.")
  }
  
  
  return(counts)
}
