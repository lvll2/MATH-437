permutation.twosample.t <- function(formula, data, alternative = "two.sided", B = 9999, seed = 1){
  # Function to perform a permutation-based two-sample t test

  t.obs <- t.test(formula, data, var.equal = TRUE)$statistic  # observed t-statistic
  # note that we require equal variance (by our assumptions)
  # but we don't care about Ha because the t-statistic is computed assuming H0 is true
  
  # Set up an nx2 data frame - left column is the y-values, right column is the groups
  t.test.df <- model.frame(formula, data)
  
  # don't worry about these issues in your lab assignment - you'll have usable data
  if (ncol(t.test.df) != 2) stop("Requires one group variable")
  if (!is.factor(t.test.df[[2]])) t.test.df[[2]] <- as.factor(t.test.df[[2]])  # convert to factor
  if (length(levels(t.test.df[[2]])) != 2) stop("Requires 2 groups")
  
  # initialize the vector of resampled t-statistics
  t.star <- numeric(B)  # B 0's
  
  set.seed(seed)
  for (i in 1:B){
    t.test.df[[1]] <- sample(t.test.df[[1]])  # randomly reorders the response variable
    t.star[i] <- t.test(formula, t.test.df, var.equal = TRUE)$statistic  # get the new t-statistic
    
    # print out how far we are 
    if ((i %% 500) == 0) cat("Iteration Number: ", i, "\n", sep = "")  # if we are at a multiple of 500 print it
  }
  
  t.all <- c(t.star, t.obs)  # add the original t-statistic back in
  # strictly speaking, this is not mathematically necessary, however:
  # 1) It leads to the correct p-value formulas for permutation tests using Monte Carlo simulation
  # 2) It results from an intuitive explanation - by definition, one of the zillions of permutations we could have observed gave us t.obs
  # 3) It prevents us from ever getting a p-value of 0

  t.left <- sum(t.all <= t.obs)  # compute the number of simulated t statistics <= t.obs
  t.right <- sum(t.all >= t.obs)  # compute the number of simulated t statistics >= t.obs
  
  # Use the switch function to compute the correct p-value
  p.value <- dplyr::case_when(
    alternative %in% c("less", "l") ~ t.left/(B+1),
    alternative %in% c("greater", "g") ~ t.right/(B+1),
    alternative %in% c("two.sided", "t") ~ 2*min(t.left, t.right)/(B+1))
  
  cat("Observed t-Statistic = ", t.obs, "\n", sep = "")  # paste and output to console
  cat("Approximate P-Value = ", p.value, "\n", sep = "")

  # return a list with the observed t-statistic, resampled t-statistics, and p-value
  # invisible means that the returned values are not printed to the console
  invisible(list(t = t.obs, t.perm = t.star, p = p.value))  
}

# compare using R example
t.output <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
permutation.output <- permutation.twosample.t(extra ~ group, data = sleep)

t.output$p.value  # original p-value
permutation.output$p  # permutation p-value

# quick and dirty histogram for comparison
hist(permutation.output$t.perm, main = "Simulated t-star values")
abline(v = permutation.output$t, col = "red", lty = 2)  # red dashed vertical line at t.observed
