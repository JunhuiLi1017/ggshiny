# Test script for incomplete expression handling
library(rlang)

# Create a test data frame similar to mtcars
test_df <- data.frame(
  mpg = c(21.0, 21.0, 22.8, 21.4, 18.7),
  wt = c(2.620, 2.875, 2.320, 3.215, 3.440),
  cyl = c(6, 6, 4, 6, 8)
)

# Test the improved parse_expression function
parse_expression <- function(expr_str, df) {
  if (is.null(expr_str) || expr_str == "" || expr_str == "None") {
    return(NULL)
  }
  
  # Check for incomplete expressions that could cause parsing errors
  incomplete_patterns <- c(
    "/$",     # Ends with /
    "\\*$",   # Ends with *
    "\\+$",   # Ends with +
    "-$",     # Ends with -
    "\\^$",   # Ends with ^
    "\\($",   # Ends with (
    "\\[$",   # Ends with [
    "\\{$",   # Ends with {
    "\\s$"    # Ends with whitespace
  )
  
  for (pattern in incomplete_patterns) {
    if (grepl(pattern, expr_str)) {
      return(NULL)  # Return NULL for incomplete expressions
    }
  }
  
  # Check for balanced parentheses
  if (length(grep("\\(", expr_str)) != length(grep("\\)", expr_str))) {
    return(NULL)  # Unbalanced parentheses
  }
  
  tryCatch({
    # Parse the expression
    parsed_expr <- rlang::parse_expr(expr_str)
    return(parsed_expr)
  }, error = function(e) {
    return(NULL)
  })
}

# Test cases for incomplete expressions
incomplete_expressions <- c(
  "mpg/",      # Ends with /
  "wt*",       # Ends with *
  "mpg+",      # Ends with +
  "wt-",       # Ends with -
  "mpg^",      # Ends with ^
  "sqrt(",     # Unclosed parenthesis
  "mpg[",      # Unclosed bracket
  "mpg{",      # Unclosed brace
  "mpg ",      # Ends with space
  "mpg/cyl",   # Valid expression
  "wt^2",      # Valid expression
  "sqrt(mpg)"  # Valid expression
)

cat("Testing incomplete expression handling:\n")
for (expr in incomplete_expressions) {
  parsed <- parse_expression(expr, test_df)
  if (is.null(parsed)) {
    cat(sprintf("Expression '%s': ❌ REJECTED (incomplete/invalid)\n", expr))
  } else {
    result <- tryCatch({
      rlang::eval_tidy(parsed, data = test_df)
      cat(sprintf("Expression '%s': ✅ ACCEPTED\n", expr))
    }, error = function(e) {
      cat(sprintf("Expression '%s': ❌ REJECTED (evaluation error: %s)\n", expr, e$message))
    })
  }
}

cat("\nTesting specific incomplete expressions:\n")
# Test mpg/ (should be rejected)
expr1 <- parse_expression("mpg/", test_df)
if (is.null(expr1)) {
  cat("mpg/ correctly rejected as incomplete\n")
} else {
  cat("ERROR: mpg/ should have been rejected\n")
}

# Test mpg/cyl (should be accepted)
expr2 <- parse_expression("mpg/cyl", test_df)
if (!is.null(expr2)) {
  result2 <- rlang::eval_tidy(expr2, data = test_df)
  cat("mpg/cyl correctly accepted, result:", result2, "\n")
} else {
  cat("ERROR: mpg/cyl should have been accepted\n")
}

cat("\nAll tests completed!\n") 