# Test script for expression functionality
library(rlang)

# Create a test data frame similar to mtcars
test_df <- data.frame(
  mpg = c(21.0, 21.0, 22.8, 21.4, 18.7),
  wt = c(2.620, 2.875, 2.320, 3.215, 3.440),
  cyl = c(6, 6, 4, 6, 8)
)

# Test the parse_expression function
parse_expression <- function(expr_str, df) {
  if (is.null(expr_str) || expr_str == "" || expr_str == "None") {
    return(NULL)
  }
  
  tryCatch({
    # Parse the expression
    parsed_expr <- rlang::parse_expr(expr_str)
    return(parsed_expr)
  }, error = function(e) {
    return(NULL)
  })
}

# Test cases
test_expressions <- c("mpg", "wt", "mpg^2", "wt/cyl", "mpg * wt", "sqrt(mpg)")

cat("Testing expression parsing:\n")
for (expr in test_expressions) {
  parsed <- parse_expression(expr, test_df)
  if (!is.null(parsed)) {
    result <- tryCatch({
      rlang::eval_tidy(parsed, data = test_df)
      "SUCCESS"
    }, error = function(e) {
      paste("ERROR:", e$message)
    })
    cat(sprintf("Expression '%s': %s\n", expr, result))
  } else {
    cat(sprintf("Expression '%s': FAILED TO PARSE\n", expr))
  }
}

cat("\nTesting specific expressions:\n")
# Test mpg^2
expr1 <- parse_expression("mpg^2", test_df)
result1 <- rlang::eval_tidy(expr1, data = test_df)
cat("mpg^2 result:", result1, "\n")

# Test wt/cyl
expr2 <- parse_expression("wt/cyl", test_df)
result2 <- rlang::eval_tidy(expr2, data = test_df)
cat("wt/cyl result:", result2, "\n")

cat("\nAll tests completed!\n") 