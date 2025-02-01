# test_hiperglm.R

# Load the package
library(hiperglm)

# Create dummy data
design <- matrix(1:12, nrow = 4, ncol = 3)
outcome <- c(1, 0, 1, 0)

# Call the main function; creates a 'hiperglm' object
message("\n\n--- --- --- --- --- ---\n\n")
model_out <- hiperglm(design = design, outcome = outcome)
message("\n\n--- --- --- --- --- ---\n\n")


# Check the methods
coef(model_out)     # Should warn it's not implemented yet
message("\n\n--- --- --- --- --- ---\n\n")

vcov(model_out)     # (same)
message("\n\n--- --- --- --- --- ---\n\n")

print(model_out)    # (same)
message("\n\n--- --- --- --- --- ---\n\n")

# Confirm the class
class(model_out)    # Should be "hiperglm"
