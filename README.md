# hiperglm

The **hiperglm** package is a GLM modeling library developed in R for the JHU-BSPH course **140.778**.

## Author: Deerspring

### **UPDATE 0.0.2: (2025-02-10 HW2)**  
hiperglm package has been updated to fulfill HW2 requirements:

- **MLE Estimation Methods:**  
  - Implemented a pseudo-inverse (OLS) solution for MLE without directly computing an inverse.
  - Added a BFGS optimizer using `stats::optim()` with from-scratch implementations of log-likelihood and gradient functions.

- **Function Enhancements:**  
  - Primary `hiperglm` function now accepts an optimizer argument to switch between pseudo-inverse and BFGS solving methods.
  - Following S3 methods have been updated:
    - `coef()` now returns the estimated coefficients.
    - `print()` now displays the coefficients and provides details on the chosen optimizer

- **Testing Improvements:**  
  - Set up testthat.
  - Added helper functions (`are_all_close`, `simulate_data`, and `approx_grad`) and corresponding unit tests.
  - Included tests for following tasks:
    - Verify the consistency of the analytical gradient with a finite-difference approximation
    - Compare MLE estimates from both optimization methods.

---

### UPDATE 0.0.1: 2025-01-31 (HW1)
Currently, package has a placeholder function, `hiper_glm()`, and minimal S3 methods (`coef()`, `vcov()`, and `print()`). These methods currently only warn that they are unimplemented when called.
