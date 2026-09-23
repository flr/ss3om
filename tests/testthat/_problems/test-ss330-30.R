# Extracted from test-ss330.R:30

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ss3om", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
path <- file.path("3.30", "adrhke")
hke <- readOutputss3(path)
hkes <- readFLSss3(path, range = c(minfbar=1, maxfbar=4))
hkeom <- readFLomss3(path, range = c(minfbar=1, maxfbar=4))

# test -------------------------------------------------------------------------
expect_comparable(
    unitSums(ssb(hkes)),
    extractSSB(hke))
