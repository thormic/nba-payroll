###################
# global.R
# 
# Anything shared between your ui and server.
###################


set.seed(122)
df <- data.frame(
  n = rnorm(500),
  m = rnorm(100))
