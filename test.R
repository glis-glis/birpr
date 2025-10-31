# Install birp
library(devtools)
com <- strsplit(system("git log --oneline | head -n 1", intern=TRUE), " ")[[1]][1]
install_bitbucket("wegmannlab/birpr", com, force=TRUE)

library(birp)

# Simulate some data
data <- simulate_birp(gamma = c(-0.1, 0.1), timepoints = 2000:2020, timesOfChange = 2010)
print(data)
plot(data)
