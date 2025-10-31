library(devtools)
com <- strsplit(system("git log --oneline | head -n 1", intern=TRUE), " ")[[1]][1]
install_bitbucket("wegmannlab/birpr", com)
