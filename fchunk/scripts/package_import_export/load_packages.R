# restore_packages.R
#
# installs each package from the stored list of packages

load("packages.rda")

for (count in 1:length(installedpackages)) install.packages(installedpackages[count])