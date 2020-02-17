#!/usr/bin/Rscript

library(rsconnect)
print(R.Version())
##rsconnect::deployApp('app.R')
rsconnect::deployApp()
rsconnect::showLogs()
##rsconnect::terminateApp
