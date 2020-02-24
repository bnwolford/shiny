library(shiny)
library(ggvis)
library(optparse)
library(data.table)
library(tidyverse)
library(DT)
library(ggvis)


A<-fread("troponin_variants_HRC_lookup_phenoDefs.txt.gz")
B<-fread("troponin_variants_HRC_lookup_phenoDefs.txt.gz")
data_sets<-list(A,B)

