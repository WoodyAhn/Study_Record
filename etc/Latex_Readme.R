rm(list=ls())
library(latexreadme)
library(knitr)

args(parse_latex)

setwd("/Users/wooddekk/Desktop/project_R/Study_Record/")
getwd()
rmd = file.path(getwd(),"etc/Selection_Ahn.md")

new_md = file.path(getwd(),"etc/Selection_Ahn_parsed.md")

parse_latex(rmd,new_md,
            git_username = "WoodyAhn",
            git_reponame = "Study_Record")

