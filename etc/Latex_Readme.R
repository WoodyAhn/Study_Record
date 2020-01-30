rm(list=ls())
library(latexreadme)
library(knitr)

args(parse_latex)

rmd = file.path(getwd(),"School/Semina/Linear_Regression/GLS/AR1.Rmd")

new_md = file.path(getwd(),"School/Semina/Linear_Regression/GLS/AR1_parsed.md")

parse_latex(rmd,new_md,
            git_username = "WoodyAhn",
            git_reponame = "Study_Record")


