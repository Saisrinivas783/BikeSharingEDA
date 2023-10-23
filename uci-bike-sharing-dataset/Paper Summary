---
# This is the YAML header/metadata for the document
title: "Bike Share Chronicles_Unveiling Patterns"
author's: "1) Lakshmi Sreya Rapolu 2) Ashwin Muthuraman 3) Sai Srinivas Lakkoju "
# date: "today"
date: "`r Sys.Date()`" 
# this style requires installing rmdformats package 
output:  
    rmdformats::readthedown:
      toc_float: true
      toc_depth: 3
      number_sections: true
      code_folding: hide
      includes:
        before_body: header.tmphtml
---




```{r setup, include=FALSE}
# some of common options (and the defaults) are: 
# include=T, eval=T, echo=T, results='hide'/'asis'/'markup',..., collapse=F, warning=T, message=T, error=T, cache=T, fig.width=6, fig.height=4, fig.dim=c(6,4) #inches, fig.align='left'/'center','right', 
 knitr::opts_chunk$set(warning = F, results = "markup", message = F)
#knitr::opts_chunk$set(warning = F, results = "hide", message = F)

# knitr::opts_chunk$set(include = F)
# knitr::opts_chunk$set(echo = TRUE)
options(scientific=T, digits = 3) 
# options(scipen=9, digits = 3) 
# ‘scipen’: integer. A penalty to be applied when deciding to print numeric values in fixed or exponential notation.  Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than ‘scipen’ digits wider.
# use scipen=999 to prevent scientific notation at all times
```

# Introduction

Bike sharing programs are gaining popularity worldwide because they are environmentally friendly and healthy. Cities are developing bike sharing programs to encourage people to ride bicycles.

Riders can rent bikes from manual or automated stations throughout the city for a set period of time. Riders can typically pick up bikes from one location and return them to another designated location.

Bike sharing programs generate a lot of data, such as travel time, start and end locations, and rider demographics. This data can be combined with other sources of information, such as weather,holiday and season, to learn more about how and when people use bike sharing programs.

## Problem Statement

The goal of this case is to perform Exploratory Data Analysis (EDA) on daily bike rental counts, considering environmental and seasonal variables. This EDA aims to understand the historical usage patterns of a bike-sharing program in Washington, D.C., in relation to weather, environmental factors, and other data. We are interested in exploring the relationships between bike rentals and various factors such as season, temperature, and weather conditions. Our objective is to build insights and understanding from the data 
