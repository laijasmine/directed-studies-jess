---
title: "READme"
author: "Jessica"
date: "12/04/2020"
output: html_document
---

#Spatiotemporal Analysis of Algal Samples from the UBC Herbarium in British Columbia & Alaska 

This projects (EOSC448B_BCData.R) explores and maps the spatiotemporal trends within the UBC herbariums algae collections data using R. The UBC Herbarium is a rich resource, containing the largest collection of herbaria specimens in Western Canada. Within this collection we house an impressive set of macro algae samples, totaling approximately 90, 000 specimens. Our representative algal samples allow us to better understand the diverse ranges of seaweed around the world, but as researchers it can be difficult to pinpoint species of interest. It is also necessary to identify the limitations of herbaria data. This project starts to solve some of these problems by identifying potential species of interest within the UBC Herbarium collection by exploring some of the spatial and temporal trends of these species. Additionally, we are able to identify the top three locations with the greatest collection frequency and identify the top four species collected in one area over the largest time frome. 

##Program Use

This program filters data and produces a interactive map of alagal samples collected in BC. Data can then be filtered and mapped by species of interest and then further filtered by specified spatial boundaries. You can then create a box plot of the data. 

## Getting Started

These instructions will get you a copy of the project up and running on your local machine. 

###Software Requirements: 

R

###Packages

sf
tmap
tmaptools
tidyverse
readxl
leaflet
plotly
lubridate
lasso2

### Prerequisites

If you are new to R you will want to follow the [Setup instructions] (https://datacarpentry.org/R-ecology-lesson/#chapters) for downloading and installing R and RStudio. 

### Installing

1. Clone the git repository to local environment 
2. Open the project in R Studio 
3. Open the ExploreAlgae_BCData.R and run 

##Acknowledgements 
 
I would like to give a big thanks to Jasmine Lai for being such an excellent mentor as well as to Linda and the rest of her team at the herbarium. Additionally, I would also like to thank Michael for making this possible as well as Michelle Tseng for taking the time to answer our questions and give meaningful feedback for the project.  
