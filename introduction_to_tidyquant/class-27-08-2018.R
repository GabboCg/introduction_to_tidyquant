#'---
#'author: Gabriel Cabrera G.
#'title: Introduction to tidyquant
#'subtitle: Bringing financial analysis to the tidyverse
#'date: 27/08/2018
#'---

# cargamos librerias 
if(!require("pacman")) install.packages("pacman")
p_load("tidyverse","tidyquant")


# Introduction to tidyquant -----------------------------------------------


stock <- c("FB","AMZN")

data <-  tq_get(stock, 
                get = "stock.prices", 
                from = "2010-01-01", 
                to = "2018-01-01", 
                periodicity = "monthly")

