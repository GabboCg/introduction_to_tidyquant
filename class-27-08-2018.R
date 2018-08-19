#'---
#'author: Gabriel Cabrera G.
#'title: Introduction to tidyquant
#'subtitle: Bringing financial analysis to the tidyverse
#'date: 27/08/2018
#'---

# cargamos librerias 
if(!require("pacman")) install.packages("pacman")
p_load("tidyverse","tidyquant", "ggthemes")


# Descargando datos  ------------------------------------------------------

# Directamente con tidyquant
sp500_precio  <- tq_get("^GSPC", 
                        get = "stock.prices", 
                        from = "2010-01-01", 
                        to = "2018-08-01", 
                        periodicity = "daily")

# Usando quantmod
getSymbols("^GSPC",src = "yahoo", from = "2010-01-01", to = "2018-08-01", periodicity = "daily")

# Graficamos con chartSeries 
chartSeries(GSPC)

# Graficamos sin los volume
chartSeries(GSPC, TA = NULL)

# Tres últimos meses
chartSeries(GSPC, subset = "last 3 months")

# creamos un objeto con las mismas caracteristicas que sp500_precio
sp500_quantmod <- as.data.frame(GSPC) %>%
                  as.tibble() %>% 
                  mutate(date = index(GSPC)) %>% 
                  rename("open" = "GSPC.Open", "high" = "GSPC.High",     
                         "low"  = "GSPC.Low" , "close"= "GSPC.Close", 
                         "volume" = "GSPC.Volume", "adjusted" = "GSPC.Adjusted")

# Usando ggplot2
g <- ggplot(sp500_precio) + geom_line(aes(date,adjusted), color = "red")
g <- g + labs(title = "Precio S&P 500", subtitle = "Desde Enero 2010 hasta Julio 2018")
g <- g + theme_tq() + scale_color_tq() 
g <- g + xlab("Periodo") + ylab("Precio")
g 

# Usando ggplot2 con stata theme 
sp500_quantmod %>%  
     ggplot() + 
     geom_line(aes(date,adjusted), color = "red" ) + 
     labs(title = "Precio S&P 500", subtitle = "Desde Enero 2010 hasta Julio 2018") + 
     theme_stata() + xlab("Periodo") + ylab("Precio") + 
     scale_color_stata()

