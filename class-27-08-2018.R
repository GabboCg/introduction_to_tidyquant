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


# Multiples activos -------------------------------------------------------

tickers <- c("ORCL", "IT", "NVDA", "NFLX")

data_activos  <- tq_get(tickers, 
                        get = "stock.prices", 
                        from = "2010-01-01", 
                        to = "2018-08-01", 
                        periodicity = "daily")


# retorno y retorno acumulado ---------------------------------------------

# retornos 
retornos_activos <- data_activos %>% 
                       group_by(symbol) %>% 
                       tq_transmute(select = adjusted,
                                    mutate_fun = periodReturn,
                                    period = "daily",
                                    type = "log",
                                    col_rename = "retornos.diarios")
# retornos acumulados 
retornos_acum_activos <- retornos_activos %>% 
                            group_by(symbol) %>% 
                            mutate(ret.cum = cumsum(retornos.diarios))



# Grafico en conjunto retornos --------------------------------------------

retornos_activos %>% 
  ggplot(mapping = aes(x = retornos.diarios, fill = symbol))+
  geom_density(alpha = 0.5) +
  labs(title = "Retornos Activos", subtitle = "Oracle (ORCL), Intel (IT), Nvidia (NVDA) y Netflix (NFLX)",
       x = "Retornos diarios", y = "Densidad") + 
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ symbol, ncol = 2) + 
  guides(fill=guide_legend(title="Activos:"))


# Grafico en conjunto retornos acumulados ---------------------------------

retornos_acum_activos %>% 
     ggplot(mapping = aes(x = date, y = ret.cum/100, color = symbol)) +
     geom_line() + 
     labs(title = "Retornos Activos", subtitle = "Oracle (ORCL), Intel (IT), Nvidia (NVDA) y Netflix (NFLX)",
          x = "Periodo", y = "Retorno Acumulado") + 
     theme_tq() +
     scale_fill_tq() + 
     facet_wrap(~ symbol, ncol = 2) + 
     guides(color = guide_legend(title="Activos:")) + 
     scale_y_continuous(labels = scales::percent)


# Bandas de Bollinger -----------------------------------------------------

end <- as_date("2018-07-31")

sp500_precio %>% 
  ggplot(aes(x=date, y=close, open = open,
             high = high, low = low, close = close)) +
  geom_candlestick() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
  labs(title = "Standard & Poor 500 Candlestick Chart", 
       subtitle = "BBands con SMA", 
       y = "Closing Price", x = "") + 
  coord_x_date(xlim = c(end - weeks(24), end),
               ylim = c(2500, 3000)) + 
  theme_tq()



