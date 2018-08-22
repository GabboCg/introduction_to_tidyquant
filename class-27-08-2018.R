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
                       tq_transmute(select = close,
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



# Bar Chart ---------------------------------------------------------------

end <- as_date("2018-07-31")

sp500_precio %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close),
                  color_up = "darkgreen", color_down = "darkred", size = 1) +
    labs(title = "S&P 500 Bar Chart", 
         subtitle = "Con un zoom 6 semanas",
         y = "Precio de Cierre", x = "") + 
    coord_x_date(xlim = c(end - weeks(6), end),
                 ylim = c(2500, 3000)) + 
    theme_tq()


# Candlestick Chart -------------------------------------------------------

sp500_precio %>%
     ggplot(aes(x = date, y = close)) +
     geom_candlestick(aes(open = open, high = high, low = low, close = close),
                      color_up = "darkgreen", color_down = "darkred", 
                      fill_up  = "darkgreen", fill_down  = "darkred") +
     labs(title = "S&P 500 Candlestick Chart", 
          subtitle = "Con un zoom 6 semanas",
          y = "Precio de Cierre", x = "") + 
     coord_x_date(xlim = c(end - weeks(6), end),
                  ylim = c(2500, 3000)) + 
     theme_tq()


# Bandas de Bollinger con quantmod ----------------------------------------

chartSeries(GSPC, subset = "last 3 months")
addBBands()


# Bandas de Bollinger con tidyquant ---------------------------------------

# fijamos las cordenadas
end <- as_date("2018-07-31")

# Construimos las bandas por separdos
sp500_bbands <- sp500_precio %>%
                mutate(sma = runMean(close, n = 20), sdev = runSD(close, 20, sample = FALSE),
                       up = sma + 2*sdev, dn = sma - 2*sdev) %>% 
                na.omit()

# graficamos
sp500_bbands %>% 
      ggplot(aes(x=date, y=close, open = open,
                 high = high, low = low, close = close)) + 
      geom_line(aes(date, up), linetype = "dashed") +
      geom_line(aes(date, dn), linetype = "dashed") +
      geom_line(aes(date, sma), linetype = "dashed") +
      geom_candlestick() +
      labs(title = "Standard & Poor 500 Candlestick Chart", 
           subtitle = "BBands con SMA", 
           y = "Closing Price", x = "") + 
      theme_bw() + 
      coord_x_date(xlim = c(end - weeks(12), end),
                   ylim = c(2500, 3000)) 

sp500_precio %>% 
      ggplot(aes(x=date, y=close, open = open,
                 high = high, low = low, close = close)) +
      geom_candlestick() +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
      labs(title = "Standard & Poor 500 Candlestick Chart", 
           subtitle = "BBands con SMA", 
           y = "Closing Price", x = "") + 
      coord_x_date(xlim = c(end - weeks(12), end),
                   ylim = c(2500, 3000)) + 
      theme_tq()


# Multiples datos con quantmod --------------------------------------------

tickers <- c("ORCL", "IT", "NVDA", "NFLX")

getSymbols(tickers, 
           src = "yahoo", 
           from = "2010-01-01", 
           to = "2018-08-01", 
           periodicity = "daily")

orcl <- as.data.frame(ORCL) %>% 
        select(ORCL.Close) %>% 
        rename(close = ORCL.Close) %>% 
        mutate(date = index(ORCL), return = log(close/lag(close,1)), symbol = "orcl",
               return = ifelse(is.na(return),0,return))  
        
it <- as.data.frame(IT) %>% 
      select(IT.Close) %>% 
      rename(close = IT.Close) %>% 
      mutate(date = index(IT), return = log(close/lag(close,1)), symbol = "it",
             return = ifelse(is.na(return),0,return))    

nvda <- as.data.frame(NVDA) %>% 
        select(NVDA.Close) %>% 
        rename(close = NVDA.Close) %>% 
        mutate(date = index(NVDA), return = log(close/lag(close,1)), symbol = "nvda",
               return = ifelse(is.na(return),0,return))   

nflx <- as.data.frame(NFLX) %>% 
        select(NFLX.Close) %>% 
        rename(close = NFLX.Close) %>% 
        mutate(date = index(NFLX), return = log(close/lag(close,1)), symbol = "nflx",
               return = ifelse(is.na(return),0,return))   

rm("ORCL", "IT", "NVDA", "NFLX")

return_data <- rbind(orcl, it, nvda, nflx) %>% 
               select(date, symbol, close, return)

acumulado_data <- return_data %>% 
                  group_by(symbol) %>% 
                  mutate(acumulado = cumsum(return))
  
