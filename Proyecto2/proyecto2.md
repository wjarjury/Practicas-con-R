Proyecto2
================
Ing Winston Jarjury Msc
2024-05-25

# Cargando las librerias necesarias

``` r
library(openxlsx)
library(reshape2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats 1.0.0     ✔ stringr 1.5.1
    ## ✔ purrr   1.0.2     ✔ tibble  3.2.1
    ## ✔ readr   2.1.5     ✔ tidyr   1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidyquant)
```

    ## Loading required package: PerformanceAnalytics
    ## Loading required package: xts
    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################
    ## 
    ## Attaching package: 'xts'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last
    ## 
    ## 
    ## Attaching package: 'PerformanceAnalytics'
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend
    ## 
    ## Loading required package: quantmod
    ## Loading required package: TTR
    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

# Generamos nuestro dataset base para trabajar

``` r
comercio=read.xlsx("C:\\Users\\Winston\\Downloads\\Bolivia - Comercio Exterior segun Año y Mes, 1992 - 2024.xlsx",
                        sheet = "COMEX MENSUAL 92-24",detectDates = TRUE)


produccion=read.xlsx("C:\\Users\\Winston\\Downloads\\1. Bolivia - Produccion de Petroleo y Gas Natural segun Año y Mes 1990 - 2024.xlsx",
                   sheet = "H01",detectDates = TRUE)

produccion=produccion %>% 
  select(-PERÍODO)

bolivia=cbind(comercio,produccion)

View(bolivia)
```

``` r
agrupados=bolivia %>% 
  mutate(anio=year(Periodo),
         mes=month(Periodo))
```

# Generamos graficos de columnas apiladas

``` r
ggplot(data=agrupados,aes(x=as.factor(anio),y=Exportaciones),fill=as.factor(anio))+
  geom_boxplot(color="#131313")+
  labs(title="Diagrama de caja y bigotes de las exportaciones en Bolivia",
       subtitle = "En millones dólares estadounidenses",
       caption = "Fuente: Banco central de Bolivia\nElaboracion: Autor",
       x="Años")+
  theme(axis.text.x=element_text(angle=90,hjust = 1))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
nuevoagrupado= agrupados %>% 
  select(-mes) %>% 
  melt(id.vars = "anio") %>% 
  filter(variable=="Exportaciones" | variable=="Importaciones(2)" )
```

    ## Warning: attributes are not identical across measure variables; they will be
    ## dropped

``` r
ggplot(data=nuevoagrupado,aes(x=as.factor(anio),y=value,fill=variable))+
  geom_col(position = "dodge")+
  labs(title="Grafico de barras no apiladas por exportaciones e importaciones",
       subtitle = "En millones dólares estadounidenses",
       caption = "Fuente: Banco central de Bolivia\nElaboracion: Autor",
       x="Años",y="Cifras")+
  theme(axis.text.x=element_text(angle=90,hjust = 1))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
bolivia2=bolivia %>% 
  melt(id.vars = "Periodo") %>% 
  filter(variable=="Exportaciones" | variable=="Importaciones(2)" )


ggplot(data=bolivia2,aes(x=Periodo,y=value))+
  geom_line()+
  facet_grid(.~variable,scales = "free")+
  geom_hline(yintercept = mean(bolivia2$value), col = "grey")+
  geom_ma(aes(color="MA(12)"),ma_fun = SMA,
          n = 12, size = 1,
          show.legend = TRUE)+
  labs(title="Grafico temporal entre exportaciones e importaciones",
       subtitle = "En millones dólares estadounidenses",
       caption = "Fuente: Banco central de Bolivia\nElaboracion: Autor",
       x="Años",y="Cifras")
```

    ## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](proyecto2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
