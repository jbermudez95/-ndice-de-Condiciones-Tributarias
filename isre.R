#Nombre:        isre.R
#Descripción:   Estimación del Indicador de Condiciones Tributarias para Honduras (ICT)
#Elaborado por: Jose Carlo Bermúdez
#Institución:   Servicio de Administración de Rentas, Departamento de Estudios Fiscales y Económicos
#Modificado:    23/7/2021

library(ggplot2)
library(readxl)
library(RJDemetra)
library(tseries)
library(tidyverse)
library(fpp3)
library(ggcorrplot)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)
library(ggthemes)
library(psych)
library(DescTools)
library(zoo)
library(forecast)

#===========================================
#       Preparación de los Datos
#===========================================

setwd("C:/Users/Owner/Desktop/2021/Notas técnicas y papers/Indicador de recaudación/estimaciones/isre")
output <- paste0("C:/Users/Owner/Desktop/2021/Notas técnicas y papers/Indicador de recaudación/estimaciones")
data   <- read_excel("data_isre.xlsx")

#Convirtiendo a Lempiras variables medidas en dólares (utilizando el tipo de cambio nominal de compra promedio del mes)

data$impor   <- data$import  * data$tcn
data$reme    <- data$remesas * data$tcn

#Deflactando variables nominales

data$ventas_defl  <- (data$ventas /data$ipc) * 100
data$gasto_defl   <- (data$gasto  /data$ipc) * 100
data$remesas_defl <- (data$reme   /data$ipc) * 100
data$m2_defl      <- (data$m2     /data$ipc) * 100
data$credito_defl <- (data$credito/data$ipc) * 100
data$import_defl  <- (data$impor  /data$ipc) * 100

#Desestacionalización mediante TRAMO SEATS de las variables con estacionalidad

imae    <- ts(data$imae,         start=c(2007,1), freq=12)
ventas  <- ts(data$ventas_defl,  start=c(2007,1), freq=12)
gasto   <- ts(data$gasto_defl,   start=c(2007,1), freq=12)
remesas <- ts(data$remesas_defl, start=c(2007,1), freq=12)
mwh     <- ts(data$mwh,          start=c(2007,1), freq=12)

sa_imae    <- tramoseats(imae,    spec="RSAfull")
sa_ventas  <- tramoseats(ventas,  spec="RSAfull")
sa_gasto   <- tramoseats(gasto,   spec="RSAfull")
sa_remesas <- tramoseats(remesas, spec="RSAfull")
sa_mwh     <- tramoseats(mwh,     spec="RSAfull")

imae_l     <- as.data.frame(sa_imae[["final"]][["series"]])
ventas_l   <- as.data.frame(sa_ventas[["final"]][["series"]])
gasto_l    <- as.data.frame(sa_gasto[["final"]][["series"]])
remesas_l  <- as.data.frame(sa_remesas[["final"]][["series"]])
mwh_l      <- as.data.frame(sa_mwh[["final"]][["series"]])

data$imae_sa     <- imae_l$sa
data$ventas_sa   <- ventas_l$sa
data$gasto_sa    <- gasto_l$sa
data$remesas_sa  <- remesas_l$sa
data$mwh_sa      <- mwh_l$sa

remove(gasto, imae, mwh, remesas, ventas)
rm(gasto_l, imae_l, mwh_l, remesas_l, ventas_l, sa_gasto, sa_imae, sa_mwh, sa_remesas, sa_ventas)

#Transformación a variaciones interanuales

data <- data %>% mutate(imae_v    = difference(data$imae_sa,      lag=12) / lag(data$imae_sa,      12))
data <- data %>% mutate(ventas_v  = difference(data$ventas_sa,    lag=12) / lag(data$ventas_sa,    12))
data <- data %>% mutate(gasto_v   = difference(data$gasto_sa,     lag=12) / lag(data$gasto_sa,     12))
data <- data %>% mutate(remesas_v = difference(data$remesas_sa,   lag=12) / lag(data$remesas_sa,   12))
data <- data %>% mutate(mwh_v     = difference(data$mwh_sa,       lag=12) / lag(data$mwh_sa,       12))
data <- data %>% mutate(import_v  = difference(data$import_defl,  lag=12) / lag(data$import_defl,  12))
data <- data %>% mutate(tcr_v     = difference(data$tcr,          lag=12) / lag(data$tcr,          12))
data <- data %>% mutate(wti_v     = difference(data$wti,          lag=12) / lag(data$wti,          12))
data <- data %>% mutate(cafe_v    = difference(data$café,         lag=12) / lag(data$café,         12))
data <- data %>% mutate(m2_v      = difference(data$m2_defl,      lag=12) / lag(data$m2_defl,      12))
data <- data %>% mutate(credit_v  = difference(data$credito_defl, lag=12) / lag(data$credito_defl, 12))

#Transformación a distribución normal estándar declarando las variables como series de tiempo

imae_z   <- (data$imae_v    - mean(data$imae_v,    na.rm = TRUE)) / sd(data$imae_v,    na.rm = TRUE)
ventas_z <- (data$ventas_v  - mean(data$ventas_v,  na.rm = TRUE)) / sd(data$ventas_v,  na.rm = TRUE)
gasto_z  <- (data$gasto_v   - mean(data$gasto_v,   na.rm = TRUE)) / sd(data$gasto_v,   na.rm = TRUE)
reme_z   <- (data$remesas_v - mean(data$remesas_v, na.rm = TRUE)) / sd(data$remesas_v, na.rm = TRUE)
mwh_z    <- (data$mwh_v     - mean(data$mwh_v,     na.rm = TRUE)) / sd(data$mwh_v,     na.rm = TRUE)
import_z <- (data$import_v  - mean(data$import_v,  na.rm = TRUE)) / sd(data$import_v,  na.rm = TRUE)
tcr_z    <- (data$tcr_v     - mean(data$tcr_v,     na.rm = TRUE)) / sd(data$tcr_v,     na.rm = TRUE)
wti_z    <- (data$wti_v     - mean(data$wti_v,     na.rm = TRUE)) / sd(data$wti_v,     na.rm = TRUE)
cafe_z   <- (data$cafe_v    - mean(data$cafe_v,    na.rm = TRUE)) / sd(data$cafe_v,    na.rm = TRUE)
m2_z     <- (data$m2_v      - mean(data$m2_v,      na.rm = TRUE)) / sd(data$m2_v,      na.rm = TRUE)
credit_z <- (data$credit_v  - mean(data$credit_v,  na.rm = TRUE)) / sd(data$credit_v,  na.rm = TRUE)
spread_z <- (data$spread    - mean(data$spread,    na.rm = TRUE)) / sd(data$spread,    na.rm = TRUE)

data_isre <- cbind(imae_z, ventas_z, gasto_z, reme_z, mwh_z, import_z, tcr_z, wti_z, cafe_z, m2_z, credit_z, spread_z)
data_isre <- data_isre[-c(1:12), ]
data_isre <- ts(data_isre, start = c(2008,1), freq=12)

remove(imae_z, ventas_z, gasto_z, reme_z, mwh_z, import_z, tcr_z, wti_z, cafe_z, m2_z, credit_z, spread_z)

#=====================================
#       Gráfico 9. Descriptivos 
#=====================================

data_isre_df <- as.data.frame(data_isre)
fecha <- data$date
fecha <- fecha[13:length(fecha)]
data_isre_df$date <- as.Date(fecha)

#Gráficos de las variables estandarizadas a través del tiempo

ts_imae <- ggplot(data_isre_df, aes(x=date, y=imae_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_ventas <- ggplot(data_isre_df, aes(x=date, y=ventas_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_gasto <- ggplot(data_isre_df, aes(x=date, y=gasto_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_reme <- ggplot(data_isre_df, aes(x=date, y=reme_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_mwh <- ggplot(data_isre_df, aes(x=date, y=mwh_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_import <- ggplot(data_isre_df, aes(x=date, y=import_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_tcr <- ggplot(data_isre_df, aes(x=date, y=tcr_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_wti <- ggplot(data_isre_df, aes(x=date, y=wti_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_cafe <- ggplot(data_isre_df, aes(x=date, y=cafe_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_m2 <- ggplot(data_isre_df, aes(x=date, y=m2_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_credit <- ggplot(data_isre_df, aes(x=date, y=credit_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

ts_spread <- ggplot(data_isre_df, aes(x=date, y=spread_z)) +
  geom_line(colour="BLUE", alpha = 0.6, size = 1) +
  geom_hline(yintercept=0, color = "black", size =0.5) +
  xlab("") + ylab("") + theme_light() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

setwd(output) #Guardando los gráficos en el directorio 

pdf("ts_imae.pdf", width = 4, height = 4)
ts_imae
dev.off()

pdf("ts_ventas.pdf", width = 4, height = 4)
ts_ventas
dev.off()

pdf("ts_gasto.pdf", width = 4, height = 4)
ts_gasto
dev.off()

pdf("ts_reme.pdf", width = 4, height = 4)
ts_reme
dev.off()

pdf("ts_mwh.pdf", width = 4, height = 4)
ts_mwh
dev.off()

pdf("ts_import.pdf", width = 4, height = 4)
ts_import
dev.off()

pdf("ts_tcr.pdf", width = 4, height = 4)
ts_tcr
dev.off()

pdf("ts_wti.pdf", width = 4, height = 4)
ts_wti
dev.off()

pdf("ts_cafe.pdf", width = 4, height = 4)
ts_cafe
dev.off()

pdf("ts_m2.pdf", width = 4, height = 4)
ts_m2
dev.off()

pdf("ts_credit.pdf", width = 4, height = 4)
ts_credit
dev.off()

pdf("ts_spread.pdf", width = 4, height = 4)
ts_spread
dev.off()

#GrÃ¡fico 10 de matriz de correlaciÃ³n

corr <- round(cor(data_isre_df[, 1:12]), 1)
corr_plot <- ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

pdf("corr_plot.pdf")
corr_plot
dev.off()

#=======================================================
#       Análisis de Componentes Principales (ACP)
#=======================================================

ict.cov   <- cov(data_isre)
ict.eigen <- eigen(ict.cov)
ict.acp   <- ict.eigen$vectors
row.names(ict.acp) <- c("imae", "ventas", "gasto", "reme", "mwh", "import", "tcr", "wti", "cafe", "M2", "credit", "spread")
colnames(ict.acp)  <- c("CP1","CP2","CP3","CP4","CP5","CP6","CP7","CP8","CP9","CP10","CP11","CP12")

CP1  <- as.matrix(data_isre) %*% ict.acp[,1]
CP2  <- as.matrix(data_isre) %*% ict.acp[,2]
CP3  <- as.matrix(data_isre) %*% ict.acp[,3]
CP4  <- as.matrix(data_isre) %*% ict.acp[,4]
CP5  <- as.matrix(data_isre) %*% ict.acp[,5]
CP6  <- as.matrix(data_isre) %*% ict.acp[,6]
CP7  <- as.matrix(data_isre) %*% ict.acp[,7]
CP8  <- as.matrix(data_isre) %*% ict.acp[,8]
CP9  <- as.matrix(data_isre) %*% ict.acp[,9]
CP10 <- as.matrix(data_isre) %*% ict.acp[,10]
CP11 <- as.matrix(data_isre) %*% ict.acp[,11]
CP12 <- as.matrix(data_isre) %*% ict.acp[,12]

#Proporción de Varianza Acumulada
PVE <- ict.eigen$values / sum(ict.eigen$values) 

#Estimación del número óptimo de componentes principales para calcular el ICT
set.seed(123)                       
parallel = fa.parallel(data_isre,
                       fm = 'ml',
                       fa = 'pc',
                       n.iter = 100,
                       SMC = TRUE,
                       quant = .95)

PC_Observado            <- parallel$pc.values
para_ict                <- as.data.frame(PC_Observado)
para_ict$PC_Simulado    <- parallel$pc.sim
para_ict$PC_Remuestreo  <- parallel$pc.simr
para_ict$uno            <- c(1:12)

#Estimación del Índice de Condiciones Tributarias
CP     <- cbind(CP1, CP2, CP3)
lambda <- t(cbind(ict.acp[1], ict.acp[2], ict.acp[3]))

ICT      <- as.data.frame(CP %*% lambda)
names(ICT)[names(ICT) == "V1"] <- "ICT"
ICT$date <- data_isre_df$date

#Descomposición del Índice de COndiciones Tributarias por 3 primeros Componentes Principales 
share       = t(ict.eigen$values[1:3] / sum(ict.eigen$values[1:3]))
ict_cp      = as.data.frame(ICT$ICT %*% share)
ict_cp$ICT  = ICT$ICT
ict_cp$date = ICT$date
names(ict_cp)[names(ict_cp) == "V1"] <- "CP1"
names(ict_cp)[names(ict_cp) == "V2"] <- "CP2"
names(ict_cp)[names(ict_cp) == "V3"] <- "CP3"
write.xlsx(ict_cp, "dataicp,xlsx")           #Exporto y manipulo la base manualmente para ordenarla 
dataicp   <- read_excel("dataicp.xlsx") 

#DescomposiciÃ³n del Índice de Condiciones Tributarias por cada Variable
media_var    <- rowMeans(ict.acp[,1:3])
share1       <- t(media_var / sum(media_var))
ict_var      <- as.data.frame(ICT$ICT %*% share1)
ict_var$date <- ICT$date
write.xlsx(ict_var, "dataicp1.xlsx")         #Exporto y manipulo la base manualmente para ordenarla
dataicp1   <- read_excel("dataicp1.xlsx")

#Descomposición del Índice de Condiciones Tributarias por cada sector económico
Real         <- media_var[1] + media_var[2] + media_var[5]                                
Externo      <- media_var[4] + media_var[6] + media_var[7] + media_var[8] + media_var[9]   
Financiero   <- media_var[10] + media_var[11] + media_var[12]
Fiscal       <- media_var[3]
media_sector <- cbind(Real, Externo, Financiero, Fiscal)
share2       <- media_sector / sum(media_sector)
ict_sector   <- as.data.frame(ICT$ICT %*% share2)
ict_sector$date <- ICT$date
write.xlsx(ict_sector, "dataicp2.xlsx")    #Exporto y manipulo la base manualmente para ordenarla
dataicp2     <- read_excel("dataicp2.xlsx")

#Factor Loadings por Variable para los 3 CP
loadingCP1 <- data.frame("CP1" = ict.eigen$vectors[,1])
loadingCP2 <- data.frame("CP2" = ict.eigen$vectors[,2])
loadingCP3 <- data.frame("CP3" = ict.eigen$vectors[,3])
loadingCP1$var <- c("imae", "ventas", "gasto", "reme", "mwh", "import", "tcr", "wti", "cafe", "M2", "credit", "spread")
loadingCP2$var <- c("imae", "ventas", "gasto", "reme", "mwh", "import", "tcr", "wti", "cafe", "M2", "credit", "spread")
loadingCP3$var <- c("imae", "ventas", "gasto", "reme", "mwh", "import", "tcr", "wti", "cafe", "M2", "credit", "spread")

#Análisis graáfico a partir del ACP para las estimaciones del ICT
# Gráfico 1
para <- ggplot(para_ict, aes(x=uno)) +
        geom_line(aes(y=PC_Observado,  colour = "PC_Observado"), size=1) +
        geom_point(aes(y=PC_Observado), colour = "BLUE", size = 1.5) +
        geom_line(aes(y=PC_Simulado, colour = "PC_Simulado"), linetype="dashed", size =1) +
        geom_line(aes(y=PC_Remuestreo, colour = "PC_Remuestreo"), linetype="dotted", size =1) +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
        scale_colour_manual("", 
                      breaks = c("PC_Observado", "PC_Simulado", "PC_Remuestreo"),
                      values = c("PC_Observado"="blue", "PC_Simulado"="red", 
                                 "PC_Remuestreo"="green")) +
        ylab("Valores Propios") + xlab("NÃƒÂºmero de Componentes") + theme_classic() + 
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.78,0.78))


# Gráfico 2a
pve <- qplot(c(1:12), PVE) +
       geom_line(colour="BLUE", linetype = "dotted", size = 1.2) +
       geom_point(colour="BLUE", size=3) +
       xlab("Componente Principal") +
       ylab("ProporciÃ³n de Varianza Explicada") +
       ylim(0,1) +
       scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
       theme_classic() +
       theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))


# Gráfico 2b
pve_cum <- qplot(c(1:12), cumsum(PVE)) +
           geom_line(colour="BLUE", linetype = "dotted", size = 1.2) +
           geom_point(colour="BLUE", size=3) +
           xlab("Componente Principal") +
           ylab("ProporciÃ³n de Varianza Explicada") +
           ylim(0,1) +
           scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
           theme_classic() +
           theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

# Gráfico 3
ict <- ggplot(ICT, aes(x=date, y=ICT)) +
       geom_hline(yintercept=0, color = "black", size =0.5) +
       geom_hline(yintercept=1, color = "red", linetype="dashed", size =0.5) +
       geom_hline(yintercept=-1, color = "red", linetype="dashed", size =0.5) +
       geom_area(fill="blue2", alpha = 0.6, size = 1) +
       xlab("") + ylab("") + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

# Gráfico 6
ict_cp1 <- ggplot() + geom_bar(data = dataicp, aes(x=date, y=value, fill=var), stat = 'identity') +
           geom_hline(yintercept=0, color = "black", size =0.5) +
           xlab("") + ylab("") + theme_classic() + 
           scale_fill_manual(values = c("royalblue","cyan","navy")) +
           theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
                 legend.position="bottom", legend.direction="horizontal", 
                 legend.title = element_blank(), legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-10,-10,-10,-10)) 
# Grafico 4
ict_var <- ggplot() + geom_bar(data = dataicp1, aes(x=date, y=val, fill=var), stat = 'identity') +
           geom_hline(yintercept=0, color = "black", size =0.5) +
           xlab("") + ylab("") + theme_classic() + 
           scale_fill_viridis_d(option  = "magma") +
           theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
                 legend.position="bottom", legend.direction="horizontal", 
                 legend.title = element_blank(), legend.margin=margin(0,0,0,0),
                 legend.box.margin=margin(-2,-2,-2,-2))

# Gráfico 5
ict_sector <- ggplot() + geom_bar(data = dataicp2, aes(x=date, y=value, fill=var), stat = 'identity') +
              geom_hline(yintercept=0, color = "black", size =0.5) +
              xlab("") + ylab("") + theme_classic() + 
              scale_fill_manual(values = c("royalblue","cyan","navy","blue")) +
              theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
                    legend.position="bottom", legend.direction="horizontal", 
                    legend.title = element_blank(), legend.margin=margin(0,0,0,0),
                    legend.box.margin=margin(-2,-2,-2,-2))

load1 <- ggplot(data = loadingCP1, aes(x=var, y=CP1)) + geom_bar(stat = "identity") + coord_flip() +
         geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") + theme_light() + 
         scale_fill_viridis_d(option  = "magma") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

load2 <- ggplot(data = loadingCP2, aes(x=var, y=CP2)) + geom_bar(stat = "identity") + coord_flip() +
         geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") + theme_light() + 
         scale_fill_viridis_d(option  = "magma") + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

load3 <- ggplot(data = loadingCP3, aes(x=var, y=CP3)) + geom_bar(stat = "identity") + coord_flip() +
         geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") + theme_light() + 
         theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

setwd(output)

pdf("pve.pdf", width = 4, height = 4)
pve
dev.off()

pdf("pve_cum.pdf", width = 4, height = 4)
pve_cum
dev.off()

pdf("para.pdf", width = 4, height = 4)
para
dev.off()

pdf("ict.pdf", width = 5, height = 4)
ict
dev.off() 

pdf("ict_cp.pdf", width = 5, height = 4)
ict_cp1
dev.off() 

pdf("ict_var.pdf", width = 5, height = 4)
ict_var
dev.off() 

pdf("ict_sector.pdf", width = 5, height = 4)
ict_sector
dev.off() 

pdf("load1.pdf", width = 4, height = 5)
load1
dev.off() 

pdf("load2.pdf", width = 4, height = 5)
load2
dev.off() 

pdf("load3.pdf", width = 4, height = 5)
load3
dev.off() 

#=======================================================
#                   Hechos Estilizados
#=======================================================

data_itrib   <- read_excel("data_itrib.xlsx")

itrib   <- ts(data_itrib$itrib,        start=c(2007,1), freq=12)
dire    <- ts(data_itrib$dire,         start=c(2007,1), freq=12)
isv     <- ts(data_itrib$isv,          start=c(2007,1), freq=12)
acpv    <- ts(data_itrib$acpv,         start=c(2007,1), freq=12)
dai     <- ts(data_itrib$dai,          start=c(2007,1), freq=12)
resto   <- ts(data_itrib$resto,        start=c(2007,1), freq=12)

sa_itrib   <- tramoseats(itrib,   spec="RSAfull")
sa_dire    <- tramoseats(dire,    spec="RSAfull")
sa_isv     <- tramoseats(isv,     spec="RSAfull")
sa_acpv    <- tramoseats(acpv,    spec="RSAfull")
sa_dai     <- tramoseats(dai,     spec="RSAfull")
sa_resto   <- tramoseats(resto,   spec="RSAfull")

itrib_l    <- as.data.frame(sa_itrib[["final"]][["series"]])
dire_l     <- as.data.frame(sa_dire[["final"]][["series"]])
isv_l      <- as.data.frame(sa_isv[["final"]][["series"]])
acpv_l     <- as.data.frame(sa_acpv[["final"]][["series"]])
dai_l      <- as.data.frame(sa_dai[["final"]][["series"]])
resto_l    <- as.data.frame(sa_resto[["final"]][["series"]])

data_itrib$itrib_sa    <- itrib_l$sa
data_itrib$dire_sa     <- dire_l$sa
data_itrib$isv_sa      <- isv_l$sa
data_itrib$acpv_sa     <- acpv_l$sa
data_itrib$dai_sa      <- dai_l$sa
data_itrib$resto_sa    <- resto_l$sa

remove(itrib, dire, isv, acpv, dai, resto, 
      acpv_l, dai_l, dire_l, isv_l, itrib_l, resto_l)
rm(sa_itrib, sa_dire, sa_isv, sa_acpv, sa_dai, sa_resto)

data_itrib <- data_itrib %>% mutate(itrib_v   = difference(data_itrib$itrib_sa,     lag=12) / lag(data_itrib$itrib_sa,     12))
data_itrib <- data_itrib %>% mutate(dire_v    = difference(data_itrib$dire_sa,      lag=12) / lag(data_itrib$dire_sa,      12))
data_itrib <- data_itrib %>% mutate(isv_v     = difference(data_itrib$isv_sa,       lag=12) / lag(data_itrib$isv_sa,       12))
data_itrib <- data_itrib %>% mutate(acpv_v    = difference(data_itrib$acpv_sa,      lag=12) / lag(data_itrib$acpv_sa,      12))
data_itrib <- data_itrib %>% mutate(dai_v     = difference(data_itrib$dai_sa,       lag=12) / lag(data_itrib$dai_sa,       12))
data_itrib <- data_itrib %>% mutate(resto_v   = difference(data_itrib$resto_sa,     lag=12) / lag(data_itrib$resto_sa,     12))

itrib_ma <- rollmean(data_itrib$itrib_v, 12)
dire_ma  <- rollmean(data_itrib$dire_v,  12)
isv_ma   <- rollmean(data_itrib$isv_v,   12)
acpv_ma  <- rollmean(data_itrib$acpv_v,  12)
dai_ma   <- rollmean(data_itrib$dai_v,   12)
resto_ma <- rollmean(data_itrib$resto_v, 12)

ICT$itrib_ma <- itrib_ma[13:length(itrib_ma)]
ICT$dire_ma  <- dire_ma[13:length(dire_ma)]
ICT$isv_ma   <- isv_ma[13:length(isv_ma)]
ICT$acpv_ma  <- acpv_ma[13:length(acpv_ma)]
ICT$dai_ma   <- dai_ma[13:length(dai_ma)]
ICT$resto_ma <- resto_ma[13:length(resto_ma)]

#Correlaciones Dinámicas y Desviaciones Estándar de los Parámetros

corr1 <- ccf(ICT$ICT, ICT$itrib_ma, lag.max = 12, type = c("correlation"), plot = FALSE)
corr2 <- ccf(ICT$ICT, ICT$dire_ma,  lag.max = 12, type = c("correlation"), plot = FALSE)
corr3 <- ccf(ICT$ICT, ICT$isv_ma,   lag.max = 12, type = c("correlation"), plot = FALSE)
corr4 <- ccf(ICT$ICT, ICT$acpv_ma,  lag.max = 12, type = c("correlation"), plot = FALSE)
corr5 <- ccf(ICT$ICT, ICT$dai_ma,   lag.max = 12, type = c("correlation"), plot = FALSE)
corr6 <- ccf(ICT$ICT, ICT$resto_ma, lag.max = 12, type = c("correlation"), plot = FALSE)
remove(itrib_ma, dire_ma, isv_ma, acpv_ma, dai_ma, resto_ma)

sd_c1_r <- sqrt((1-(corr1$acf[12])^2) / (corr1$n.used-2))
sd_c1_c <- sqrt((1-(corr1$acf[13])^2) / (corr1$n.used-2))
sd_c1_a <- sqrt((1-(corr1$acf[14])^2) / (corr1$n.used-2))

sd_c2_r <- sqrt((1-(corr2$acf[12])^2) / (corr2$n.used-2))
sd_c2_c <- sqrt((1-(corr2$acf[13])^2) / (corr2$n.used-2))
sd_c2_a <- sqrt((1-(corr2$acf[14])^2) / (corr2$n.used-2))

sd_c3_r <- sqrt((1-(corr3$acf[12])^2) / (corr3$n.used-2))
sd_c3_c <- sqrt((1-(corr3$acf[13])^2) / (corr3$n.used-2))
sd_c3_a <- sqrt((1-(corr3$acf[14])^2) / (corr3$n.used-2))

sd_c4_r <- sqrt((1-(corr4$acf[12])^2) / (corr4$n.used-2))
sd_c4_c <- sqrt((1-(corr4$acf[13])^2) / (corr4$n.used-2))
sd_c4_a <- sqrt((1-(corr4$acf[14])^2) / (corr4$n.used-2))

sd_c5_r <- sqrt((1-(corr5$acf[12])^2) / (corr5$n.used-2))
sd_c5_c <- sqrt((1-(corr5$acf[13])^2) / (corr5$n.used-2))
sd_c5_a <- sqrt((1-(corr5$acf[14])^2) / (corr5$n.used-2))

sd_c6_r <- sqrt((1-(corr6$acf[12])^2) / (corr6$n.used-2))
sd_c6_c <- sqrt((1-(corr6$acf[13])^2) / (corr6$n.used-2))
sd_c6_a <- sqrt((1-(corr6$acf[14])^2) / (corr6$n.used-2))

rm(sd_c1_a, sd_c1_c, sd_c1_r, sd_c2_a, sd_c2_c, sd_c2_r, sd_c3_a, 
   sd_c3_c, sd_c3_r, sd_c4_a, sd_c4_c, sd_c4_r, sd_c5_a, sd_c5_c, sd_c5_r,
   corr1, corr2, corr3, corr4, corr5, corr6)

ICT$itrib_z <- (ICT$itrib_ma    - mean(ICT$itrib_ma)) / sd(ICT$itrib_ma)
ICT$dire_z  <- (ICT$dire_ma     - mean(ICT$dire_ma))  / sd(ICT$dire_ma)
ICT$isv_z   <- (ICT$isv_ma      - mean(ICT$isv_ma))   / sd(ICT$isv_ma)
ICT$acpv_z  <- (ICT$acpv_ma     - mean(ICT$acpv_ma))  / sd(ICT$acpv_ma)
ICT$dai_z   <- (ICT$dai_ma      - mean(ICT$dai_ma))   / sd(ICT$dai_ma)
ICT$resto_z <- (ICT$resto_ma    - mean(ICT$resto_ma)) / sd(ICT$resto_ma)

rectangle  <- data.frame(xmin = as.Date(c("2008-03-01")),
                        xmax = as.Date(c("2010-01-01")),
                        ymin = -Inf, ymax = Inf)

rectangle1 <- data.frame(xmin = as.Date(c("2014-01-01")),
                         xmax = as.Date(c("2014-12-01")),
                         ymin = -Inf, ymax = Inf)

rectangle2 <- data.frame(xmin = as.Date(c("2020-03-01")),
                         xmax = as.Date(c("2020-12-01")),
                         ymin = -Inf, ymax = Inf)

# Grafico 7
hechos <- ggplot() + geom_rect(data = rectangle, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                fill = "gray95", alpha = 0.5) +
          geom_rect(data = rectangle1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    fill = "gray95", alpha = 0.5) +
          geom_rect(data = rectangle2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    fill = "gray95", alpha = 0.5) +
          geom_line(data = ICT, aes(x = date, y = itrib_z, colour="itrib_z"), size = 1) +
          geom_line(data = ICT, aes(x = date, y = ICT, colour="ICT"), size = 1) +
          geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") +
          scale_colour_manual("", 
                              breaks = c("itrib_z", "ICT"),
                              values = c("itrib_z"="red", "ICT"="blue")) + theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.75,0.2))

# Grafico 8a
h_dire <- ggplot() + 
          geom_line(data = ICT, aes(x = date, y = dire_z, colour="dire_z"), size = 1) +
          geom_line(data = ICT, aes(x = date, y = ICT, colour="ICT"), size = 1) +
          geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") +
          scale_colour_manual("", 
                              breaks = c("dire_z", "ICT"),
                              values = c("dire_z"="red", "ICT"="blue")) + theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.75,0.2))

# Grafico 8b
h_isv <- ggplot() + 
         geom_line(data = ICT, aes(x = date, y = isv_z, colour="isv_z"), size = 1) +
         geom_line(data = ICT, aes(x = date, y = ICT, colour="ICT"), size = 1) +
         geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") +
         scale_colour_manual("", 
                      breaks = c("isv_z", "ICT"),
                      values = c("isv_z"="red", "ICT"="blue")) + theme_classic() +
         theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.75,0.2))

# Grafico 8c
h_acpv <- ggplot() + 
          geom_line(data = ICT, aes(x = date, y = acpv_z, colour="acpv_z"), size = 1) +
          geom_line(data = ICT, aes(x = date, y = ICT, colour="ICT"), size = 1) +
          geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") +
          scale_colour_manual("", 
                              breaks = c("acpv_z", "ICT"),
                              values = c("acpv_z"="red", "ICT"="blue")) + theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.75,0.2))

# Grafico 8d
h_dai <- ggplot() + 
         geom_line(data = ICT, aes(x = date, y = dai_z, colour="dai_z"), size = 1) +
         geom_line(data = ICT, aes(x = date, y = ICT, colour="ICT"), size = 1) +
         geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") +
         scale_colour_manual("", 
                            breaks = c("dai_z", "ICT"),
                            values = c("dai_z"="red", "ICT"="blue")) + theme_classic() +
         theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.75,0.2))

# Grafico 8e
h_resto <- ggplot() + 
           geom_line(data = ICT, aes(x = date, y = resto_z, colour="resto_z"), size = 1) +
           geom_line(data = ICT, aes(x = date, y = ICT, colour="ICT"), size = 1) +
           geom_hline(yintercept=0, color = "black", size =0.5) + xlab("") + ylab("") +
           scale_colour_manual("", 
                               breaks = c("resto_z", "ICT"),
                               values = c("resto_z"="red", "ICT"="blue")) + theme_classic() +
           theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), legend.position = c(0.75,0.2))

setwd(output)

pdf("h_itrib.pdf", width = 5, height = 4)
hechos
dev.off() 

pdf("h_dire.pdf", width = 4, height = 4)
h_dire
dev.off() 

pdf("h_isv.pdf", width = 4, height = 4)
h_isv
dev.off() 

pdf("h_acpv.pdf", width = 4, height = 4)
h_acpv
dev.off() 

pdf("h_dai.pdf", width = 4, height = 4)
h_dai
dev.off() 

pdf("h_resto.pdf", width = 4, height = 4)
h_resto
dev.off() 

#=======================================================
#               Análisis de Pronóstico
#=======================================================

# Preparación de las bases para estimar los modelos y las pruebas en Stata

Y <- data.frame("ict" = ICT$ICT, "itrib" = data_itrib$itrib_v[25:180], 
                "imae" =data$imae_v[13:168], "mwh" = data$mwh_v[13:168], "date" = data$date[13:168])
write.xlsx(Y, "base_fcst.xlsx")

