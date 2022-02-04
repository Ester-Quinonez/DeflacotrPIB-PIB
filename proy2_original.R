# PES Programación II, Proyecto 2
# proy2_original.R
# Analisis impulso respuesta del deflactor del PIB y el PIB real de Guatemala
# Ester Quiñonez, Pablo Gutierrez, Joel Juarez
# mariaestermil@gmail.com, pablomation@gmail.com, joel771992@gmail.com

library (ggplot2)
library(tidyverse)
library(forecast)
library(dynlm)
library(mFilter)
library(rsample)
library(dplyr)
library(readxl)
library(vars)
library(corpcor)

#Limpieza 
#rm(list = ls() )

df_1 <- read_excel("C:\\Users\\maria\\Dropbox\\My PC (LAPTOP-RKVDS0KL)\\Desktop\\PES\\Programacion II\\proyecto_2\\PES P2 Proyecto Final 1\\Empalme_CNT_1T_2001_3T_2021.xlsx", sheet = "5", range = "I6:I110", col_names = TRUE)

df_1f <- df_1  %>% slice(-c(1, 6, 11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101))#pib real

df_2 <- read_excel("C:\\Users\\maria\\Dropbox\\My PC (LAPTOP-RKVDS0KL)\\Desktop\\PES\\Programacion II\\proyecto_2\\PES P2 Proyecto Final 1\\Empalme_CNT_1T_2001_3T_2021.xlsx", sheet = "7", range = "I6:I110", col_names = TRUE)

df_2f <- df_2  %>% slice(-c(1, 6, 11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101))#pib nominal



df= df_1f / df_2f # deflactor del pib
colnames(df) <- c("Deflactor")

#Ver si el orden de las columnas afecta/ cual debería ir primero 
df <- df %>% mutate(PIB_Trimestral = df_2f$`PIB Trimestral`)



# deflactor
# Selecting the data to produce a `ts` object
var.df = df %>%
   dplyr::select(Deflactor, PIB_Trimestral)

# Declaring a `ts` object
var.ts = ts(var.df, start = c(2001, 1), frequency = 4)
autoplot(var.ts, facets = T) +
  labs(x = "",
       y = "Growth (%)",
       title = "GDP and Private Sector Credit")


var.ts = var.ts[-((83-7+1):83),]
var.ts = ts(var.ts, start = c(2001, 1), frequency = 4)
autoplot(var.ts, facets=T) +
  labs(x = "",
       y = "Growth (%)",
       main = "GDP and Private Sector Credit")


#Deflactor
stl(var.ts[,1], s.window = 'periodic', s.degree = 1) %>%
  autoplot() +
  labs(title = 'PIB')

#PIB
stl(var.ts[,2], s.window = 'periodic', s.degree = 1) %>%
  autoplot() +
  labs(title = 'CSP')

forecast::ndiffs(var.ts[,'PIB_Trimestral'],
                 test = 'adf',
                 type = 'level',
                 max.d = 2)

forecast::ndiffs(var.ts[,'Deflactor'],
                 test = 'adf',
                 type = 'level',
                 max.d = 2)

#Modelo VAR
VARselect(var.ts, lag.max = 9, type = 'const')

var.mod = VAR(y = var.ts, p = 6, type = 'const')
summary(var.mod)

#Correlograma
resid(var.mod) %>%
  ggAcf() +
  labs(title = "VAR residuals correlograms")

#Para la matrices de Cholesky
cov.mat = resid(var.mod) %>%
  cov()

A.mat = chol(cov.mat, ) %>%
  t()
A.mat

#matriz first-Period IRF
psi <- Psi(var.mod, nstep = 1)[,,1]

Givens_general <- list()
Givens_correctos <- list()
contador <- 1
for (i in 1:1000){
  theta <- runif(1, 0, pi/2)
  Givens <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol=2, nrow = 2)
  Givens_general[[i]] <- Givens
  Aj <- psi%*%Givens
  if (Aj[1,2] < 0 && Aj[1,1]>0 && Aj[2,1]>0 && Aj[2,2]>0){
    Givens_correctos[[contador]] <- Givens
    contador <- contador + 1
  }
}

#matriz first-Period IRF para n shocks
n <- 1500
psi_nueva <- Psi(var.mod, nstep = n)

phi_lista <- list()
for (i in 1:n){
  aux <- list()
  for (j in 1:10){
    phi <- (psi_nueva[,,i])%*%Givens_correctos[[j]]
    aux[[j]] <- phi
  }
  phi_lista[[i]] <- aux
}

shocks <- 1:n

graph_a <- ggplot()
graph_b <- ggplot()
graph_c <- ggplot()
graph_d <- ggplot()

for (j in 1:length(phi_lista[[1]])){
  a <- c()
  b <- c()
  c <- c()
  d <- c()
  for (i in 1:n){
    a <- c(a, phi_lista[[i]][[j]][1,1])
    b <- c(b, phi_lista[[i]][[j]][1,2])
    c <- c(c, phi_lista[[i]][[j]][2,1])
    d <- c(d, phi_lista[[i]][[j]][2,2])
  }
  dfa <- data.frame(a,shocks)
  graph_a <- graph_a + geom_line(data=dfa, aes(x=shocks, y=a)) + labs(x = "tiempo", y = "")
  dfb <- data.frame(b,shocks)
  graph_b <- graph_b + geom_line(data=dfb, aes(x=shocks, y=b)) + labs(x = "tiempo", y = "")
  dfc <- data.frame(c,shocks)
  graph_c <- graph_c + geom_line(data=dfc, aes(x=shocks, y=c)) + labs(x = "tiempo", y = "")
  dfd <- data.frame(d,shocks)
  graph_d <- graph_d + geom_line(data=dfd, aes(x=shocks, y=d)) + labs(x = "tiempo", y = "")
}

graph_a
graph_b
graph_c
graph_d

# Para las gráficas de las medianas

medianas <- list(Givens_correctos[[median(1:length(Givens_correctos))]],Givens_general[[median(1:length(Givens_general))]])

phi_lista_med <- list()
for (i in 1:n){
  aux <- list()
  for (j in 1:2){
    phi <- (psi_nueva[,,i])%*%medianas[[j]]
    aux[[j]] <- phi
  }
  phi_lista_med[[i]] <- aux
}


graph_a_med <- ggplot()
graph_b_med <- ggplot()
graph_c_med <- ggplot()
graph_d_med <- ggplot()

for (j in 1:length(phi_lista_med[[1]])){
  a <- c()
  b <- c()
  c <- c()
  d <- c()
  for (i in 1:n){
    a <- c(a, phi_lista[[i]][[j]][1,1])
    b <- c(b, phi_lista[[i]][[j]][1,2])
    c <- c(c, phi_lista[[i]][[j]][2,1])
    d <- c(d, phi_lista[[i]][[j]][2,2])
  }
  if (j==1){
  dfa <- data.frame(a,shocks)
  graph_a_med <- graph_a_med + geom_line(data=dfa, aes(x=shocks, y=a)) + labs(x = "tiempo", y = "") 
  dfb <- data.frame(b,shocks)
  graph_b_med <- graph_b_med + geom_line(data=dfb, aes(x=shocks, y=b)) + labs(x = "tiempo", y = "")
  dfc <- data.frame(c,shocks)
  graph_c_med <- graph_c_med + geom_line(data=dfc, aes(x=shocks, y=c)) + labs(x = "tiempo", y = "")
  dfd <- data.frame(d,shocks)
  graph_d_med <- graph_d_med + geom_line(data=dfd, aes(x=shocks, y=d)) + labs(x = "tiempo", y = "")
  }
  if (j==2){
    dfa <- data.frame(a,shocks)
    graph_a_med <- graph_a_med + geom_line(data=dfa, aes(x=shocks, y=a), color="red") + labs(x = "tiempo", y = "") 
    dfb <- data.frame(b,shocks)
    graph_b_med <- graph_b_med + geom_line(data=dfb, aes(x=shocks, y=b), color="red") + labs(x = "tiempo", y = "")
    dfc <- data.frame(c,shocks)
    graph_c_med <- graph_c_med + geom_line(data=dfc, aes(x=shocks, y=c), color="red") + labs(x = "tiempo", y = "")
    dfd <- data.frame(d,shocks)
    graph_d_med <- graph_d_med + geom_line(data=dfd, aes(x=shocks, y=d), color="red") + labs(x = "tiempo", y = "") 
  }
}

graph_a_med
graph_b_med
graph_c_med
graph_d_med













