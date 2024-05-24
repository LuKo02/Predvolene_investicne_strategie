# Predvolené investičné stratégie - Bakalárska práca - 2024 
# Generovanie denných výnosov dôchodkových fondov ####
# Načítanie potrebných knižníc a nastavenie konzoly:
# Jednotlivé knižnice je potrebné si stiahnuť
library(quantmod) # Stiahnutie dát z finance.yahoo.com
library(readxl) # Načítanie dát z excelu
library(quadprog) # Kvadratická optimalizácia
options(max.print = 20000) # Maximálny počet vypísaných prvkov   

# Historické dáta pre index S&P 500 v období od 1.1.2003 do 31.12.2023:
getSymbols(Symbols = "^GSPC",      
           from = "2003-01-01",
           to = "2023-12-31", 
           auto.assign = TRUE)

# Historické dáta pre aktívum iShares 1-3 Year Treasury Bond ETF v období od 1.1.2003 do 31.12.2023
getSymbols(Symbols = "SHY",      
           from = "2003-01-01",
           to = "2023-12-31", 
           auto.assign = TRUE)

# Spracovanie dát:
X <- merge(GSPC$GSPC.Adjusted,
           SHY$SHY.Adjusted)
X <- as.matrix(X)

# Nájdenie indexov riadkov, ktoré majú všetky hodnoty vyplnené:
ind <- which(rowSums(is.na(X)) == 0)

# Odstránenie nežiadúcich riadkov bez vyplnených hodnôt:
X <- X[ind,]

# Vykreslenie vývoja hodnoty S&P 500 a aktíva SHY v období od 1.1.2003 do 31.12.2023:
datumy <- as.Date(rownames(X))
windows()
plot(x = as.numeric(datumy), y = X[,1],col = "blue", type = "l", ylim = c(min(X[,1]),max(X[,1])), xlab = "Rok", ylab = "Hodnota S&P 500 v USD", xaxt='n', main = "Vývoj hodnoty S&P 500 v období od 1.1.2003 do 31.12.2023")
axis(1, datumy, format(datumy, "%Y"), cex.axis = .7, tick = FALSE)
windows()
plot(x = as.numeric(datumy), y = X[,2],col = "red", type = "l", ylim = c(min(X[,2]),max(X[,2])), xlab = "Rok", ylab = "Hodnota SHY v USD", xaxt='n', main = "Vývoj hodnoty SHY v období od 1.1.2003 do 31.12.2023")
axis(1, datumy, format(datumy, "%Y"), cex.axis = .7, tick = FALSE)

# Výpočet historických výnosov:
X_vynosy <- apply(X = X,       
                  MARGIN = 2,        
                  FUN = function(z){
                    y <- z[2:length(z)]/z[1:(length(z)-1)] - 1
                    return(y)
                  })

# Výpočet priemerného denného a ročného výnosu:
r_denne <- apply(X_vynosy,
                 MARGIN = 2,
                 FUN = mean)
print(r_denne)
r_rocne <- r_denne*240
print(r_rocne)

# Výpočet kovariančnej matice:
V_denna <- cov(X_vynosy)
V_rocna <- V_denna*240
print(V_rocna)

# Výpočet volatility:
sigma_denna <- sqrt(diag(V_denna))
print(sigma_denna)
sigma_rocna <- sqrt(diag(V_rocna))
print(sigma_rocna)

# Výpočet korelácie medzi výnosmi S&P 500 a SHY:
Korelacia <- cor(X_vynosy[,1],X_vynosy[,2])
print(Korelacia)

# Generovanie denných výnosov pre indexový a dlhopisový dôchodkový fond:
set.seed(123)
Index_denne <- matrix(NA, nrow = 10000, ncol = 9600)
Dlhopis_denne <- matrix(NA, nrow = 10000, ncol = 9600)

for (i in 1:10000)
{
  Z_1 = rnorm(9600, mean = 0, sd = 1)
  Z_2 = rnorm(9600, mean = 0, sd = 1)
  Index_denne[i,] <-  r_denne[1] + sigma_denna[1] * Z_1
  Dlhopis_denne[i,] <- r_denne[2] + sigma_denna[2] * (Korelacia * Z_1 + sqrt(1-(Korelacia)^2) * Z_2)
}

# Načítanie dát o plate sporiteľa:
# Treba zmeniť cestu k súboru po jeho stiahnutí:
mzdy <- read_excel("C:\\Users\\lukas\\OneDrive\\Dokumenty\\BP\\Mzdy.xlsx", sheet = "BP", col_names = TRUE)

# Výpočet mesačne povinne platených odvodov do II.piliera:
Odvody_mesiac <- mzdy$Plat*0.04

# Definovanie matice denných odvodov (pre uľahčenie výpočtov):
Odvody_den <- matrix(0, nrow = 10000, ncol = 9600)

for (j in 1:480)
{
  Odvody_den[,j*20] <- Odvody_mesiac[j]   
}

# Výpočet celkového odvedeného množstva prostriedkov odvedených do II.piliera 
Odvody_celkom <- sum(Odvody_mesiac)
print(Odvody_celkom)

# Výpočet aktuálnej hodnoty dôchodkovej jednotky (AHDJ) pre indexový a dlhopisový dôchodkový fond:
Index_AHDJ <- matrix(NA, nrow = 10000, ncol = 9600)
Dlhopis_AHDJ <- matrix(NA, nrow = 10000, ncol = 9600)

for (i in 1:10000)
{
  for (j in 2:9600)
  { 
    Index_AHDJ[i,1] <- 0.033194
    Dlhopis_AHDJ[i,1] <- 0.033194
    Index_AHDJ[i,j] <- Index_AHDJ[i,j-1] * (1+Index_denne[i,j])
    Dlhopis_AHDJ[i,j] <- Dlhopis_AHDJ[i,j-1] * (1+Dlhopis_denne[i,j])
  }  
}

# Výsledná nasporená suma v II.pilieri pre sporivé stratégie založené na aktuálnej hodnote dôchodkovej jednotky (AHDJ):
# Stratégia Discretive ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_Discretive <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Discretive <- matrix(NA, nrow = 10000, ncol = 480)
Nasporene_Discretive[,1:19] <- 0
Zlomok_index <- matrix(NA, nrow = 10000, ncol = 9600)
Zlomok_dlhopis <- matrix(NA, nrow = 10000, ncol = 9600)
Zlomok_index[,1] <- 0
Zlomok_dlhopis[,1] <- 0
Suma_index <- matrix(NA, nrow = 10000, ncol = 480)
Suma_dlhopis <- matrix(NA, nrow = 10000, ncol = 480)
n <- 10
o <- 8

# Výpočet alokačného kritéria:
for (i in 1:10000)
{
  for (j in 2:19)
  {
    Zlomok_index[i,j] <- (Index_AHDJ[i,j] - Index_AHDJ[i,(j-1)])/Index_AHDJ[i,(j-1)]
    Zlomok_dlhopis[i,j] <- (Dlhopis_AHDJ[i,j] - Dlhopis_AHDJ[i,(j-1)])/Dlhopis_AHDJ[i,(j-1)]
  }
}

for (i in 1:10000)
{ 
  N <- 0
  z <- 0
  Min_dlhopis <- 0
  
  for (j in 20:9600)
  { 
    if (j %% 20 == 0)
    {
      z <- z + 1
      N <- N + 20
    }
    
    Zlomok_index[i,j] <- (Index_AHDJ[i,j] - Index_AHDJ[i,(j-1)])/Index_AHDJ[i,(j-1)]
    Suma_index[i,z] <- sum(Zlomok_index[i, ((N-n+1):(N-1))])
    Zlomok_dlhopis[i,j] <- (Dlhopis_AHDJ[i,j] - Dlhopis_AHDJ[i,(j-1)])/Dlhopis_AHDJ[i,(j-1)]
    Suma_dlhopis[i,z] <- sum(Zlomok_dlhopis[i, ((N-o+1):(N-1))])
    
    # Rozhodnutie o alokácii:
    if (Suma_index[i,z] > Suma_dlhopis[i,z])
    { 
      Podiel_index_Discretive[i,z] <- 1
    }
    else 
    { 
      Podiel_index_Discretive[i,z] <- 0
    }
    
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_Discretive[i,j] <- {((max((Podiel_index_Discretive[i,z]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Discretive[i,j-1]) * (1 + Index_denne[i,j])) + 
                                     ((1-max((Podiel_index_Discretive[i,z]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Discretive[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_Discretive[i,j] <- 0.996 * Nasporene_Discretive[i,j]
    }
  }  
}
Finalne_Discretive <- Nasporene_Discretive[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(Zlomok_index)
# rm(Suma_index)
# rm(Zlomok_dlhopis)
# rm(Suma_dlhopis)

# Stratégia Cross-EMA ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Cross_EMA[,19] <- 0
Podiel_index_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 480)
EMA_short <- matrix(NA, nrow = 10000, ncol = 9600)
EMA_long <- matrix(NA, nrow = 10000, ncol = 9600)
EMA_short[,1] <- Index_AHDJ[,1]
EMA_long[,1] <- Index_AHDJ[,1] 
Podiel_index_Cross_EMA[,1] <- 0
p_s <- 4
p_l <- 120

# Výpočet alokačného kritéria:
for (i in 1:10000)
{
  
  for (j in 2:9600)
  {
    EMA_short[i,j] <- (Index_AHDJ[i,j] * (2/(p_s+1))) + (EMA_short[i,(j-1)] * (1 - (2/(p_s+1))))
    EMA_long[i,j] <- (Index_AHDJ[i,j] * (2/(p_l+1))) + (EMA_long[i,(j-1)] * (1 - (2/(p_l+1))))
  }  
}  

for (i in 1:10000)
{
  z <- 0
  Min_dlhopis <- 0
  
  for (j in 20:9600)
  {
    if (j %% 20 == 0)
    {
      z <- z + 1
      
      # Rozhodnutie o alokácii:
      if (EMA_short[i,j-1] > EMA_long[i,j-1])
      { 
        Podiel_index_Cross_EMA[i,z] <- 1
      }
      else 
      { 
        Podiel_index_Cross_EMA[i,z] <- 0
      }
    }
    
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_Cross_EMA[i,j] <- {((max((Podiel_index_Cross_EMA[i,z]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Cross_EMA[i,(j-1)]) * (1 + Index_denne[i,j])) + 
                                    ((1-max((Podiel_index_Cross_EMA[i,z]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Cross_EMA[i,(j-1)]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_Cross_EMA[i,j] <- 0.996 * Nasporene_Cross_EMA[i,j]
    }
  }  
}
Finalne_Cross_EMA <- Nasporene_Cross_EMA[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(EMA_short)
# rm(EMA_long)

# Stratégia MaxMin ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_MaxMin <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_MaxMin[,1:19] <- 0
Podiel_index_MaxMin <- matrix(NA, nrow = 10000, ncol = 480)
Buy <- matrix(NA, nrow = 10000, ncol = 480)
P_I_t <- matrix(NA, nrow = 10000, ncol = 9600)
P_I_t[,1] <- 0
max_P_I_t <- matrix(NA, nrow = 10000, ncol = 9600)
max_P_I_t[,1] <- 0
Stop_Loss <- matrix(NA, nrow = 10000, ncol = 480)
min_EMA_St_cit_t <- matrix(NA, nrow = 10000, ncol = 9600)
min_EMA_St <- matrix(NA, nrow = 10000, ncol = 9600)
cit_t <- matrix(NA, nrow = 10000, ncol = 9600)
min_Pt <- matrix(NA, nrow = 10000, ncol = 9600)
EMA <- matrix(NA, nrow = 10000, ncol = 9600)
N <- 9600
k <- 2/(20+1)

# Výpočet alokačného kritéria:
# Rozhodovací mechanizmus BUY:
for (i in 1:10000)
{
  z <- 0
  
  for (j in 2:20)
  {
    P_I_t[i,j] <- Index_AHDJ[i,(j-1)]
    max_P_I_t[i,j] <- max(P_I_t[i,(1:j)])
    
    if (j %% 20 == 0)
    {
      z <- z+1
      
      if (P_I_t[i,j] >= max_P_I_t[i,j])
      { 
        Buy[i,z] <- 1
      }
      else 
      { 
        Buy[i,z] <- 0
      }
    }  
  }
  
  for (j in 21:9600)
  {
    P_I_t[i,j] <- Index_AHDJ[i,(j-1)]
    max_P_I_t[i,j] <- max(P_I_t[i,((j-20):j)])
    
    if (j %% 20 == 0)
    {
      z <- z+1
      
      if (P_I_t[i,j] >= max_P_I_t[i,j])
      { 
        Buy[i,z] <- 1
      }
      else 
      { 
        Buy[i,z] <- 0
      }
    }    
  }
}

# Rozhodovací mechanizmus Stop-Loss:
for (i in 1:10000)
{
  z <- 0
  
  for (j in 1:120)
  {
    min_Pt[i,j] <- min(P_I_t[i,(1:j)])
    t <- j
    cit_t[i,j] <- (t^(t/N)+t)/(2*N)
    EMA[i,j] <- (P_I_t[i,j]*k+min_Pt[i,j]*(1-k))*cit_t[i,j]
    min_EMA_St[i,j] <- min(EMA[i,(1:j)])
    min_EMA_St_cit_t[i,j] <- min_EMA_St[i,j]*cit_t[i,j]
    
    if (j %% 20 == 0)
    {
      z <- z+1
      
      if (P_I_t[i,j] <= min_EMA_St_cit_t[i,j])
      { 
        Stop_Loss[i,z] <- 1
      }
      else 
      { 
        Stop_Loss[i,z] <- 0
      }
    }
  }
  
  for (j in 121:9600)
  {
    min_Pt[i,j] <- min(P_I_t[i,((j-120):j)])
    t <- j
    cit_t[i,j] <- (t^(t/N)+t)/(2*N)
    EMA[i,j] <- (P_I_t[i,j]*k+min_Pt[i,j]*(1-k))*cit_t[i,j]
    min_EMA_St[i,j] <- min(EMA[i,((j-120):j)])
    min_EMA_St_cit_t[i,j] <- min_EMA_St[i,j]*cit_t[i,j] 
    
    if (j %% 20 == 0)
    {
      z <- z+1
      
      if (P_I_t[i,j] <= min_EMA_St_cit_t[i,j])
      { 
        Stop_Loss[i,z] <- 1
      }
      else
      { 
        Stop_Loss[i,z] <- 0
      }
    }
  }
}

for (i in 1:10000)
{
  z <- 0
  Min_dlhopis <- 0
  
  for (j in 20:9600)
  {
    if (j %% 20 == 0)
    {
      z <- z+1
      
      # Rozhodnutie o alokácii:
      if (Stop_Loss[i,z] == 0 && Buy[i,z] == 1)
      { 
        Podiel_index_MaxMin[i,z] <- 1
      }
      else 
      { 
        Podiel_index_MaxMin[i,z] <- 0
      }
    }
    
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_MaxMin[i,j] <- {((max((Podiel_index_MaxMin[i,z]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_MaxMin[i,(j-1)]) * (1 + Index_denne[i,j])) + 
                                 ((1-max((Podiel_index_MaxMin[i,z]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_MaxMin[i,(j-1)]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_MaxMin[i,j] <- 0.996 * Nasporene_MaxMin[i,j]
    }
  }  
}
Finalne_MaxMin <- Nasporene_MaxMin[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(Buy)
# rm(P_I_t)
# rm(max_P_I_t)
# rm(Stop_Loss)
# rm(min_EMA_St_cit_t)
# rm(min_EMA_St)
# rm(cit_t)
# rm(min_Pt)
# rm(EMA)

# Stratégia RiskTolerance ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_Risk_Tolerance <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Risk_Tolerance[,1:19] <- 0
Podiel_index_Risk_Tolerance <- matrix(NA, nrow = 10000, ncol = 480)
P_S_t <- matrix(NA, nrow = 10000, ncol = 9600)
Buy_t <- matrix(NA, nrow = 10000, ncol = 9600)
DR_t <- matrix(NA, nrow = 10000, ncol = 9600)
D_max_t <- matrix(NA, nrow = 10000, ncol = 9600)
D_max_t[,1] <- 0
D_min_t <- matrix(NA, nrow = 10000, ncol = 9600)
D_min_t[,1] <- 0
N <- 9600
n <- 19
P_S_t[,1] <- 0
DR_t[,1] <- 0
Buy_t[,1] <- 1

# Výpočet alokačného kritéria:
for (i in 1:10000)
{
  for (j in 2:9600)
  {
    P_S_t[i,j] <- Index_AHDJ[i,(j-1)]
    t <- j
    k <- max(1,((2*t)-N))
    D_max_t[i,j] <- max(P_S_t[i,(k:t)])
    D_min_t[i,j] <- min(P_S_t[i,(k:t)])
    
    if (D_max_t[i,j] > D_min_t[i,j])
    { 
      DR_t[i,j] <- (D_max_t[i,j]-P_S_t[i,j])/(D_max_t[i,j]-D_min_t[i,j])
    }
    else 
    { 
      DR_t[i,j] <- 0
    }
    Buy_t[i,j] <- 1-DR_t[i,j]
  }
}

for (i in 1:10000)
{
  z <- 0
  t <- 0
  Min_dlhopis <- 0
  
  # Rozhodnutie o alokácii:
  for (j in 20:9600)
  {
    if (j == 20)
    {
      z <- z+1
      t <- t+20
      Podiel_index_Risk_Tolerance[i,z] <- (sum(Buy_t[i,((t-19):t)])/20)
    }
    
    if (j>20 && j %% 20 == 0)
    {
      z <- z+1
      t <- t+20
      Podiel_index_Risk_Tolerance[i,z] <- (sum(Buy_t[i,((t-n):t)])/(n+1))
    }
    
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_Risk_Tolerance[i,j] <- {((max((Podiel_index_Risk_Tolerance[i,z]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Risk_Tolerance[i,(j-1)]) * (1 + Index_denne[i,j])) + 
                                         ((1-max((Podiel_index_Risk_Tolerance[i,z]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Risk_Tolerance[i,(j-1)]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_Risk_Tolerance[i,j] <- 0.996 * Nasporene_Risk_Tolerance[i,j]
    }
  }  
}
Finalne_Risk_Tolerance <- Nasporene_Risk_Tolerance[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(Podiel_index_Risk_Tolerance)
# rm(P_S_t)
# rm(Buy_t)
# rm(DR_t)
# rm(D_max_t)
# rm(D_min_t)

# Výsledná nasporená suma v II.pilieri pre statické sporivé stratégie: 
# 100:0, 90:10, 75:25, 60:40, 50:50, 40:60, 25:75, 10:90 a 0:100 ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_100_0 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_100_0[,1:19] <- 0
Nasporene_90_10 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_90_10[,1:19] <- 0
Nasporene_75_25 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_75_25[,1:19] <- 0
Nasporene_60_40 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_60_40[,1:19] <- 0
Nasporene_50_50 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_50_50[,1:19] <- 0
Nasporene_40_60 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_40_60[,1:19] <- 0
Nasporene_25_75 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_25_75[,1:19] <- 0
Nasporene_10_90 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_10_90[,1:19] <- 0
Nasporene_0_100 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_0_100[,1:19] <- 0

for (i in 1:10000)
{
  Min_dlhopis <- 0
  Min_dlhopis_pol <- 0
  
  # Výpočet minimálneho podielu v dlhopisovom fonde:
  for (j in 20:9600)
  {
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis_pol <- Min_dlhopis_pol + 0.02
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_100_0[i,j] <- {((max((1-Min_dlhopis_pol),0)*(Odvody_den[i,j] + Nasporene_100_0[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((1-Min_dlhopis_pol),0))*(Odvody_den[i,j] + Nasporene_100_0[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_90_10[i,j] <- {((max((0.9-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_90_10[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.9-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_90_10[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_75_25[i,j] <- {((max((0.75-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_75_25[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.75-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_75_25[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_60_40[i,j] <- {((max((0.6-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_60_40[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.6-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_60_40[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_50_50[i,j] <- {((max((0.5-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_50_50[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.5-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_50_50[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_40_60[i,j] <- {((max((0.4-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_40_60[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.4-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_40_60[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_25_75[i,j] <- {((max((0.25-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_25_75[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.25-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_25_75[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_10_90[i,j] <- {((max((0.1-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_10_90[i,j-1]) * (1 + Index_denne[i,j])) + 
                                ((1-max((0.1-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_10_90[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_0_100[i,j] <- (Odvody_den[i,j] + Nasporene_0_100[i,j-1]) * (1 + Dlhopis_denne[i,j])
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_100_0[i,j] <- 0.996 * Nasporene_100_0[i,j]
      Nasporene_90_10[i,j] <- 0.996 * Nasporene_90_10[i,j]
      Nasporene_75_25[i,j] <- 0.996 * Nasporene_75_25[i,j]
      Nasporene_60_40[i,j] <- 0.996 * Nasporene_60_40[i,j]
      Nasporene_50_50[i,j] <- 0.996 * Nasporene_50_50[i,j]
      Nasporene_40_60[i,j] <- 0.996 * Nasporene_40_60[i,j]
      Nasporene_25_75[i,j] <- 0.996 * Nasporene_25_75[i,j]
      Nasporene_10_90[i,j] <- 0.996 * Nasporene_10_90[i,j]
      Nasporene_0_100[i,j] <- 0.996 * Nasporene_0_100[i,j]
    }  
  }  
}
Finalne_100_0 <- Nasporene_100_0[,9600]
Finalne_90_10 <- Nasporene_90_10[,9600]
Finalne_75_25 <- Nasporene_75_25[,9600]
Finalne_60_40 <- Nasporene_60_40[,9600]
Finalne_50_50 <- Nasporene_50_50[,9600]
Finalne_40_60 <- Nasporene_40_60[,9600]
Finalne_25_75 <- Nasporene_25_75[,9600]
Finalne_10_90 <- Nasporene_10_90[,9600]
Finalne_0_100 <- Nasporene_0_100[,9600]

# Výsledná nasporená suma v II.pilieri pre predvolené investičné stratégie (PIS):
# PIS ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_PIS <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_PIS[,1:19] <- 0
Nasporene_Svedsko <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Svedsko[,1:19] <- 0
Nasporene_Cile <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Cile[,1:19] <- 0
Podiel_index_PIS <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_PIS[,1:6000] <- 1
Podiel_index_Svedsko <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Svedsko[,1:7200] <- 1
Podiel_index_Cile <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Cile[,1:2400] <- 0.6
Podiel_index_Cile[,2400:7200] <- 0.4
Podiel_index_Cile[,7200:9600] <- 0.2

for (i in 1:10000)
{
  for (j in 20:9600)
  {
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Podiel_index_PIS[,j:(j+240)] <- Podiel_index_PIS[,j-1] - 0.04
    }
    if (j >= 7200 && j <= 9360 && (j-7200) %% 240 == 0)
    {
      Podiel_index_Svedsko[,j:(j+240)] <- Podiel_index_Svedsko[,j-1] - 0.035
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_PIS[i,j] <- {((Podiel_index_PIS[i,j]*(Odvody_den[i,j] + Nasporene_PIS[i,j-1]) * (1 + Index_denne[i,j])) + 
                              ((1-Podiel_index_PIS[i,j])*(Odvody_den[i,j] + Nasporene_PIS[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_Svedsko[i,j] <- {((Podiel_index_Svedsko[i,j]*(Odvody_den[i,j] + Nasporene_Svedsko[i,j-1]) * (1 + Index_denne[i,j])) + 
                                  ((1-Podiel_index_Svedsko[i,j])*(Odvody_den[i,j] + Nasporene_Svedsko[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_Cile[i,j] <- {((Podiel_index_Cile[i,j]*(Odvody_den[i,j] + Nasporene_Cile[i,j-1]) * (1 + Index_denne[i,j])) + 
                               ((1-Podiel_index_Cile[i,j])*(Odvody_den[i,j] + Nasporene_Cile[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_PIS[i,j] <- 0.996 * Nasporene_PIS[i,j]
      Nasporene_Svedsko[i,j] <- 0.996 * Nasporene_Svedsko[i,j]
      Nasporene_Cile[i,j] <- 0.996 * Nasporene_Cile[i,j]
    }
  }  
}
Finalne_PIS <- Nasporene_PIS[,9600]
Finalne_Svedsko <- Nasporene_Svedsko[,9600]
Finalne_Cile <- Nasporene_Cile[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(Podiel_index_PIS)
# rm(Podiel_index_Svedsko)
# rm(Podiel_index_Cile)

# Výsledná nasporená suma v II.pilieri pre sporivé stratégie založené na teórii celoživotného cyklu 
# Stratégia Černý-Melicherčík ####
alpha <- matrix(NA, nrow = 10000, ncol = 480)
U_t <- matrix(NA, nrow = 10000, ncol = 480)
C_t_NPV <- matrix(NA, nrow = 480, ncol = 1)
Podiel_index_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 480)
Nasporene_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Cerny_Melichercik[,1:19] <- 0
mu <- r_rocne
r <- 0.02
gamma <- 9

for (i in 1:10000)
{
  z <- 0
  Min_dlhopis <- 0
  
  for (j in 20:9600)
  {
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    # Výpočet alokačného kritéria:
    if (j %% 20 == 0)
    {
      z <- z + 1
      U_t[i,z] <-  Nasporene_Cerny_Melichercik[i,j-1] + Odvody_mesiac[z]
      C_t_NPV[z] <- (sum(Odvody_mesiac[(z:480)])/((1+r)^((480-z)/12)))
      alpha[i,z] <- U_t[i,z]/(U_t[i,z] + C_t_NPV[z])
      
      Optimalne_vahy <- solve.QP(Dmat = gamma * V_rocna,
                                 dvec = mu-r,
                                 Amat = cbind(rep(1,2),diag(2)),
                                 bvec = c(alpha[i,z],rep(0,2)), 
                                 meq = 1)
      
      # Rozhodnutie o alokácii:
      Podiel_index_Cerny_Melichercik[i,z] <- (Optimalne_vahy$solution[1]/alpha[i,z])
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_Cerny_Melichercik[i,j] <- {((max((Podiel_index_Cerny_Melichercik[i,z]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Cerny_Melichercik[i,(j-1)]) * (1 + Index_denne[i,j])) + 
                                            ((1-max((Podiel_index_Cerny_Melichercik[i,z]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Cerny_Melichercik[i,(j-1)]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_Cerny_Melichercik[i,j] <- 0.996 * Nasporene_Cerny_Melichercik[i,j]
    }
  }  
}
Finalne_Cerny_Melichercik <- Nasporene_Cerny_Melichercik[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(alpha)
# rm(U_t)
# rm(C_t_NPV)

# Konečná nasporená suma v II.pilieri pre sporivé stratégie založené na veku sporiteľa:
# Stratégie Aging ####
# Definovanie objektov a nastavenie parametrov statégie:
Nasporene_Aging_1 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Aging_1[,1:19] <- 0
Nasporene_Aging_2 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Aging_2[,1:19] <- 0
Nasporene_Aging_3 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Aging_3[,1:19] <- 0
Nasporene_Aging_4 <- matrix(NA, nrow = 10000, ncol = 9600)
Nasporene_Aging_4[,1:19] <- 0
Podiel_index_Aging_1 <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Aging_1[,1:240] <- 0.75
Podiel_index_Aging_2 <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Aging_2[,1:240] <- 39/40
Podiel_index_Aging_3 <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Aging_3[,1:240] <- 0.25
Podiel_index_Aging_4 <- matrix(NA, nrow = 10000, ncol = 9600)
Podiel_index_Aging_4[,1:240] <- 1/40
y <- matrix(NA, nrow = 10000, ncol = 40)
y[,1] <- 1

for (i in 1:10000)
{
  Min_dlhopis <- 0
  
  for (j in 20:9600)
  {
    # Rozhodnutie o alokácii:
    if (j %% 240 == 0  && j <= 9360)
    { 
      y[,((j/240)+1)] <- y[,(j/240)] + 1 
      Podiel_index_Aging_1[,j:(j+240)] <- Podiel_index_Aging_1[,j-1] - 0.01
      Podiel_index_Aging_2[,j:(j+240)] <- 1 - (y[,((j/240)+1)]/40)
      Podiel_index_Aging_3[,j:(j+240)] <- Podiel_index_Aging_3[,j-1] + 0.01
      Podiel_index_Aging_4[,j:(j+240)] <- y[,((j/240)+1)]/40
    }
    
    # Výpočet minimálneho podielu v dlhopisovom fonde:
    if (j >= 6000 && j <= 9360 && (j-6000) %% 240 == 0)
    {
      Min_dlhopis <- Min_dlhopis + 0.04
    }
    
    # Výpočet nasporenej sumy:
    Nasporene_Aging_1[i,j] <- {((max((Podiel_index_Aging_1[i,j]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Aging_1[i,j-1]) * (1 + Index_denne[i,j])) + 
                                  ((1-max((Podiel_index_Aging_1[i,j]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Aging_1[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_Aging_2[i,j] <- {((max((Podiel_index_Aging_2[i,j]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Aging_2[i,j-1]) * (1 + Index_denne[i,j])) + 
                                  ((1-max((Podiel_index_Aging_2[i,j]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Aging_2[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_Aging_3[i,j] <- {((max((Podiel_index_Aging_3[i,j]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Aging_3[i,j-1]) * (1 + Index_denne[i,j])) + 
                                  ((1-max((Podiel_index_Aging_3[i,j]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Aging_3[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    Nasporene_Aging_4[i,j] <- {((max((Podiel_index_Aging_4[i,j]-Min_dlhopis),0)*(Odvody_den[i,j] + Nasporene_Aging_4[i,j-1]) * (1 + Index_denne[i,j])) + 
                                  ((1-max((Podiel_index_Aging_4[i,j]-Min_dlhopis),0))*(Odvody_den[i,j] + Nasporene_Aging_4[i,j-1]) * (1 + Dlhopis_denne[i,j])))}
    
    # Výpočet poplatkov:
    if (j %% 240 == 0)
    {
      Nasporene_Aging_1[i,j] <- 0.996 * Nasporene_Aging_1[i,j]
      Nasporene_Aging_2[i,j] <- 0.996 * Nasporene_Aging_2[i,j]
      Nasporene_Aging_3[i,j] <- 0.996 * Nasporene_Aging_3[i,j]
      Nasporene_Aging_4[i,j] <- 0.996 * Nasporene_Aging_4[i,j]
    }
  }  
}
Finalne_Aging_1 <- Nasporene_Aging_1[,9600]
Finalne_Aging_2 <- Nasporene_Aging_2[,9600]
Finalne_Aging_3 <- Nasporene_Aging_3[,9600]
Finalne_Aging_4 <- Nasporene_Aging_4[,9600]

# Odstránenie ďalej nepotrebných objektov:
# rm(Podiel_index_Aging_1)
# rm(Podiel_index_Aging_2)
# rm(Podiel_index_Aging_3)
# rm(Podiel_index_Aging_4)
# rm(y)
# rm(Dlhopis_AHDJ)
# rm(Dlhopis_denne)
# rm(GSPC)
# rm(SHY)
# rm(Index_AHDJ)
# rm(Index_denne)
# rm(datumy)
# rm(Optimalne_vahy)
# rm(X)
# rm(X_vynosy)
# rm(V_denna)
# rm(V_rocna)
# rm(ind)

# Nasporená suma ####
# Priemerná výsledná nasporená suma v II.pilieri pre jednotlivé sporivé stratégie:
round(mean(Finalne_100_0),-3)/1000
round(mean(Finalne_90_10),-3)/1000
round(mean(Finalne_75_25),-3)/1000
round(mean(Finalne_60_40),-3)/1000
round(mean(Finalne_50_50),-3)/1000
round(mean(Finalne_40_60),-3)/1000
round(mean(Finalne_25_75),-3)/1000
round(mean(Finalne_10_90),-3)/1000
round(mean(Finalne_0_100),-3)/1000
round(mean(Finalne_PIS),-3)/1000
round(mean(Finalne_Svedsko),-3)/1000
round(mean(Finalne_Cile),-3)/1000
round(mean(Finalne_Aging_1),-3)/1000
round(mean(Finalne_Aging_2),-3)/1000
round(mean(Finalne_Aging_3),-3)/1000
round(mean(Finalne_Aging_4),-3)/1000
round(mean(Finalne_Cerny_Melichercik),-3)/1000
round(mean(Finalne_Discretive),-3)/1000
round(mean(Finalne_Cross_EMA),-3)/1000
round(mean(Finalne_MaxMin),-3)/1000
round(mean(Finalne_Risk_Tolerance),-3)/1000

# Mediánová výsledná nasporená suma v II.pilieri pre jednotlivé sporivé stratégie:
round(median(Finalne_100_0),-3)/1000
round(median(Finalne_90_10),-3)/1000
round(median(Finalne_75_25),-3)/1000
round(median(Finalne_60_40),-3)/1000
round(median(Finalne_50_50),-3)/1000
round(median(Finalne_40_60),-3)/1000
round(median(Finalne_25_75),-3)/1000
round(median(Finalne_10_90),-3)/1000
round(median(Finalne_0_100),-3)/1000
round(median(Finalne_PIS),-3)/1000
round(median(Finalne_Svedsko),-3)/1000
round(median(Finalne_Cile),-3)/1000
round(median(Finalne_Aging_1),-3)/1000
round(median(Finalne_Aging_2),-3)/1000
round(median(Finalne_Aging_3),-3)/1000
round(median(Finalne_Aging_4),-3)/1000
round(median(Finalne_Cerny_Melichercik),-3)/1000
round(median(Finalne_Discretive),-3)/1000
round(median(Finalne_Cross_EMA),-3)/1000
round(median(Finalne_MaxMin),-3)/1000
round(median(Finalne_Risk_Tolerance),-3)/1000

# Maximálna výsledná nasporená suma v II.pilieri pre jednotlivé sporivé stratégie:
round(max(Finalne_100_0),-3)/1000
round(max(Finalne_90_10),-3)/1000
round(max(Finalne_75_25),-3)/1000
round(max(Finalne_60_40),-3)/1000
round(max(Finalne_50_50),-3)/1000
round(max(Finalne_40_60),-3)/1000
round(max(Finalne_25_75),-3)/1000
round(max(Finalne_10_90),-3)/1000
round(max(Finalne_0_100),-3)/1000
round(max(Finalne_PIS),-3)/1000
round(max(Finalne_Svedsko),-3)/1000
round(max(Finalne_Cile),-3)/1000
round(max(Finalne_Aging_1),-3)/1000
round(max(Finalne_Aging_2),-3)/1000
round(max(Finalne_Aging_3),-3)/1000
round(max(Finalne_Aging_4),-3)/1000
round(max(Finalne_Cerny_Melichercik),-3)/1000
round(max(Finalne_Discretive),-3)/1000
round(max(Finalne_Cross_EMA),-3)/1000
round(max(Finalne_MaxMin),-3)/1000
round(max(Finalne_Risk_Tolerance),-3)/1000

# Minimálna výsledná nasporená suma v II.pilieri pre jednotlivé sporivé stratégie:
round(min(Finalne_100_0),-3)/1000
round(min(Finalne_90_10),-3)/1000
round(min(Finalne_75_25),-3)/1000
round(min(Finalne_60_40),-3)/1000
round(min(Finalne_50_50),-3)/1000
round(min(Finalne_40_60),-3)/1000
round(min(Finalne_25_75),-3)/1000
round(min(Finalne_10_90),-3)/1000
round(min(Finalne_0_100),-3)/1000
round(min(Finalne_PIS),-3)/1000
round(min(Finalne_Svedsko),-3)/1000
round(min(Finalne_Cile),-3)/1000
round(min(Finalne_Aging_1),-3)/1000
round(min(Finalne_Aging_2),-3)/1000
round(min(Finalne_Aging_3),-3)/1000
round(min(Finalne_Aging_4),-3)/1000
round(min(Finalne_Cerny_Melichercik),-3)/1000
round(min(Finalne_Discretive),-3)/1000
round(min(Finalne_Cross_EMA),-3)/1000
round(min(Finalne_MaxMin),-3)/1000
round(min(Finalne_Risk_Tolerance),-3)/1000

# 5 % kvantil pre výsledná nasporenú sumu v II.pilieri pre jednotlivé sporivé stratégie:
round(quantile(Finalne_100_0, probs= 0.05),-3)/1000
round(quantile(Finalne_90_10, probs= 0.05),-3)/1000
round(quantile(Finalne_75_25, probs= 0.05),-3)/1000
round(quantile(Finalne_60_40, probs= 0.05),-3)/1000
round(quantile(Finalne_50_50, probs= 0.05),-3)/1000
round(quantile(Finalne_40_60, probs= 0.05),-3)/1000
round(quantile(Finalne_25_75, probs= 0.05),-3)/1000
round(quantile(Finalne_10_90, probs= 0.05),-3)/1000
round(quantile(Finalne_0_100, probs= 0.05),-3)/1000
round(quantile(Finalne_PIS, probs= 0.05),-3)/1000
round(quantile(Finalne_Svedsko, probs= 0.05),-3)/1000
round(quantile(Finalne_Cile, probs= 0.05),-3)/1000
round(quantile(Finalne_Aging_1, probs= 0.05),-3)/1000
round(quantile(Finalne_Aging_2, probs= 0.05),-3)/1000
round(quantile(Finalne_Aging_3, probs= 0.05),-3)/1000
round(quantile(Finalne_Aging_4, probs= 0.05),-3)/1000
round(quantile(Finalne_Cerny_Melichercik, probs= 0.05),-3)/1000
round(quantile(Finalne_Discretive, probs= 0.05),-3)/1000
round(quantile(Finalne_Cross_EMA, probs= 0.05),-3)/1000
round(quantile(Finalne_MaxMin, probs= 0.05),-3)/1000
round(quantile(Finalne_Risk_Tolerance, probs= 0.05),-3)/1000

# 95 % kvantil pre výsledná nasporenú sumu v II.pilieri pre jednotlivé sporivé stratégie:
round(quantile(Finalne_100_0, probs= 0.95),-3)/1000
round(quantile(Finalne_90_10, probs= 0.95),-3)/1000
round(quantile(Finalne_75_25, probs= 0.95),-3)/1000
round(quantile(Finalne_60_40, probs= 0.95),-3)/1000
round(quantile(Finalne_50_50, probs= 0.95),-3)/1000
round(quantile(Finalne_40_60, probs= 0.95),-3)/1000
round(quantile(Finalne_25_75, probs= 0.95),-3)/1000
round(quantile(Finalne_10_90, probs= 0.95),-3)/1000
round(quantile(Finalne_0_100, probs= 0.95),-3)/1000
round(quantile(Finalne_PIS, probs= 0.95),-3)/1000
round(quantile(Finalne_Svedsko, probs= 0.95),-3)/1000
round(quantile(Finalne_Cile, probs= 0.95),-3)/1000
round(quantile(Finalne_Aging_1, probs= 0.95),-3)/1000
round(quantile(Finalne_Aging_2, probs= 0.95),-3)/1000
round(quantile(Finalne_Aging_3, probs= 0.95),-3)/1000
round(quantile(Finalne_Aging_4, probs= 0.95),-3)/1000
round(quantile(Finalne_Cerny_Melichercik, probs= 0.95),-3)/1000
round(quantile(Finalne_Discretive, probs= 0.95),-3)/1000
round(quantile(Finalne_Cross_EMA, probs= 0.95),-3)/1000
round(quantile(Finalne_MaxMin, probs= 0.95),-3)/1000
round(quantile(Finalne_Risk_Tolerance, probs= 0.95),-3)/1000

# Štandardná odchýlka pre konečnú nasporenú sumu v II.pilieri pre jednotlivé sporivé stratégie:
round(sd(Finalne_100_0),-3)/1000
round(sd(Finalne_90_10),-3)/1000
round(sd(Finalne_75_25),-3)/1000
round(sd(Finalne_60_40),-3)/1000
round(sd(Finalne_50_50),-3)/1000
round(sd(Finalne_40_60),-3)/1000
round(sd(Finalne_25_75),-3)/1000
round(sd(Finalne_10_90),-3)/1000
round(sd(Finalne_0_100),-3)/1000
round(sd(Finalne_PIS),-3)/1000
round(sd(Finalne_Svedsko),-3)/1000
round(sd(Finalne_Cile),-3)/1000
round(sd(Finalne_Aging_1),-3)/1000
round(sd(Finalne_Aging_2),-3)/1000
round(sd(Finalne_Aging_3),-3)/1000
round(sd(Finalne_Aging_4),-3)/1000
round(sd(Finalne_Cerny_Melichercik),-3)/1000
round(sd(Finalne_Discretive),-3)/1000
round(sd(Finalne_Cross_EMA),-3)/1000
round(sd(Finalne_MaxMin),-3)/1000
round(sd(Finalne_Risk_Tolerance),-3)/1000

# Vykreslenie závislosti nasporenej sumy od štandardnej odchýlky nasporenej sumy
{Suma <- c(round(mean(Finalne_100_0),-3)/1000, round(mean(Finalne_90_10),-3)/1000, round(mean(Finalne_75_25),-3)/1000, 
           round(mean(Finalne_60_40),-3)/1000, round(mean(Finalne_50_50),-3)/1000, round(mean(Finalne_40_60),-3)/1000, 
           round(mean(Finalne_25_75),-3)/1000, round(mean(Finalne_10_90),-3)/1000, round(mean(Finalne_0_100),-3)/1000, 
           round(mean(Finalne_PIS),-3)/1000, round(mean(Finalne_Svedsko),-3)/1000, round(mean(Finalne_Cile),-3)/1000, 
           round(mean(Finalne_Aging_1),-3)/1000, round(mean(Finalne_Aging_2),-3)/1000, round(mean(Finalne_Aging_3),-3)/1000, 
           round(mean(Finalne_Aging_4),-3)/1000, round(mean(Finalne_Cerny_Melichercik),-3)/1000, round(mean(Finalne_Discretive),-3)/1000, 
           round(mean(Finalne_Cross_EMA),-3)/1000, round(mean(Finalne_MaxMin),-3)/1000, round(mean(Finalne_Risk_Tolerance),-3)/1000)}

{Std <- c(round(sd(Finalne_100_0),-3)/1000, round(sd(Finalne_90_10),-3)/1000, round(sd(Finalne_75_25),-3)/1000, 
          round(sd(Finalne_60_40),-3)/1000, round(sd(Finalne_50_50),-3)/1000, round(sd(Finalne_40_60),-3)/1000, 
          round(sd(Finalne_25_75),-3)/1000, round(sd(Finalne_10_90),-3)/1000, round(sd(Finalne_0_100),-3)/1000,
          round(sd(Finalne_PIS),-3)/1000, round(sd(Finalne_Svedsko),-3)/1000, round(sd(Finalne_Cile),-3)/1000,
          round(sd(Finalne_Aging_1),-3)/1000, round(sd(Finalne_Aging_2),-3)/1000, round(sd(Finalne_Aging_3),-3)/1000, 
          round(sd(Finalne_Aging_4),-3)/1000, round(sd(Finalne_Cerny_Melichercik),-3)/1000, round(sd(Finalne_Discretive),-3)/1000, 
          round(sd(Finalne_Cross_EMA),-3)/1000, round(sd(Finalne_MaxMin),-3)/1000, round(sd(Finalne_Risk_Tolerance),-3)/1000)}

windows()

{plot(Std, Suma, xlim=c(0, 400), ylim=c(0,460), main="Závislosť nasporenej sumy od štandardnej odchýlky nasporenej sumy", xlab="Štandardná odchýlka nasporenej sumy", ylab="Nasporená suma", 
      col=c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold"
            ,"darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, cex=1.2)}
par(xpd=TRUE)
{legend(-16.2,478.9, legend=c("100:0","90:10","75:25","60:40","50:50","40:60","25:75","10:90","0:100","PIS","Švédsko","Čile", 
                              "Aging1","Aging2","Aging3","Aging4","Černý-Melicherčík","Discretive","CrossEMA","MaxMin","RiskTolerance"), 
        col= c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold",
               "darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, ncol=3, cex=1)}

# Dôchodkomesiace ####
# Počet nasporených dôchodkomesiacov pre sporivé stratégie: 
Dochodkomesiace_100_0 <- Finalne_100_0/mean(mzdy$Plat[469:480])
Dochodkomesiace_90_10 <- Finalne_90_10/mean(mzdy$Plat[469:480])
Dochodkomesiace_75_25 <- Finalne_75_25/mean(mzdy$Plat[469:480])
Dochodkomesiace_60_40 <- Finalne_60_40/mean(mzdy$Plat[469:480])
Dochodkomesiace_50_50 <- Finalne_50_50/mean(mzdy$Plat[469:480])
Dochodkomesiace_40_60 <- Finalne_40_60/mean(mzdy$Plat[469:480])
Dochodkomesiace_25_75 <- Finalne_25_75/mean(mzdy$Plat[469:480])
Dochodkomesiace_10_90 <- Finalne_10_90/mean(mzdy$Plat[469:480])
Dochodkomesiace_0_100 <- Finalne_0_100/mean(mzdy$Plat[469:480])
Dochodkomesiace_PIS <- Finalne_PIS/mean(mzdy$Plat[469:480])
Dochodkomesiace_Svedsko <- Finalne_Svedsko/mean(mzdy$Plat[469:480])
Dochodkomesiace_Cile <- Finalne_Cile/mean(mzdy$Plat[469:480])
Dochodkomesiace_Aging_1 <- Finalne_Aging_1/mean(mzdy$Plat[469:480])
Dochodkomesiace_Aging_2 <- Finalne_Aging_2/mean(mzdy$Plat[469:480])
Dochodkomesiace_Aging_3 <- Finalne_Aging_3/mean(mzdy$Plat[469:480])
Dochodkomesiace_Aging_4 <- Finalne_Aging_4/mean(mzdy$Plat[469:480])
Dochodkomesiace_Cerny_Melichercik <- Finalne_Cerny_Melichercik/mean(mzdy$Plat[469:480])
Dochodkomesiace_Discretive <- Finalne_Discretive/mean(mzdy$Plat[469:480])
Dochodkomesiace_Cross_EMA <- Finalne_Cross_EMA/mean(mzdy$Plat[469:480])
Dochodkomesiace_MaxMin <- Finalne_MaxMin/mean(mzdy$Plat[469:480])
Dochodkomesiace_Risk_Tolerance <- Finalne_Risk_Tolerance/mean(mzdy$Plat[469:480])

# Priemerný počet nasporených dôchodkomesiacov pre sporivé stratégie:
round(mean(Dochodkomesiace_100_0),2)
round(mean(Dochodkomesiace_90_10),2)
round(mean(Dochodkomesiace_75_25),2)
round(mean(Dochodkomesiace_60_40),2)
round(mean(Dochodkomesiace_50_50),2)
round(mean(Dochodkomesiace_40_60),2)
round(mean(Dochodkomesiace_25_75),2)
round(mean(Dochodkomesiace_10_90),2)
round(mean(Dochodkomesiace_0_100),2)
round(mean(Dochodkomesiace_PIS),2)
round(mean(Dochodkomesiace_Svedsko),2)
round(mean(Dochodkomesiace_Cile),2)
round(mean(Dochodkomesiace_Aging_1),2)
round(mean(Dochodkomesiace_Aging_2),2)
round(mean(Dochodkomesiace_Aging_3),2)
round(mean(Dochodkomesiace_Aging_4),2)
round(mean(Dochodkomesiace_Cerny_Melichercik),2)
round(mean(Dochodkomesiace_Discretive),2)
round(mean(Dochodkomesiace_Cross_EMA),2)
round(mean(Dochodkomesiace_MaxMin),2)
round(mean(Dochodkomesiace_Risk_Tolerance),2)

# Mediánový počet nasporených dôchodkomesiacov pre sporivé stratégie:
round(median(Dochodkomesiace_100_0),2)
round(median(Dochodkomesiace_90_10),2)
round(median(Dochodkomesiace_75_25),2)
round(median(Dochodkomesiace_60_40),2)
round(median(Dochodkomesiace_50_50),2)
round(median(Dochodkomesiace_40_60),2)
round(median(Dochodkomesiace_25_75),2)
round(median(Dochodkomesiace_10_90),2)
round(median(Dochodkomesiace_0_100),2)
round(median(Dochodkomesiace_PIS),2)
round(median(Dochodkomesiace_Svedsko),2)
round(median(Dochodkomesiace_Cile),2)
round(median(Dochodkomesiace_Aging_1),2)
round(median(Dochodkomesiace_Aging_2),2)
round(median(Dochodkomesiace_Aging_3),2)
round(median(Dochodkomesiace_Aging_4),2)
round(median(Dochodkomesiace_Cerny_Melichercik),2)
round(median(Dochodkomesiace_Discretive),2)
round(median(Dochodkomesiace_Cross_EMA),2)
round(median(Dochodkomesiace_MaxMin),2)
round(median(Dochodkomesiace_Risk_Tolerance),2)

# Maximálny počet nasporených dôchodkomesiacov pre sporivé stratégie:
round(max(Dochodkomesiace_100_0),2)
round(max(Dochodkomesiace_90_10),2)
round(max(Dochodkomesiace_75_25),2)
round(max(Dochodkomesiace_60_40),2)
round(max(Dochodkomesiace_50_50),2)
round(max(Dochodkomesiace_40_60),2)
round(max(Dochodkomesiace_25_75),2)
round(max(Dochodkomesiace_10_90),2)
round(max(Dochodkomesiace_0_100),2)
round(max(Dochodkomesiace_PIS),2)
round(max(Dochodkomesiace_Svedsko),2)
round(max(Dochodkomesiace_Cile),2)
round(max(Dochodkomesiace_Aging_1),2)
round(max(Dochodkomesiace_Aging_2),2)
round(max(Dochodkomesiace_Aging_3),2)
round(max(Dochodkomesiace_Aging_4),2)
round(max(Dochodkomesiace_Cerny_Melichercik),2)
round(max(Dochodkomesiace_Discretive),2)
round(max(Dochodkomesiace_Cross_EMA),2)
round(max(Dochodkomesiace_MaxMin),2)
round(max(Dochodkomesiace_Risk_Tolerance),2)

# Minimálny počet nasporených dôchodkomesiacov pre sporivé stratégie:
round(min(Dochodkomesiace_100_0),2)
round(min(Dochodkomesiace_90_10),2)
round(min(Dochodkomesiace_75_25),2)
round(min(Dochodkomesiace_60_40),2)
round(min(Dochodkomesiace_50_50),2)
round(min(Dochodkomesiace_40_60),2)
round(min(Dochodkomesiace_25_75),2)
round(min(Dochodkomesiace_10_90),2)
round(min(Dochodkomesiace_0_100),2)
round(min(Dochodkomesiace_PIS),2)
round(min(Dochodkomesiace_Svedsko),2)
round(min(Dochodkomesiace_Cile),2)
round(min(Dochodkomesiace_Aging_1),2)
round(min(Dochodkomesiace_Aging_2),2)
round(min(Dochodkomesiace_Aging_3),2)
round(min(Dochodkomesiace_Aging_4),2)
round(min(Dochodkomesiace_Cerny_Melichercik),2)
round(min(Dochodkomesiace_Discretive),2)
round(min(Dochodkomesiace_Cross_EMA),2)
round(min(Dochodkomesiace_MaxMin),2)
round(min(Dochodkomesiace_Risk_Tolerance),2)

# 5 % kvantil pre počet nasporených dôchodkomesiacov pre sporivé stratégie: 
round(quantile(Dochodkomesiace_100_0, probs= 0.05),2)
round(quantile(Dochodkomesiace_90_10, probs= 0.05),2)
round(quantile(Dochodkomesiace_75_25, probs= 0.05),2)
round(quantile(Dochodkomesiace_60_40, probs= 0.05),2)
round(quantile(Dochodkomesiace_50_50, probs= 0.05),2)
round(quantile(Dochodkomesiace_40_60, probs= 0.05),2)
round(quantile(Dochodkomesiace_25_75, probs= 0.05),2)
round(quantile(Dochodkomesiace_10_90, probs= 0.05),2)
round(quantile(Dochodkomesiace_0_100, probs= 0.05),2)
round(quantile(Dochodkomesiace_PIS, probs= 0.05),2)
round(quantile(Dochodkomesiace_Svedsko, probs= 0.05),2)
round(quantile(Dochodkomesiace_Cile, probs= 0.05),2)
round(quantile(Dochodkomesiace_Aging_1, probs= 0.05),2)
round(quantile(Dochodkomesiace_Aging_2, probs= 0.05),2)
round(quantile(Dochodkomesiace_Aging_3, probs= 0.05),2)
round(quantile(Dochodkomesiace_Aging_4, probs= 0.05),2)
round(quantile(Dochodkomesiace_Cerny_Melichercik, probs= 0.05),2)
round(quantile(Dochodkomesiace_Discretive, probs= 0.05),2)
round(quantile(Dochodkomesiace_Cross_EMA, probs= 0.05),2)
round(quantile(Dochodkomesiace_MaxMin, probs= 0.05),2)
round(quantile(Dochodkomesiace_Risk_Tolerance, probs= 0.05),2)

# 95 % kvantil pre počet nasporených dôchodkomesiacov pre sporivé stratégie: 
round(quantile(Dochodkomesiace_100_0, probs= 0.95),2)
round(quantile(Dochodkomesiace_90_10, probs= 0.95),2)
round(quantile(Dochodkomesiace_75_25, probs= 0.95),2)
round(quantile(Dochodkomesiace_60_40, probs= 0.95),2)
round(quantile(Dochodkomesiace_50_50, probs= 0.95),2)
round(quantile(Dochodkomesiace_40_60, probs= 0.95),2)
round(quantile(Dochodkomesiace_25_75, probs= 0.95),2)
round(quantile(Dochodkomesiace_10_90, probs= 0.95),2)
round(quantile(Dochodkomesiace_0_100, probs= 0.95),2)
round(quantile(Dochodkomesiace_PIS, probs= 0.95),2)
round(quantile(Dochodkomesiace_Svedsko, probs= 0.95),2)
round(quantile(Dochodkomesiace_Cile, probs= 0.95),2)
round(quantile(Dochodkomesiace_Aging_1, probs= 0.95),2)
round(quantile(Dochodkomesiace_Aging_2, probs= 0.95),2)
round(quantile(Dochodkomesiace_Aging_3, probs= 0.95),2)
round(quantile(Dochodkomesiace_Aging_4, probs= 0.95),2)
round(quantile(Dochodkomesiace_Cerny_Melichercik, probs= 0.95),2)
round(quantile(Dochodkomesiace_Discretive, probs= 0.95),2)
round(quantile(Dochodkomesiace_Cross_EMA, probs= 0.95),2)
round(quantile(Dochodkomesiace_MaxMin, probs= 0.95),2)
round(quantile(Dochodkomesiace_Risk_Tolerance, probs= 0.95),2)

# Štandardná odchýlka pre počet nasporených dôchodkomesiacov pre sporivé stratégie:
round(sd(Dochodkomesiace_100_0),2)
round(sd(Dochodkomesiace_90_10),2)
round(sd(Dochodkomesiace_75_25),2)
round(sd(Dochodkomesiace_60_40),2)
round(sd(Dochodkomesiace_50_50),2)
round(sd(Dochodkomesiace_40_60),2)
round(sd(Dochodkomesiace_25_75),2)
round(sd(Dochodkomesiace_10_90),2)
round(sd(Dochodkomesiace_0_100),2)
round(sd(Dochodkomesiace_PIS),2)
round(sd(Dochodkomesiace_Svedsko),2)
round(sd(Dochodkomesiace_Cile),2)
round(sd(Dochodkomesiace_Aging_1),2)
round(sd(Dochodkomesiace_Aging_2),2)
round(sd(Dochodkomesiace_Aging_3),2)
round(sd(Dochodkomesiace_Aging_4),2)
round(sd(Dochodkomesiace_Cerny_Melichercik),2)
round(sd(Dochodkomesiace_Discretive),2)
round(sd(Dochodkomesiace_Cross_EMA),2)
round(sd(Dochodkomesiace_MaxMin),2)
round(sd(Dochodkomesiace_Risk_Tolerance),2)

# Vykreslenie závislosti počtu nasporených dôchodkomesiacov od štandardnej odchýlky počtu nasporených dôchodkomesiacov
{Dochodkomesiace <- c(round(mean(Dochodkomesiace_100_0),2), round(mean(Dochodkomesiace_90_10),2), round(mean(Dochodkomesiace_75_25),2), 
                      round(mean(Dochodkomesiace_60_40),2), round(mean(Dochodkomesiace_50_50),2), round(mean(Dochodkomesiace_40_60),2), 
                      round(mean(Dochodkomesiace_25_75),2), round(mean(Dochodkomesiace_10_90),2), round(mean(Dochodkomesiace_0_100),2), 
                      round(mean(Dochodkomesiace_PIS),2), round(mean(Dochodkomesiace_Svedsko),2), round(mean(Dochodkomesiace_Cile),2), 
                      round(mean(Dochodkomesiace_Aging_1),2), round(mean(Dochodkomesiace_Aging_2),2), round(mean(Dochodkomesiace_Aging_3),2), 
                      round(mean(Dochodkomesiace_Aging_4),2), round(mean(Dochodkomesiace_Cerny_Melichercik),2), round(mean(Dochodkomesiace_Discretive),2), 
                      round(mean(Dochodkomesiace_Cross_EMA),2), round(mean(Dochodkomesiace_MaxMin),2), round(mean(Dochodkomesiace_Risk_Tolerance),2))}

{Std <- c(round(sd(Dochodkomesiace_100_0),2), round(sd(Dochodkomesiace_90_10),2), round(sd(Dochodkomesiace_75_25),2), 
          round(sd(Dochodkomesiace_60_40),2), round(sd(Dochodkomesiace_50_50),2), round(sd(Dochodkomesiace_40_60),2), 
          round(sd(Dochodkomesiace_25_75),2), round(sd(Dochodkomesiace_10_90),2), round(sd(Dochodkomesiace_0_100),2),
          round(sd(Dochodkomesiace_PIS),2), round(sd(Dochodkomesiace_Svedsko),2), round(sd(Dochodkomesiace_Cile),2),
          round(sd(Dochodkomesiace_Aging_1),2), round(sd(Dochodkomesiace_Aging_2),2), round(sd(Dochodkomesiace_Aging_3),2), 
          round(sd(Dochodkomesiace_Aging_4),2), round(sd(Dochodkomesiace_Cerny_Melichercik),2), round(sd(Dochodkomesiace_Discretive),2), 
          round(sd(Dochodkomesiace_Cross_EMA),2), round(sd(Dochodkomesiace_MaxMin),2), round(sd(Dochodkomesiace_Risk_Tolerance),2))}

windows()

{plot(Std, Dochodkomesiace, xlim=c(0, 150), ylim=c(0, 200), main="Závislosť počtu nasporených dôchodkomesiacov od štandardnej odchýlky počtu nasporených dôchodkomesiacov", xlab="Štandardná odchýlka počtu nasporených dôchodkomesiacov", ylab="Počet nasporených dôchodkomesiacov", 
      col=c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold"
            ,"darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, cex=1.2)}
par(xpd=TRUE)
{legend(-6,208, legend=c("100:0","90:10","75:25","60:40","50:50","40:60","25:75","10:90","0:100","PIS","Švédsko","Čile", 
                         "Aging1","Aging2","Aging3","Aging4","Černý-Melicherčík","Discretive","CrossEMA","MaxMin","RiskTolerance"), 
        col= c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold",
               "darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, ncol=3, cex=1)}

# Odstránenie ďalej nepotrebných objektov:
# rm(Dochodkomesiace_100_0)
# rm(Dochodkomesiace_90_10)
# rm(Dochodkomesiace_75_25)
# rm(Dochodkomesiace_60_40)
# rm(Dochodkomesiace_50_50)
# rm(Dochodkomesiace_40_60)
# rm(Dochodkomesiace_25_75)
# rm(Dochodkomesiace_10_90)
# rm(Dochodkomesiace_0_100)
# rm(Dochodkomesiace_PIS)
# rm(Dochodkomesiace_Svedsko)
# rm(Dochodkomesiace_Cile)
# rm(Dochodkomesiace_Aging_1)
# rm(Dochodkomesiace_Aging_2)
# rm(Dochodkomesiace_Aging_3)
# rm(Dochodkomesiace_Aging_4)
# rm(Dochodkomesiace_Cerny_Melichercik)
# rm(Dochodkomesiace_Discretive)
# rm(Dochodkomesiace_Cross_EMA)
# rm(Dochodkomesiace_MaxMin)
# rm(Dochodkomesiace_Risk_Tolerance)

# Výkonnosť ####
# Výkonnosť sporivých stratégií:
Vykonnost_100_0 <- (Finalne_100_0/Odvody_celkom)-1
Vykonnost_90_10 <- (Finalne_90_10/Odvody_celkom)-1
Vykonnost_75_25 <- (Finalne_75_25/Odvody_celkom)-1
Vykonnost_60_40 <- (Finalne_60_40/Odvody_celkom)-1
Vykonnost_50_50 <- (Finalne_50_50/Odvody_celkom)-1
Vykonnost_40_60 <- (Finalne_40_60/Odvody_celkom)-1
Vykonnost_25_75 <- (Finalne_25_75/Odvody_celkom)-1
Vykonnost_10_90 <- (Finalne_10_90/Odvody_celkom)-1
Vykonnost_0_100 <- (Finalne_0_100/Odvody_celkom)-1
Vykonnost_PIS <- (Finalne_PIS/Odvody_celkom)-1
Vykonnost_Svedsko <- (Finalne_Svedsko/Odvody_celkom)-1
Vykonnost_Cile <- (Finalne_Cile/Odvody_celkom)-1
Vykonnost_Aging_1 <- (Finalne_Aging_1/Odvody_celkom)-1
Vykonnost_Aging_2 <- (Finalne_Aging_2/Odvody_celkom)-1
Vykonnost_Aging_3 <- (Finalne_Aging_3/Odvody_celkom)-1
Vykonnost_Aging_4 <- (Finalne_Aging_4/Odvody_celkom)-1
Vykonnost_Cerny_Melichercik <- (Finalne_Cerny_Melichercik/Odvody_celkom)-1
Vykonnost_Discretive <- (Finalne_Discretive/Odvody_celkom)-1
Vykonnost_Cross_EMA <- (Finalne_Cross_EMA/Odvody_celkom)-1
Vykonnost_MaxMin <- (Finalne_MaxMin/Odvody_celkom)-1
Vykonnost_Risk_Tolerance <- (Finalne_Risk_Tolerance/Odvody_celkom)-1

# Priemerná výkonnosť sporivých stratégií:
round(mean(Vykonnost_100_0),2)
round(mean(Vykonnost_90_10),2)
round(mean(Vykonnost_75_25),2)
round(mean(Vykonnost_60_40),2)
round(mean(Vykonnost_50_50),2)
round(mean(Vykonnost_40_60),2)
round(mean(Vykonnost_25_75),2)
round(mean(Vykonnost_10_90),2)
round(mean(Vykonnost_0_100),2)
round(mean(Vykonnost_PIS),2)
round(mean(Vykonnost_Svedsko),2)
round(mean(Vykonnost_Cile),2)
round(mean(Vykonnost_Aging_1),2)
round(mean(Vykonnost_Aging_2),2)
round(mean(Vykonnost_Aging_3),2)
round(mean(Vykonnost_Aging_4),2)
round(mean(Vykonnost_Cerny_Melichercik),2)
round(mean(Vykonnost_Discretive),2)
round(mean(Vykonnost_Cross_EMA),2)
round(mean(Vykonnost_MaxMin),2)
round(mean(Vykonnost_Risk_Tolerance),2)

# Mediánová výkonnosť sporivých stratégií:
round(median(Vykonnost_100_0),2)
round(median(Vykonnost_90_10),2)
round(median(Vykonnost_75_25),2)
round(median(Vykonnost_60_40),2)
round(median(Vykonnost_50_50),2)
round(median(Vykonnost_40_60),2)
round(median(Vykonnost_25_75),2)
round(median(Vykonnost_10_90),2)
round(median(Vykonnost_0_100),2)
round(median(Vykonnost_PIS),2)
round(median(Vykonnost_Svedsko),2)
round(median(Vykonnost_Cile),2)
round(median(Vykonnost_Aging_1),2)
round(median(Vykonnost_Aging_2),2)
round(median(Vykonnost_Aging_3),2)
round(median(Vykonnost_Aging_4),2)
round(median(Vykonnost_Cerny_Melichercik),2)
round(median(Vykonnost_Discretive),2)
round(median(Vykonnost_Cross_EMA),2)
round(median(Vykonnost_MaxMin),2)
round(median(Vykonnost_Risk_Tolerance),2)

# Maximálna výkonnosť sporivých stratégií:
round(max(Vykonnost_100_0),2)
round(max(Vykonnost_90_10),2)
round(max(Vykonnost_75_25),2)
round(max(Vykonnost_60_40),2)
round(max(Vykonnost_50_50),2)
round(max(Vykonnost_40_60),2)
round(max(Vykonnost_25_75),2)
round(max(Vykonnost_10_90),2)
round(max(Vykonnost_0_100),2)
round(max(Vykonnost_PIS),2)
round(max(Vykonnost_Svedsko),2)
round(max(Vykonnost_Cile),2)
round(max(Vykonnost_Aging_1),2)
round(max(Vykonnost_Aging_2),2)
round(max(Vykonnost_Aging_3),2)
round(max(Vykonnost_Aging_4),2)
round(max(Vykonnost_Cerny_Melichercik),2)
round(max(Vykonnost_Discretive),2)
round(max(Vykonnost_Cross_EMA),2)
round(max(Vykonnost_MaxMin),2)
round(max(Vykonnost_Risk_Tolerance),2)

# Minimálna výkonnosť sporivých stratégií:
round(min(Vykonnost_100_0),2)
round(min(Vykonnost_90_10),2)
round(min(Vykonnost_75_25),2)
round(min(Vykonnost_60_40),2)
round(min(Vykonnost_50_50),2)
round(min(Vykonnost_40_60),2)
round(min(Vykonnost_25_75),2)
round(min(Vykonnost_10_90),2)
round(min(Vykonnost_0_100),2)
round(min(Vykonnost_PIS),2)
round(min(Vykonnost_Svedsko),2)
round(min(Vykonnost_Cile),2)
round(min(Vykonnost_Aging_1),2)
round(min(Vykonnost_Aging_2),2)
round(min(Vykonnost_Aging_3),2)
round(min(Vykonnost_Aging_4),2)
round(min(Vykonnost_Cerny_Melichercik),2)
round(min(Vykonnost_Discretive),2)
round(min(Vykonnost_Cross_EMA),2)
round(min(Vykonnost_MaxMin),2)
round(min(Vykonnost_Risk_Tolerance),2)

# 5 % kvantil pre výkonnosť sporivých stratégií: 
round(quantile(Vykonnost_100_0, probs= 0.05),2)
round(quantile(Vykonnost_90_10, probs= 0.05),2)
round(quantile(Vykonnost_75_25, probs= 0.05),2)
round(quantile(Vykonnost_60_40, probs= 0.05),2)
round(quantile(Vykonnost_50_50, probs= 0.05),2)
round(quantile(Vykonnost_40_60, probs= 0.05),2)
round(quantile(Vykonnost_25_75, probs= 0.05),2)
round(quantile(Vykonnost_10_90, probs= 0.05),2)
round(quantile(Vykonnost_0_100, probs= 0.05),2)
round(quantile(Vykonnost_PIS, probs= 0.05),2)
round(quantile(Vykonnost_Svedsko, probs= 0.05),2)
round(quantile(Vykonnost_Cile, probs= 0.05),2)
round(quantile(Vykonnost_Aging_1, probs= 0.05),2)
round(quantile(Vykonnost_Aging_2, probs= 0.05),2)
round(quantile(Vykonnost_Aging_3, probs= 0.05),2)
round(quantile(Vykonnost_Aging_4, probs= 0.05),2)
round(quantile(Vykonnost_Cerny_Melichercik, probs= 0.05),2)
round(quantile(Vykonnost_Discretive, probs= 0.05),2)
round(quantile(Vykonnost_Cross_EMA, probs= 0.05),2)
round(quantile(Vykonnost_MaxMin, probs= 0.05),2)
round(quantile(Vykonnost_Risk_Tolerance, probs= 0.05),2)

# 95 % kvantil pre výkonnosť sporivých stratégií: 
round(quantile(Vykonnost_100_0, probs= 0.95),2)
round(quantile(Vykonnost_90_10, probs= 0.95),2)
round(quantile(Vykonnost_75_25, probs= 0.95),2)
round(quantile(Vykonnost_60_40, probs= 0.95),2)
round(quantile(Vykonnost_50_50, probs= 0.95),2)
round(quantile(Vykonnost_40_60, probs= 0.95),2)
round(quantile(Vykonnost_25_75, probs= 0.95),2)
round(quantile(Vykonnost_10_90, probs= 0.95),2)
round(quantile(Vykonnost_0_100, probs= 0.95),2)
round(quantile(Vykonnost_PIS, probs= 0.95),2)
round(quantile(Vykonnost_Svedsko, probs= 0.95),2)
round(quantile(Vykonnost_Cile, probs= 0.95),2)
round(quantile(Vykonnost_Aging_1, probs= 0.95),2)
round(quantile(Vykonnost_Aging_2, probs= 0.95),2)
round(quantile(Vykonnost_Aging_3, probs= 0.95),2)
round(quantile(Vykonnost_Aging_4, probs= 0.95),2)
round(quantile(Vykonnost_Cerny_Melichercik, probs= 0.95),2)
round(quantile(Vykonnost_Discretive, probs= 0.95),2)
round(quantile(Vykonnost_Cross_EMA, probs= 0.95),2)
round(quantile(Vykonnost_MaxMin, probs= 0.95),2)
round(quantile(Vykonnost_Risk_Tolerance, probs= 0.95),2)

# Štandardná odchýlka pre výkonnosť sporivých stratégií:
round(sd(Vykonnost_100_0),2)
round(sd(Vykonnost_90_10),2)
round(sd(Vykonnost_75_25),2)
round(sd(Vykonnost_60_40),2)
round(sd(Vykonnost_50_50),2)
round(sd(Vykonnost_40_60),2)
round(sd(Vykonnost_25_75),2)
round(sd(Vykonnost_10_90),2)
round(sd(Vykonnost_0_100),2)
round(sd(Vykonnost_PIS),2)
round(sd(Vykonnost_Svedsko),2)
round(sd(Vykonnost_Cile),2)
round(sd(Vykonnost_Aging_1),2)
round(sd(Vykonnost_Aging_2),2)
round(sd(Vykonnost_Aging_3),2)
round(sd(Vykonnost_Aging_4),2)
round(sd(Vykonnost_Cerny_Melichercik),2)
round(sd(Vykonnost_Discretive),2)
round(sd(Vykonnost_Cross_EMA),2)
round(sd(Vykonnost_MaxMin),2)
round(sd(Vykonnost_Risk_Tolerance),2)

# Vykreslenie závislosti výkonnosti od štandardnej odchýlky výkonnosti
{Vykonnost <- c(round(mean(Vykonnost_100_0),2), round(mean(Vykonnost_90_10),2), round(mean(Vykonnost_75_25),2), 
                round(mean(Vykonnost_60_40),2), round(mean(Vykonnost_50_50),2), round(mean(Vykonnost_40_60),2), 
                round(mean(Vykonnost_25_75),2), round(mean(Vykonnost_10_90),2), round(mean(Vykonnost_0_100),2), 
                round(mean(Vykonnost_PIS),2), round(mean(Vykonnost_Svedsko),2), round(mean(Vykonnost_Cile),2), 
                round(mean(Vykonnost_Aging_1),2), round(mean(Vykonnost_Aging_2),2), round(mean(Vykonnost_Aging_3),2), 
                round(mean(Vykonnost_Aging_4),2), round(mean(Vykonnost_Cerny_Melichercik),2), round(mean(Vykonnost_Discretive),2), 
                round(mean(Vykonnost_Cross_EMA),2), round(mean(Vykonnost_MaxMin),2), round(mean(Vykonnost_Risk_Tolerance),2))}

{Std <- c(round(sd(Vykonnost_100_0),2), round(sd(Vykonnost_90_10),2), round(sd(Vykonnost_75_25),2), 
          round(sd(Vykonnost_60_40),2), round(sd(Vykonnost_50_50),2), round(sd(Vykonnost_40_60),2), 
          round(sd(Vykonnost_25_75),2), round(sd(Vykonnost_10_90),2), round(sd(Vykonnost_0_100),2),
          round(sd(Vykonnost_PIS),2), round(sd(Vykonnost_Svedsko),2), round(sd(Vykonnost_Cile),2),
          round(sd(Vykonnost_Aging_1),2), round(sd(Vykonnost_Aging_2),2), round(sd(Vykonnost_Aging_3),2), 
          round(sd(Vykonnost_Aging_4),2), round(sd(Vykonnost_Cerny_Melichercik),2), round(sd(Vykonnost_Discretive),2), 
          round(sd(Vykonnost_Cross_EMA),2), round(sd(Vykonnost_MaxMin),2), round(sd(Vykonnost_Risk_Tolerance),2))}

windows()

{plot(Std, Vykonnost, xlim=c(0, 8), ylim=c(0,10), main="Závislosť výkonnosti od štandardnej odchýlky výkonnosti", xlab="Štandardná odchýlka výkonnosti", ylab="Výkonnosť", 
      col=c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold"
            ,"darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, cex=1.2)}
par(xpd=TRUE)
{legend(-0.32,10.4, legend=c("100:0","90:10","75:25","60:40","50:50","40:60","25:75","10:90","0:100","PIS","Švédsko","Čile", 
                             "Aging1","Aging2","Aging3","Aging4","Černý-Melicherčík","Discretive","CrossEMA","MaxMin","RiskTolerance"), 
        col= c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold",
               "darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, ncol=3, cex=1)}

# Odstránenie ďalej nepotrebných objektov:
# rm(Vykonnost_100_0)
# rm(Vykonnost_90_10)
# rm(Vykonnost_75_25)
# rm(Vykonnost_60_40)
# rm(Vykonnost_50_50)
# rm(Vykonnost_40_60)
# rm(Vykonnost_25_75)
# rm(Vykonnost_10_90)
# rm(Vykonnost_0_100)
# rm(Vykonnost_PIS)
# rm(Vykonnost_Svedsko)
# rm(Vykonnost_Cile)
# rm(Vykonnost_Aging_1)
# rm(Vykonnost_Aging_2)
# rm(Vykonnost_Aging_3)
# rm(Vykonnost_Aging_4)
# rm(Vykonnost_Cerny_Melichercik)
# rm(Vykonnost_Discretive)
# rm(Vykonnost_Cross_EMA)
# rm(Vykonnost_MaxMin)
# rm(Vykonnost_Risk_Tolerance)

# Pravdepodobnosť nenavrátenia a strata ####
# Pravdepodobnosť, že výsledná nasporená suma bude nižšia ako celková suma odvedená do II.piliera:
pocet_iteracii <- 10000
P_nenavratenia_100_0 <- (sum(Odvody_celkom > Finalne_100_0)/pocet_iteracii)*100; print(P_nenavratenia_100_0) 
P_nenavratenia_90_10 <- (sum(Odvody_celkom > Finalne_90_10)/pocet_iteracii)*100; print(P_nenavratenia_90_10)
P_nenavratenia_75_25 <- (sum(Odvody_celkom > Finalne_75_25)/pocet_iteracii)*100; print(P_nenavratenia_75_25)
P_nenavratenia_60_40 <- (sum(Odvody_celkom > Finalne_60_40)/pocet_iteracii)*100; print(P_nenavratenia_60_40)
P_nenavratenia_50_50 <- (sum(Odvody_celkom > Finalne_50_50)/pocet_iteracii)*100; print(P_nenavratenia_50_50)
P_nenavratenia_40_60 <- (sum(Odvody_celkom > Finalne_40_60)/pocet_iteracii)*100; print(P_nenavratenia_40_60)
P_nenavratenia_25_75 <- (sum(Odvody_celkom > Finalne_25_75)/pocet_iteracii)*100; print(P_nenavratenia_25_75)
P_nenavratenia_10_90 <- (sum(Odvody_celkom > Finalne_10_90)/pocet_iteracii)*100; print(P_nenavratenia_10_90)
P_nenavratenia_0_100 <- (sum(Odvody_celkom > Finalne_0_100)/pocet_iteracii)*100; print(P_nenavratenia_0_100)
P_nenavratenia_PIS <- (sum(Odvody_celkom > Finalne_PIS)/pocet_iteracii)*100; print(P_nenavratenia_PIS)
P_nenavratenia_Svedsko <- (sum(Odvody_celkom > Finalne_Svedsko)/pocet_iteracii)*100; print(P_nenavratenia_Svedsko)
P_nenavratenia_Cile <- (sum(Odvody_celkom > Finalne_Cile)/pocet_iteracii)*100; print(P_nenavratenia_Cile)
P_nenavratenia_Aging_1 <- (sum(Odvody_celkom > Finalne_Aging_1)/pocet_iteracii)*100; print(P_nenavratenia_Aging_1)
P_nenavratenia_Aging_2 <- (sum(Odvody_celkom > Finalne_Aging_2)/pocet_iteracii)*100; print(P_nenavratenia_Aging_2)
P_nenavratenia_Aging_3 <- (sum(Odvody_celkom > Finalne_Aging_3)/pocet_iteracii)*100; print(P_nenavratenia_Aging_3)
P_nenavratenia_Aging_4 <- (sum(Odvody_celkom > Finalne_Aging_4)/pocet_iteracii)*100; print(P_nenavratenia_Aging_4)
P_nenavratenia_Cerny_Melichercik <- (sum(Odvody_celkom > Finalne_Cerny_Melichercik)/pocet_iteracii)*100; print(P_nenavratenia_Cerny_Melichercik)
P_nenavratenia_Discretive <- (sum(Odvody_celkom > Finalne_Discretive)/pocet_iteracii)*100; print(P_nenavratenia_Discretive)
P_nenavratenia_Cross_EMA <- (sum(Odvody_celkom > Finalne_Cross_EMA)/pocet_iteracii)*100; print(P_nenavratenia_Cross_EMA)
P_nenavratenia_MaxMin <- (sum(Odvody_celkom > Finalne_MaxMin)/pocet_iteracii)*100; print(P_nenavratenia_MaxMin)
P_nenavratenia_Risk_Tolerance <- (sum(Odvody_celkom > Finalne_Risk_Tolerance)/pocet_iteracii)*100; print(P_nenavratenia_Risk_Tolerance)

# Strata, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
Strata_100_0 <- Odvody_celkom - Finalne_100_0[Odvody_celkom > Finalne_100_0]
Strata_90_10 <- Odvody_celkom - Finalne_90_10[Odvody_celkom > Finalne_90_10]
Strata_75_25 <- Odvody_celkom - Finalne_75_25[Odvody_celkom > Finalne_75_25]
Strata_60_40 <- Odvody_celkom - Finalne_60_40[Odvody_celkom > Finalne_60_40]
Strata_50_50 <- Odvody_celkom - Finalne_50_50[Odvody_celkom > Finalne_50_50]
Strata_40_60 <- Odvody_celkom - Finalne_40_60[Odvody_celkom > Finalne_40_60]
Strata_10_90 <- Odvody_celkom - Finalne_10_90[Odvody_celkom > Finalne_10_90]
Strata_0_100 <- Odvody_celkom - Finalne_0_100[Odvody_celkom > Finalne_0_100]
Strata_PIS <- Odvody_celkom - Finalne_PIS[Odvody_celkom > Finalne_PIS]
Strata_Svedsko <- Odvody_celkom - Finalne_Svedsko[Odvody_celkom > Finalne_Svedsko]
Strata_Cile <- Odvody_celkom - Finalne_Cile[Odvody_celkom > Finalne_Cile]
Strata_Aging_1 <- Odvody_celkom - Finalne_Aging_1[Odvody_celkom > Finalne_Aging_1]
Strata_Aging_2 <- Odvody_celkom - Finalne_Aging_2[Odvody_celkom > Finalne_Aging_2]
Strata_Aging_3 <- Odvody_celkom - Finalne_Aging_3[Odvody_celkom > Finalne_Aging_3]
Strata_Aging_4 <- Odvody_celkom - Finalne_Aging_4[Odvody_celkom > Finalne_Aging_4]
Strata_Cerny_Melichercik <- Odvody_celkom - Finalne_Cerny_Melichercik[Odvody_celkom > Finalne_Cerny_Melichercik]
Strata_Discretive <- Odvody_celkom - Finalne_Discretive[Odvody_celkom > Finalne_Discretive]
Strata_Cross_EMA <- Odvody_celkom - Finalne_Cross_EMA[Odvody_celkom > Finalne_Cross_EMA]
Strata_MaxMin <- Odvody_celkom - Finalne_MaxMin[Odvody_celkom > Finalne_MaxMin]
Strata_Risk_Tolerance <- Odvody_celkom - Finalne_Risk_Tolerance[Odvody_celkom > Finalne_Risk_Tolerance]

# Priemerná strata, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(mean(Strata_100_0),-2)/100
round(mean(Strata_90_10),-2)/100
round(mean(Strata_75_25),-2)/100
round(mean(Strata_60_40),-2)/100
round(mean(Strata_50_50),-2)/100
round(mean(Strata_40_60),-2)/100
round(mean(Strata_PIS),-2)/100
round(mean(Strata_Svedsko),-2)/100
round(mean(Strata_Cile),-2)/100
round(mean(Strata_Aging_1),-2)/100
round(mean(Strata_Aging_2),-2)/100
round(mean(Strata_Aging_3),-2)/100
round(mean(Strata_Aging_4),-2)/100
round(mean(Strata_Cerny_Melichercik),-2)/100
round(mean(Strata_Discretive),-2)/100
round(mean(Strata_Cross_EMA),-2)/100
round(mean(Strata_MaxMin),-2)/100
round(mean(Strata_Risk_Tolerance),-2)/100

# Mediánová strata, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(median(Strata_100_0),-2)/100
round(median(Strata_90_10),-2)/100
round(median(Strata_75_25),-2)/100
round(median(Strata_60_40),-2)/100
round(median(Strata_50_50),-2)/100
round(median(Strata_40_60),-2)/100
round(median(Strata_PIS),-2)/100
round(median(Strata_Svedsko),-2)/100
round(median(Strata_Cile),-2)/100
round(median(Strata_Aging_1),-2)/100
round(median(Strata_Aging_2),-2)/100
round(median(Strata_Aging_3),-2)/100
round(median(Strata_Aging_4),-2)/100
round(median(Strata_Cerny_Melichercik),-2)/100
round(median(Strata_Discretive),-2)/100
round(median(Strata_Cross_EMA),-2)/100
round(median(Strata_MaxMin),-2)/100
round(median(Strata_Risk_Tolerance),-2)/100

# Maximálna strata, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(max(Strata_100_0),-2)/100
round(max(Strata_90_10),-2)/100
round(max(Strata_75_25),-2)/100
round(max(Strata_60_40),-2)/100
round(max(Strata_50_50),-2)/100
round(max(Strata_40_60),-2)/100
round(max(Strata_PIS),-2)/100
round(max(Strata_Svedsko),-2)/100
round(max(Strata_Cile),-2)/100
round(max(Strata_Aging_1),-2)/100
round(max(Strata_Aging_2),-2)/100
round(max(Strata_Aging_3),-2)/100
round(max(Strata_Aging_4),-2)/100
round(max(Strata_Cerny_Melichercik),-2)/100
round(max(Strata_Discretive),-2)/100
round(max(Strata_Cross_EMA),-2)/100
round(max(Strata_MaxMin),-2)/100
round(max(Strata_Risk_Tolerance),-2)/100

# Minimálna strata, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(min(Strata_100_0),-2)/100
round(min(Strata_90_10),-2)/100
round(min(Strata_75_25),-2)/100
round(min(Strata_60_40),-2)/100
round(min(Strata_50_50),-2)/100
round(min(Strata_40_60),-2)/100
round(min(Strata_PIS),-2)/100
round(min(Strata_Svedsko),-2)/100
round(min(Strata_Cile),-2)/100
round(min(Strata_Aging_1),-2)/100
round(min(Strata_Aging_2),-2)/100
round(min(Strata_Aging_3),-2)/100
round(min(Strata_Aging_4),-2)/100
round(min(Strata_Cerny_Melichercik),-2)/100
round(min(Strata_Discretive),-2)/100
round(min(Strata_Cross_EMA),-2)/100
round(min(Strata_MaxMin),-2)/100
round(min(Strata_Risk_Tolerance),-2)/100

# 5 % kvantil pre stratu, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(quantile(Strata_100_0, probs= 0.05),-2)/100
round(quantile(Strata_90_10, probs= 0.05),-2)/100
round(quantile(Strata_75_25, probs= 0.05),-2)/100
round(quantile(Strata_60_40, probs= 0.05),-2)/100
round(quantile(Strata_50_50, probs= 0.05),-2)/100
round(quantile(Strata_40_60, probs= 0.05),-2)/100
round(quantile(Strata_PIS, probs= 0.05),-2)/100
round(quantile(Strata_Svedsko, probs= 0.05),-2)/100
round(quantile(Strata_Cile, probs= 0.05),-2)/100
round(quantile(Strata_Aging_1, probs= 0.05),-2)/100
round(quantile(Strata_Aging_2, probs= 0.05),-2)/100
round(quantile(Strata_Aging_3, probs= 0.05),-2)/100
round(quantile(Strata_Aging_4, probs= 0.05),-2)/100
round(quantile(Strata_Cerny_Melichercik, probs= 0.05),-2)/100
round(quantile(Strata_Discretive, probs= 0.05),-2)/100
round(quantile(Strata_Cross_EMA, probs= 0.05),-2)/100
round(quantile(Strata_MaxMin, probs= 0.05),-2)/100
round(quantile(Strata_Risk_Tolerance, probs= 0.05),-2)/100

# 95 % kvantil pre stratu, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(quantile(Strata_100_0, probs= 0.95),-2)/100
round(quantile(Strata_90_10, probs= 0.95),-2)/100
round(quantile(Strata_75_25, probs= 0.95),-2)/100
round(quantile(Strata_60_40, probs= 0.95),-2)/100
round(quantile(Strata_50_50, probs= 0.95),-2)/100
round(quantile(Strata_40_60, probs= 0.95),-2)/100
round(quantile(Strata_PIS, probs= 0.95),-2)/100
round(quantile(Strata_Svedsko, probs= 0.95),-2)/100
round(quantile(Strata_Cile, probs= 0.95),-2)/100
round(quantile(Strata_Aging_1, probs= 0.95),-2)/100
round(quantile(Strata_Aging_2, probs= 0.95),-2)/100
round(quantile(Strata_Aging_3, probs= 0.95),-2)/100
round(quantile(Strata_Aging_4, probs= 0.95),-2)/100
round(quantile(Strata_Cerny_Melichercik, probs= 0.95),-2)/100
round(quantile(Strata_Discretive, probs= 0.95),-2)/100
round(quantile(Strata_Cross_EMA, probs= 0.95),-2)/100
round(quantile(Strata_MaxMin, probs= 0.95),-2)/100
round(quantile(Strata_Risk_Tolerance, probs= 0.95),-2)/100

# Štandardná odchýlka pre stratu, keď výsledná nasporená suma je nižšia ako celková suma odvedená do II.piliera pre jednotlivé sporivé stratégie:
round(sd(Strata_100_0),-2)/100
round(sd(Strata_90_10),-2)/100
round(sd(Strata_75_25),-2)/100
round(sd(Strata_60_40),-2)/100
round(sd(Strata_50_50),-2)/100
round(sd(Strata_40_60),-2)/100
round(sd(Strata_10_90),-2)/100
round(sd(Strata_0_100),-2)/100
round(sd(Strata_PIS),-2)/100
round(sd(Strata_Svedsko),-2)/100
round(sd(Strata_Cile),-2)/100
round(sd(Strata_Aging_1),-2)/100
round(sd(Strata_Aging_2),-2)/100
round(sd(Strata_Aging_3),-2)/100
round(sd(Strata_Aging_4),-2)/100
round(sd(Strata_Cerny_Melichercik),-2)/100
round(sd(Strata_Discretive),-2)/100
round(sd(Strata_Cross_EMA),-2)/100
round(sd(Strata_MaxMin),-2)/100
round(sd(Strata_Risk_Tolerance),-2)/100

# Odstránenie ďalej nepotrebných objektov:
# rm(P_nenavratenia_100_0)
# rm(P_nenavratenia_90_10)
# rm(P_nenavratenia_75_25)
# rm(P_nenavratenia_60_40)
# rm(P_nenavratenia_50_50)
# rm(P_nenavratenia_40_60)
# rm(P_nenavratenia_25_75)
# rm(P_nenavratenia_10_90)
# rm(P_nenavratenia_0_100)
# rm(P_nenavratenia_PIS)
# rm(P_nenavratenia_Svedsko)
# rm(P_nenavratenia_Cile)
# rm(P_nenavratenia_Aging_1)
# rm(P_nenavratenia_Aging_2)
# rm(P_nenavratenia_Aging_3)
# rm(P_nenavratenia_Aging_4)
# rm(P_nenavratenia_Cerny_Melichercik)
# rm(P_nenavratenia_Discretive)
# rm(P_nenavratenia_Cross_EMA)
# rm(P_nenavratenia_MaxMin)
# rm(P_nenavratenia_Risk_Tolerance)
# rm(Strata_100_0)
# rm(Strata_90_10)
# rm(Strata_75_25)
# rm(Strata_60_40)
# rm(Strata_50_50)
# rm(Strata_40_60)
# rm(Strata_10_90)
# rm(Strata_0_100)
# rm(Strata_PIS)
# rm(Strata_Svedsko)
# rm(Strata_Cile)
# rm(Strata_Aging_1)
# rm(Strata_Aging_2)
# rm(Strata_Aging_3)
# rm(Strata_Aging_4)
# rm(Strata_Cerny_Melichercik)
# rm(Strata_Discretive)
# rm(Strata_Cross_EMA)
# rm(Strata_MaxMin)
# rm(Strata_Risk_Tolerance)

# Priemerný ročný výnos a jeho variancia ####
# Priemerné ročné výnosy jednotlivých sporivých stratégií:
# Definovanie objektov:
Denny_vynos_100_0 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_90_10 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_75_25 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_60_40 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_50_50 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_40_60 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_25_75 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_10_90 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_0_100 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_PIS <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Svedsko <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Cile <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Aging_1 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Aging_2 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Aging_3 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Aging_4 <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Discretive <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_MaxMin <- matrix(NA, nrow = 10000, ncol = 9580)
Denny_vynos_Risk_Tolerance <- matrix(NA, nrow = 10000, ncol = 9580)

for (i in 1:10000)
{
  for (j in 21:9600)
  {
    # Výpočet denného výnosu jednotlivých stratégií:
    Denny_vynos_100_0[i,(j-20)] <- (Nasporene_100_0[i,j]/(Nasporene_100_0[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_90_10[i,(j-20)] <- (Nasporene_90_10[i,j]/(Nasporene_90_10[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_75_25[i,(j-20)] <- (Nasporene_75_25[i,j]/(Nasporene_75_25[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_60_40[i,(j-20)] <- (Nasporene_60_40[i,j]/(Nasporene_60_40[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_50_50[i,(j-20)] <- (Nasporene_50_50[i,j]/(Nasporene_50_50[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_40_60[i,(j-20)] <- (Nasporene_40_60[i,j]/(Nasporene_40_60[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_25_75[i,(j-20)] <- (Nasporene_25_75[i,j]/(Nasporene_25_75[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_10_90[i,(j-20)] <- (Nasporene_10_90[i,j]/(Nasporene_10_90[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_0_100[i,(j-20)] <- (Nasporene_0_100[i,j]/(Nasporene_0_100[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_PIS[i,(j-20)] <- (Nasporene_PIS[i,j]/(Nasporene_PIS[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Svedsko[i,(j-20)] <- (Nasporene_Svedsko[i,j]/(Nasporene_Svedsko[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Cile[i,(j-20)] <- (Nasporene_Cile[i,j]/(Nasporene_Cile[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Aging_1[i,(j-20)] <- (Nasporene_Aging_1[i,j]/(Nasporene_Aging_1[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Aging_2[i,(j-20)] <- (Nasporene_Aging_2[i,j]/(Nasporene_Aging_2[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Aging_3[i,(j-20)] <- (Nasporene_Aging_3[i,j]/(Nasporene_Aging_3[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Aging_4[i,(j-20)] <- (Nasporene_Aging_4[i,j]/(Nasporene_Aging_4[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Cerny_Melichercik[i,(j-20)] <- (Nasporene_Cerny_Melichercik[i,j]/(Nasporene_Cerny_Melichercik[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Discretive[i,(j-20)] <- (Nasporene_Discretive[i,j]/(Nasporene_Discretive[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Cross_EMA[i,(j-20)] <- (Nasporene_Cross_EMA[i,j]/(Nasporene_Cross_EMA[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_MaxMin[i,(j-20)] <- (Nasporene_MaxMin[i,j]/(Nasporene_MaxMin[i,(j-1)]+Odvody_den[i,j]))-1
    Denny_vynos_Risk_Tolerance[i,(j-20)] <- (Nasporene_Risk_Tolerance[i,j]/(Nasporene_Risk_Tolerance[i,(j-1)]+Odvody_den[i,j]))-1
  }  
}

# Výpočet priemerného ročného výnosu jednotlivých stratégií:
Priemerny_rocny_vynos_100_0 <- mean(Denny_vynos_100_0)*240
Priemerny_rocny_vynos_90_10 <- mean(Denny_vynos_90_10)*240
Priemerny_rocny_vynos_75_25 <- mean(Denny_vynos_75_25)*240
Priemerny_rocny_vynos_60_40 <- mean(Denny_vynos_60_40)*240
Priemerny_rocny_vynos_50_50 <- mean(Denny_vynos_50_50)*240
Priemerny_rocny_vynos_40_60 <- mean(Denny_vynos_40_60)*240
Priemerny_rocny_vynos_25_75 <- mean(Denny_vynos_25_75)*240
Priemerny_rocny_vynos_10_90 <- mean(Denny_vynos_10_90)*240
Priemerny_rocny_vynos_0_100 <- mean(Denny_vynos_0_100)*240
Priemerny_rocny_vynos_PIS <- mean(Denny_vynos_PIS)*240
Priemerny_rocny_vynos_Svedsko <- mean(Denny_vynos_Svedsko)*240
Priemerny_rocny_vynos_Cile <- mean(Denny_vynos_Cile)*240
Priemerny_rocny_vynos_Aging_1 <- mean(Denny_vynos_Aging_1)*240
Priemerny_rocny_vynos_Aging_2 <- mean(Denny_vynos_Aging_2)*240
Priemerny_rocny_vynos_Aging_3 <- mean(Denny_vynos_Aging_3)*240
Priemerny_rocny_vynos_Aging_4 <- mean(Denny_vynos_Aging_4)*240
Priemerny_rocny_vynos_Cerny_Melichercik <- mean(Denny_vynos_Cerny_Melichercik)*240
Priemerny_rocny_vynos_Discretive <- mean(Denny_vynos_Discretive)*240
Priemerny_rocny_vynos_Cross_EMA <- mean(Denny_vynos_Cross_EMA)*240
Priemerny_rocny_vynos_MaxMin <- mean(Denny_vynos_MaxMin)*240
Priemerny_rocny_vynos_Risk_Tolerance <- mean(Denny_vynos_Risk_Tolerance)*240

# Percentuálny priemerný ročný výnos jednotlivých stratégií:
round(Priemerny_rocny_vynos_100_0,4)*100
round(Priemerny_rocny_vynos_90_10,4)*100
round(Priemerny_rocny_vynos_75_25,4)*100
round(Priemerny_rocny_vynos_60_40,4)*100
round(Priemerny_rocny_vynos_50_50,4)*100
round(Priemerny_rocny_vynos_40_60,4)*100
round(Priemerny_rocny_vynos_25_75,4)*100
round(Priemerny_rocny_vynos_10_90,4)*100
round(Priemerny_rocny_vynos_0_100,4)*100
round(Priemerny_rocny_vynos_PIS,4)*100
round(Priemerny_rocny_vynos_Svedsko,4)*100
round(Priemerny_rocny_vynos_Cile,4)*100
round(Priemerny_rocny_vynos_Aging_1,4)*100
round(Priemerny_rocny_vynos_Aging_2,4)*100
round(Priemerny_rocny_vynos_Aging_3,4)*100
round(Priemerny_rocny_vynos_Aging_4,4)*100
round(Priemerny_rocny_vynos_Cerny_Melichercik,4)*100
round(Priemerny_rocny_vynos_Discretive,4)*100
round(Priemerny_rocny_vynos_Cross_EMA,4)*100
round(Priemerny_rocny_vynos_MaxMin,4)*100
round(Priemerny_rocny_vynos_Risk_Tolerance,4)*100

# Výpočet volatility priemerného ročného výnosu jednotlivých sporivých stratégií:
Volatilita_100_0 <- sd(Denny_vynos_100_0)*sqrt(240)
Volatilita_90_10 <- sd(Denny_vynos_90_10)*sqrt(240)
Volatilita_75_25 <- sd(Denny_vynos_75_25)*sqrt(240)
Volatilita_60_40 <- sd(Denny_vynos_60_40)*sqrt(240)
Volatilita_50_50 <- sd(Denny_vynos_50_50)*sqrt(240)
Volatilita_40_60 <- sd(Denny_vynos_40_60)*sqrt(240)
Volatilita_25_75 <- sd(Denny_vynos_25_75)*sqrt(240)
Volatilita_10_90 <- sd(Denny_vynos_10_90)*sqrt(240)
Volatilita_0_100 <- sd(Denny_vynos_0_100)*sqrt(240)
Volatilita_PIS <- sd(Denny_vynos_PIS)*sqrt(240)
Volatilita_Svedsko <- sd(Denny_vynos_Svedsko)*sqrt(240)
Volatilita_Cile <- sd(Denny_vynos_Cile)*sqrt(240)
Volatilita_Aging_1 <- sd(Denny_vynos_Aging_1)*sqrt(240)
Volatilita_Aging_2 <- sd(Denny_vynos_Aging_2)*sqrt(240)
Volatilita_Aging_3 <- sd(Denny_vynos_Aging_3)*sqrt(240)
Volatilita_Aging_4 <- sd(Denny_vynos_Aging_4)*sqrt(240)
Volatilita_Cerny_Melichercik <- sd(Denny_vynos_Cerny_Melichercik)*sqrt(240)
Volatilita_Discretive <- sd(Denny_vynos_Discretive)*sqrt(240)
Volatilita_Cross_EMA <- sd(Denny_vynos_Cross_EMA)*sqrt(240)
Volatilita_MaxMin <- sd(Denny_vynos_MaxMin)*sqrt(240)
Volatilita_Risk_Tolerance <- sd(Denny_vynos_Risk_Tolerance)*sqrt(240)

# Percentuálna volatilita priemerného ročného výnosu jednotlivých sporivých stratégií:
round(Volatilita_100_0,4)*100
round(Volatilita_90_10,4)*100
round(Volatilita_75_25,4)*100
round(Volatilita_60_40,4)*100
round(Volatilita_50_50,4)*100
round(Volatilita_40_60,4)*100
round(Volatilita_25_75,4)*100
round(Volatilita_10_90,4)*100
round(Volatilita_0_100,4)*100
round(Volatilita_PIS,4)*100
round(Volatilita_Svedsko,4)*100
round(Volatilita_Cile,4)*100
round(Volatilita_Aging_1,4)*100
round(Volatilita_Aging_2,4)*100
round(Volatilita_Aging_3,4)*100
round(Volatilita_Aging_4,4)*100
round(Volatilita_Cerny_Melichercik,4)*100
round(Volatilita_Discretive,4)*100
round(Volatilita_Cross_EMA,4)*100
round(Volatilita_MaxMin,4)*100
round(Volatilita_Risk_Tolerance,4)*100
rm(Odvody_den)

# Vykreslenie závislosti priemerného ročného výnosu od volatility priemerného ročného výnosu
{Vynos <- c(Priemerny_rocny_vynos_100_0, Priemerny_rocny_vynos_90_10, Priemerny_rocny_vynos_75_25, 
            Priemerny_rocny_vynos_60_40, Priemerny_rocny_vynos_50_50, Priemerny_rocny_vynos_40_60, 
            Priemerny_rocny_vynos_25_75, Priemerny_rocny_vynos_10_90, Priemerny_rocny_vynos_0_100, 
            Priemerny_rocny_vynos_PIS, Priemerny_rocny_vynos_Svedsko, Priemerny_rocny_vynos_Cile, 
            Priemerny_rocny_vynos_Aging_1, Priemerny_rocny_vynos_Aging_2, Priemerny_rocny_vynos_Aging_3, 
            Priemerny_rocny_vynos_Aging_4, Priemerny_rocny_vynos_Cerny_Melichercik, Priemerny_rocny_vynos_Discretive, 
            Priemerny_rocny_vynos_Cross_EMA, Priemerny_rocny_vynos_MaxMin, Priemerny_rocny_vynos_Risk_Tolerance)}

{Volatilita <- c(Volatilita_100_0, Volatilita_90_10, Volatilita_75_25, 
                 Volatilita_60_40, Volatilita_50_50, Volatilita_40_60, 
                 Volatilita_25_75, Volatilita_10_90, Volatilita_0_100,
                 Volatilita_PIS, Volatilita_Svedsko, Volatilita_Cile,
                 Volatilita_Aging_1, Volatilita_Aging_2, Volatilita_Aging_3, 
                 Volatilita_Aging_4, Volatilita_Cerny_Melichercik, Volatilita_Discretive, 
                 Volatilita_Cross_EMA, Volatilita_MaxMin, Volatilita_Risk_Tolerance)}

windows()

{plot(Volatilita, Vynos, xlim=c(0, 0.198), ylim=c(-0.02, 0.13), main="Závislosť priemerného ročného výnosu od volatility priemerného ročného výnosu", xlab="Volatilita priemerného ročného výnosu", ylab="Priemerný ročný výnos", 
      col=c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold"
            ,"darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, cex=1.2)}
par(xpd=TRUE)
{legend(-0.008,0.136, legend=c("100:0","90:10","75:25","60:40","50:50","40:60","25:75","10:90","0:100","PIS","Švédsko","Čile", 
                               "Aging1","Aging2","Aging3","Aging4","Černý-Melicherčík","Discretive","CrossEMA","MaxMin","RiskTolerance"), 
        col= c("blue","red","orange4","yellow4","green","purple","pink","magenta","brown","black","darkgray","deeppink","gold",
               "darkgreen","red3","lightgreen","orchid","darkorange","cyan","lightblue","peachpuff"), pch=16, ncol=3, cex=1)}

# Odstránenie ďalej nepotrebných objektov:
# rm(Vynos)
# rm(Priemerny_rocny_vynos_100_0)
# rm(Priemerny_rocny_vynos_90_10)
# rm(Priemerny_rocny_vynos_75_25)
# rm(Priemerny_rocny_vynos_60_40)
# rm(Priemerny_rocny_vynos_50_50)
# rm(Priemerny_rocny_vynos_40_60)
# rm(Priemerny_rocny_vynos_25_75)
# rm(Priemerny_rocny_vynos_10_90)
# rm(Priemerny_rocny_vynos_0_100)
# rm(Priemerny_rocny_vynos_PIS)
# rm(Priemerny_rocny_vynos_Svedsko)
# rm(Priemerny_rocny_vynos_Cile)
# rm(Priemerny_rocny_vynos_Aging_1)
# rm(Priemerny_rocny_vynos_Aging_2)
# rm(Priemerny_rocny_vynos_Aging_3)
# rm(Priemerny_rocny_vynos_Aging_4)
# rm(Priemerny_rocny_vynos_Cerny_Melichercik)
# rm(Priemerny_rocny_vynos_Discretive)
# rm(Priemerny_rocny_vynos_Cross_EMA)
# rm(Priemerny_rocny_vynos_MaxMin)
# rm(Priemerny_rocny_vynos_Risk_Tolerance)
# rm(Volatilita_100_0)
# rm(Volatilita_90_10)
# rm(Volatilita_75_25)
# rm(Volatilita_60_40)
# rm(Volatilita_50_50)
# rm(Volatilita_40_60)
# rm(Volatilita_25_75)
# rm(Volatilita_10_90)
# rm(Volatilita_0_100)
# rm(Volatilita_PIS)
# rm(Volatilita_Svedsko)
# rm(Volatilita_Cile)
# rm(Volatilita_Aging_1)
# rm(Volatilita_Aging_2)
# rm(Volatilita_Aging_3)
# rm(Volatilita_Aging_4)
# rm(Volatilita_Cerny_Melichercik)
# rm(Volatilita_Discretive)
# rm(Volatilita_Cross_EMA)
# rm(Volatilita_MaxMin)
# rm(Volatilita_Risk_Tolerance)
# rm(Volatilita)

# Zmeny pomeru ####
# Počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
# Definovanie objektov:
Pocet_zmien_pomeru_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 1)
Pocet_zmien_pomeru_Discretive <- matrix(NA, nrow = 10000, ncol = 1)
Pocet_zmien_pomeru_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 1)
Pocet_zmien_pomeru_MaxMin <- matrix(NA, nrow = 10000, ncol = 1)

# Výpočet počťu vykonaných zmien alokačného pomeru:
for (i in 1:10000)
{ 
  Pocet_zmien_pomeru_Cerny_Melichercik[i] <- 0
  Pocet_zmien_pomeru_Discretive[i] <- 0
  Pocet_zmien_pomeru_Cross_EMA[i] <- 0
  Pocet_zmien_pomeru_MaxMin[i] <- 0
  
  for (z in 2:480)
  {
    if (Podiel_index_Cerny_Melichercik[i,z] != Podiel_index_Cerny_Melichercik[i,z-1])
    {
      Pocet_zmien_pomeru_Cerny_Melichercik[i] <- Pocet_zmien_pomeru_Cerny_Melichercik[i] + 1 
    }
    if (Podiel_index_Discretive[i,z] != Podiel_index_Discretive[i,z-1])
    {
      Pocet_zmien_pomeru_Discretive[i] <- Pocet_zmien_pomeru_Discretive[i] + 1 
    }
    if (Podiel_index_Cross_EMA[i,z] != Podiel_index_Cross_EMA[i,z-1])
    {
      Pocet_zmien_pomeru_Cross_EMA[i] <- Pocet_zmien_pomeru_Cross_EMA[i] + 1 
    }
    if (Podiel_index_MaxMin[i,z] != Podiel_index_MaxMin[i,z-1])
    {
      Pocet_zmien_pomeru_MaxMin[i] <- Pocet_zmien_pomeru_MaxMin[i] + 1 
    }
  }
}

# Priemerný počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
round(mean(Pocet_zmien_pomeru_Cerny_Melichercik),0)
round(mean(Pocet_zmien_pomeru_Discretive),0)
round(mean(Pocet_zmien_pomeru_Cross_EMA),0)
round(mean(Pocet_zmien_pomeru_MaxMin),0)

# Mediánový počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
median(Pocet_zmien_pomeru_Cerny_Melichercik)
median(Pocet_zmien_pomeru_Discretive)
median(Pocet_zmien_pomeru_Cross_EMA)
median(Pocet_zmien_pomeru_MaxMin)

# Maximálny počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
max(Pocet_zmien_pomeru_Cerny_Melichercik)
max(Pocet_zmien_pomeru_Discretive)
max(Pocet_zmien_pomeru_Cross_EMA)
max(Pocet_zmien_pomeru_MaxMin)

# Minimálny počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
min(Pocet_zmien_pomeru_Cerny_Melichercik)
min(Pocet_zmien_pomeru_Discretive)
min(Pocet_zmien_pomeru_Cross_EMA)
min(Pocet_zmien_pomeru_MaxMin)

# 5% kvantil pre počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
quantile(Pocet_zmien_pomeru_Cerny_Melichercik, probs=0.05)
quantile(Pocet_zmien_pomeru_Discretive, probs= 0.05)
quantile(Pocet_zmien_pomeru_Cross_EMA, probs= 0.05)
quantile(Pocet_zmien_pomeru_MaxMin, probs= 0.05)

# 95% kvantil pre počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
quantile(Pocet_zmien_pomeru_Cerny_Melichercik, probs=0.95)
quantile(Pocet_zmien_pomeru_Discretive, probs= 0.95)
quantile(Pocet_zmien_pomeru_Cross_EMA, probs= 0.95)
quantile(Pocet_zmien_pomeru_MaxMin, probs= 0.95)

# Štandardná odchýlka pre počet vykonaných zmien alokačného pomeru pre jednotlivé sporivé stratégie:
round(sd(Pocet_zmien_pomeru_Cerny_Melichercik),0)
round(sd(Pocet_zmien_pomeru_Discretive),0)
round(sd(Pocet_zmien_pomeru_Cross_EMA),0)
round(sd(Pocet_zmien_pomeru_MaxMin),0)

# Odstránenie ďalej nepotrebných objektov:
# rm(Podiel_index_Cerny_Melichercik)
# rm(Podiel_index_Discretive)
# rm(Podiel_index_Cross_EMA)
# rm(Podiel_index_MaxMin)
# rm(Pocet_zmien_pomeru_Cerny_Melichercik)
# rm(Pocet_zmien_pomeru_Discretive)
# rm(Pocet_zmien_pomeru_Cross_EMA)
# rm(Pocet_zmien_pomeru_MaxMin)
# rm(t)
# rm(gamma)
# rm(k)
# rm(sigma_denna)
# rm(sigma_rocna)
# rm(r_denne)
# rm(r_rocne)
# rm(r)
# rm(Korelacia)
# rm(pocet_iteracii)
# rm(p_s)
# rm(p_l)
# rm(Odvody_mesiac)
# rm(Odvody_celkom)
# rm(o)
# rm(N)
# rm(n)
# rm(mu)
# rm(Min_dlhopis)
# rm(Min_dlhopis_pol)
# rm(mzdy)

# Maximálny absolútny prepad ####
# Možno nastane error, na funkčnosť zvyšku kódu treba odkomentovať a spustiť časti kódu s názvom # Odstránenie ďalej nepotrebných objektov: 
# Definovanie objektov:
Max_U_t_100_0 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_100_0[,1] <- Nasporene_100_0[,20]
Max_U_t_90_10 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_90_10[,1] <- Nasporene_90_10[,20]
Max_U_t_75_25 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_75_25[,1] <- Nasporene_75_25[,20]
Max_U_t_60_40 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_60_40[,1] <- Nasporene_60_40[,20]
Max_U_t_50_50 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_50_50[,1] <- Nasporene_50_50[,20]
Max_U_t_40_60 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_40_60[,1] <- Nasporene_40_60[,20]
Max_U_t_25_75 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_25_75[,1] <- Nasporene_25_75[,20]
Max_U_t_10_90 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_10_90[,1] <- Nasporene_10_90[,20]
Max_U_t_0_100 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_0_100[,1] <- Nasporene_0_100[,20]
Max_U_t_PIS <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_PIS[,1] <- Nasporene_PIS[,20]
Max_U_t_Svedsko <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Svedsko[,1] <- Nasporene_Svedsko[,20]
Max_U_t_Cile <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Cile[,1] <- Nasporene_Cile[,20]
Max_U_t_Aging_1 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Aging_1[,1] <- Nasporene_Aging_1[,20]
Max_U_t_Aging_2 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Aging_2[,1] <- Nasporene_Aging_2[,20]
Max_U_t_Aging_3 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Aging_3[,1] <- Nasporene_Aging_3[,20]
Max_U_t_Aging_4 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Aging_4[,1] <- Nasporene_Aging_4[,20]
Max_U_t_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Cerny_Melichercik[,1] <- Nasporene_Cerny_Melichercik[,20]
Max_U_t_Discretive <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Discretive[,1] <- Nasporene_Discretive[,20]
Max_U_t_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Cross_EMA[,1] <- Nasporene_Cross_EMA[,20]
Max_U_t_MaxMin <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_MaxMin[,1] <- Nasporene_MaxMin[,20]
Max_U_t_Risk_Tolerance <- matrix(NA, nrow = 10000, ncol = 9580)
Max_U_t_Risk_Tolerance[,1] <- Nasporene_Risk_Tolerance[,20]

Max_Absolut_Drawdown_100_0 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_100_0[,1] <- 0
Max_Absolut_Drawdown_90_10 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_90_10[,1] <- 0
Max_Absolut_Drawdown_75_25 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_75_25[,1] <- 0
Max_Absolut_Drawdown_60_40 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_60_40[,1] <- 0
Max_Absolut_Drawdown_50_50 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_50_50[,1] <- 0
Max_Absolut_Drawdown_40_60 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_40_60[,1] <- 0
Max_Absolut_Drawdown_25_75 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_25_75[,1] <- 0
Max_Absolut_Drawdown_10_90 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_10_90[,1] <- 0
Max_Absolut_Drawdown_0_100 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_0_100[,1] <- 0
Max_Absolut_Drawdown_PIS <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_PIS[,1] <- 0
Max_Absolut_Drawdown_Svedsko <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Svedsko[,1] <- 0
Max_Absolut_Drawdown_Cile <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Cile[,1] <- 0
Max_Absolut_Drawdown_Aging_1 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Aging_1[,1] <- 0
Max_Absolut_Drawdown_Aging_2 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Aging_2[,1] <- 0
Max_Absolut_Drawdown_Aging_3 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Aging_3[,1] <- 0
Max_Absolut_Drawdown_Aging_4 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Aging_4[,1] <- 0
Max_Absolut_Drawdown_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Cerny_Melichercik[,1] <- 0
Max_Absolut_Drawdown_Discretive <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Discretive[,1] <- 0
Max_Absolut_Drawdown_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Cross_EMA[,1] <-0
Max_Absolut_Drawdown_MaxMin <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_MaxMin[,1] <- 0
Max_Absolut_Drawdown_Risk_Tolerance <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Absolut_Drawdown_Risk_Tolerance[,1] <- 0

# Výpočet maximálneho absolútneho prepadu:
for (i in 1:10000)
{ 
  for (j in 2:9580)
  {
    Max_U_t_100_0[i,j] <- max(Nasporene_100_0[i,j+19],Max_U_t_100_0[i,j-1])
    Max_Absolut_Drawdown_100_0[i,j] <- min((Nasporene_100_0[i,j+19]-Max_U_t_100_0[i,j]),Max_Absolut_Drawdown_100_0[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_90_10[i,j] <- max(Nasporene_90_10[i,j+19],Max_U_t_90_10[i,j-1])
    Max_Absolut_Drawdown_90_10[i,j] <- min((Nasporene_90_10[i,j+19]-Max_U_t_90_10[i,j]),Max_Absolut_Drawdown_90_10[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_75_25[i,j] <- max(Nasporene_75_25[i,j+19],Max_U_t_75_25[i,j-1])
    Max_Absolut_Drawdown_75_25[i,j] <- min((Nasporene_75_25[i,j+19]-Max_U_t_75_25[i,j]),Max_Absolut_Drawdown_75_25[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_60_40[i,j] <- max(Nasporene_60_40[i,j+19],Max_U_t_60_40[i,j-1])
    Max_Absolut_Drawdown_60_40[i,j] <- min((Nasporene_60_40[i,j+19]-Max_U_t_60_40[i,j]),Max_Absolut_Drawdown_60_40[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_50_50[i,j] <- max(Nasporene_50_50[i,j+19],Max_U_t_50_50[i,j-1])
    Max_Absolut_Drawdown_50_50[i,j] <- min((Nasporene_50_50[i,j+19]-Max_U_t_50_50[i,j]),Max_Absolut_Drawdown_50_50[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_40_60[i,j] <- max(Nasporene_40_60[i,j+19],Max_U_t_40_60[i,j-1])
    Max_Absolut_Drawdown_40_60[i,j] <- min((Nasporene_40_60[i,j+19]-Max_U_t_40_60[i,j]),Max_Absolut_Drawdown_40_60[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_25_75[i,j] <- max(Nasporene_25_75[i,j+19],Max_U_t_25_75[i,j-1])
    Max_Absolut_Drawdown_25_75[i,j] <- min((Nasporene_25_75[i,j+19]-Max_U_t_25_75[i,j]),Max_Absolut_Drawdown_25_75[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_10_90[i,j] <- max(Nasporene_10_90[i,j+19],Max_U_t_10_90[i,j-1])
    Max_Absolut_Drawdown_10_90[i,j] <- min((Nasporene_10_90[i,j+19]-Max_U_t_10_90[i,j]),Max_Absolut_Drawdown_10_90[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_0_100[i,j] <- max(Nasporene_0_100[i,j+19],Max_U_t_0_100[i,j-1])
    Max_Absolut_Drawdown_0_100[i,j] <- min((Nasporene_0_100[i,j+19]-Max_U_t_0_100[i,j]),Max_Absolut_Drawdown_0_100[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_PIS[i,j] <- max(Nasporene_PIS[i,j+19],Max_U_t_PIS[i,j-1])
    Max_Absolut_Drawdown_PIS[i,j] <- min((Nasporene_PIS[i,j+19]-Max_U_t_PIS[i,j]),Max_Absolut_Drawdown_PIS[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Svedsko[i,j] <- max(Nasporene_Svedsko[i,j+19],Max_U_t_Svedsko[i,j-1])
    Max_Absolut_Drawdown_Svedsko[i,j] <- min((Nasporene_Svedsko[i,j+19]-Max_U_t_Svedsko[i,j]),Max_Absolut_Drawdown_Svedsko[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Cile[i,j] <- max(Nasporene_Cile[i,j+19],Max_U_t_Cile[i,j-1])
    Max_Absolut_Drawdown_Cile[i,j] <- min((Nasporene_Cile[i,j+19]-Max_U_t_Cile[i,j]),Max_Absolut_Drawdown_Cile[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Aging_1[i,j] <- max(Nasporene_Aging_1[i,j+19],Max_U_t_Aging_1[i,j-1])
    Max_Absolut_Drawdown_Aging_1[i,j] <- min((Nasporene_Aging_1[i,j+19]-Max_U_t_Aging_1[i,j]),Max_Absolut_Drawdown_Aging_1[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Aging_2[i,j] <- max(Nasporene_Aging_2[i,j+19],Max_U_t_Aging_2[i,j-1])
    Max_Absolut_Drawdown_Aging_2[i,j] <- min((Nasporene_Aging_2[i,j+19]-Max_U_t_Aging_2[i,j]),Max_Absolut_Drawdown_Aging_2[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Aging_3[i,j] <- max(Nasporene_Aging_3[i,j+19],Max_U_t_Aging_3[i,j-1])
    Max_Absolut_Drawdown_Aging_3[i,j] <- min((Nasporene_Aging_3[i,j+19]-Max_U_t_Aging_3[i,j]),Max_Absolut_Drawdown_Aging_3[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Aging_4[i,j] <- max(Nasporene_Aging_4[i,j+19],Max_U_t_Aging_4[i,j-1])
    Max_Absolut_Drawdown_Aging_4[i,j] <- min((Nasporene_Aging_4[i,j+19]-Max_U_t_Aging_4[i,j]),Max_Absolut_Drawdown_Aging_4[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Cerny_Melichercik[i,j] <- max(Nasporene_Cerny_Melichercik[i,j+19],Max_U_t_Cerny_Melichercik[i,j-1])
    Max_Absolut_Drawdown_Cerny_Melichercik[i,j] <- min((Nasporene_Cerny_Melichercik[i,j+19]-Max_U_t_Cerny_Melichercik[i,j]),Max_Absolut_Drawdown_Cerny_Melichercik[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Discretive[i,j] <- max(Nasporene_Discretive[i,j+19],Max_U_t_Discretive[i,j-1])
    Max_Absolut_Drawdown_Discretive[i,j] <- min((Nasporene_Discretive[i,j+19]-Max_U_t_Discretive[i,j]),Max_Absolut_Drawdown_Discretive[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Cross_EMA[i,j] <- max(Nasporene_Cross_EMA[i,j+19],Max_U_t_Cross_EMA[i,j-1])
    Max_Absolut_Drawdown_Cross_EMA[i,j] <- min((Nasporene_Cross_EMA[i,j+19]-Max_U_t_Cross_EMA[i,j]),Max_Absolut_Drawdown_Cross_EMA[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_MaxMin[i,j] <- max(Nasporene_MaxMin[i,j+19],Max_U_t_MaxMin[i,j-1])
    Max_Absolut_Drawdown_MaxMin[i,j] <- min((Nasporene_MaxMin[i,j+19]-Max_U_t_MaxMin[i,j]),Max_Absolut_Drawdown_MaxMin[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_U_t_Risk_Tolerance[i,j] <- max(Nasporene_Risk_Tolerance[i,j+19],Max_U_t_Risk_Tolerance[i,j-1])
    Max_Absolut_Drawdown_Risk_Tolerance[i,j] <- min((Nasporene_Risk_Tolerance[i,j+19]-Max_U_t_Risk_Tolerance[i,j]),Max_Absolut_Drawdown_Risk_Tolerance[i,j-1])
  }
  
}

# Priemerný maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(mean(Max_Absolut_Drawdown_100_0[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_90_10[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_75_25[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_60_40[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_50_50[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_40_60[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_25_75[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_10_90[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_0_100[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_PIS[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Svedsko[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Cile[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Aging_1[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Aging_2[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Aging_3[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Aging_4[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Cerny_Melichercik[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Discretive[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Cross_EMA[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_MaxMin[,9580]),-3)/1000
round(mean(Max_Absolut_Drawdown_Risk_Tolerance[,9580]),-3)/1000

# Mediánový maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(median(Max_Absolut_Drawdown_100_0[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_90_10[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_75_25[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_60_40[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_50_50[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_40_60[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_25_75[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_10_90[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_0_100[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_PIS[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Svedsko[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Cile[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Aging_1[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Aging_2[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Aging_3[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Aging_4[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Cerny_Melichercik[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Discretive[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Cross_EMA[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_MaxMin[,9580]),-3)/1000
round(median(Max_Absolut_Drawdown_Risk_Tolerance[,9580]),-3)/1000

# Maximálny maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(max(Max_Absolut_Drawdown_100_0[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_90_10[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_75_25[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_60_40[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_50_50[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_40_60[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_25_75[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_10_90[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_0_100[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_PIS[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Svedsko[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Cile[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Aging_1[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Aging_2[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Aging_3[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Aging_4[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Cerny_Melichercik[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Discretive[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Cross_EMA[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_MaxMin[,9580]),-3)/1000
round(max(Max_Absolut_Drawdown_Risk_Tolerance[,9580]),-3)/1000

# Minimálny maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(min(Max_Absolut_Drawdown_100_0[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_90_10[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_75_25[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_60_40[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_50_50[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_40_60[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_25_75[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_10_90[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_0_100[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_PIS[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Svedsko[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Cile[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Aging_1[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Aging_2[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Aging_3[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Aging_4[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Cerny_Melichercik[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Discretive[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Cross_EMA[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_MaxMin[,9580]),-3)/1000
round(min(Max_Absolut_Drawdown_Risk_Tolerance[,9580]),-3)/1000

# 5 % kvantil pre maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(quantile(Max_Absolut_Drawdown_100_0[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_90_10[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_75_25[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_60_40[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_50_50[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_40_60[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_25_75[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_10_90[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_0_100[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_PIS[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Svedsko[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Cile[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_1[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_2[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_3[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_4[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Cerny_Melichercik[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Discretive[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Cross_EMA[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_MaxMin[,9580], probs= 0.05),-3)/1000
round(quantile(Max_Absolut_Drawdown_Risk_Tolerance[,9580], probs= 0.05),-3)/1000

# 95 % kvantil pre maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(quantile(Max_Absolut_Drawdown_100_0[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_90_10[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_75_25[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_60_40[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_50_50[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_40_60[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_25_75[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_10_90[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_0_100[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_PIS[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Svedsko[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Cile[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_1[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_2[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_3[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Aging_4[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Cerny_Melichercik[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Discretive[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Cross_EMA[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_MaxMin[,9580], probs= 0.95),-3)/1000
round(quantile(Max_Absolut_Drawdown_Risk_Tolerance[,9580], probs= 0.95),-3)/1000

# Štandardná odchýlka pre maximálny absolútny prepad pre jednotlivé sporivé stratégie v II.pilieri:
round(sd(Max_Absolut_Drawdown_100_0[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_100_0[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_90_10[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_75_25[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_60_40[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_50_50[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_40_60[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_25_75[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_10_90[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_0_100[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_PIS[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Svedsko[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Cile[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Aging_1[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Aging_2[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Aging_3[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Aging_4[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Cerny_Melichercik[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Discretive[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Cross_EMA[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_MaxMin[,9580]),-3)/1000
round(sd(Max_Absolut_Drawdown_Risk_Tolerance[,9580]),-3)/1000

# Odstránenie ďalej nepotrebných objektov:
# rm(Max_Absolut_Drawdown_100_0)
# rm(Max_Absolut_Drawdown_100_0)
# rm(Max_Absolut_Drawdown_90_10)
# rm(Max_Absolut_Drawdown_75_25)
# rm(Max_Absolut_Drawdown_60_40)
# rm(Max_Absolut_Drawdown_50_50)
# rm(Max_Absolut_Drawdown_40_60)
# rm(Max_Absolut_Drawdown_25_75)
# rm(Max_Absolut_Drawdown_10_90)
# rm(Max_Absolut_Drawdown_0_100)
# rm(Max_Absolut_Drawdown_PIS)
# rm(Max_Absolut_Drawdown_Svedsko)
# rm(Max_Absolut_Drawdown_Cile)
# rm(Max_Absolut_Drawdown_Aging_1)
# rm(Max_Absolut_Drawdown_Aging_2)
# rm(Max_Absolut_Drawdown_Aging_3)
# rm(Max_Absolut_Drawdown_Aging_4)
# rm(Max_Absolut_Drawdown_Cerny_Melichercik)
# rm(Max_Absolut_Drawdown_Discretive)
# rm(Max_Absolut_Drawdown_Cross_EMA)
# rm(Max_Absolut_Drawdown_MaxMin)
# rm(Max_Absolut_Drawdown_Risk_Tolerance)

# Maximálny relatívny prepad #### 
# Definovanie objektov:
Max_Relat_Drawdown_100_0 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_100_0[,1] <- 0
Max_Relat_Drawdown_90_10 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_90_10[,1] <- 0
Max_Relat_Drawdown_75_25 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_75_25[,1] <- 0
Max_Relat_Drawdown_60_40 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_60_40[,1] <- 0
Max_Relat_Drawdown_50_50 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_50_50[,1] <- 0
Max_Relat_Drawdown_40_60 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_40_60[,1] <- 0
Max_Relat_Drawdown_25_75 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_25_75[,1] <- 0
Max_Relat_Drawdown_10_90 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_10_90[,1] <- 0
Max_Relat_Drawdown_0_100 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_0_100[,1] <- 0
Max_Relat_Drawdown_PIS <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_PIS[,1] <- 0
Max_Relat_Drawdown_Svedsko <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Svedsko[,1] <- 0
Max_Relat_Drawdown_Cile <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Cile[,1] <- 0
Max_Relat_Drawdown_Aging_1 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Aging_1[,1] <- 0
Max_Relat_Drawdown_Aging_2 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Aging_2[,1] <- 0
Max_Relat_Drawdown_Aging_3 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Aging_3[,1] <- 0
Max_Relat_Drawdown_Aging_4 <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Aging_4[,1] <- 0
Max_Relat_Drawdown_Cerny_Melichercik <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Cerny_Melichercik[,1] <- 0
Max_Relat_Drawdown_Discretive <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Discretive[,1] <- 0
Max_Relat_Drawdown_Cross_EMA <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Cross_EMA[,1] <-0
Max_Relat_Drawdown_MaxMin <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_MaxMin[,1] <- 0
Max_Relat_Drawdown_Risk_Tolerance <- matrix(NA, nrow = 10000, ncol = 9580)
Max_Relat_Drawdown_Risk_Tolerance[,1] <- 0

# Výpočet maximálneho relatívneho prepadu:
for (i in 1:10000)
{ 
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_100_0[i,j] <- min((((Nasporene_100_0[i,j+19]-Max_U_t_100_0[i,j])/Max_U_t_100_0[i,j])*100),Max_Relat_Drawdown_100_0[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_90_10[i,j] <- min((((Nasporene_90_10[i,j+19]-Max_U_t_90_10[i,j])/Max_U_t_90_10[i,j])*100),Max_Relat_Drawdown_90_10[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_75_25[i,j] <- min((((Nasporene_75_25[i,j+19]-Max_U_t_75_25[i,j])/Max_U_t_75_25[i,j])*100),Max_Relat_Drawdown_75_25[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_60_40[i,j] <- min((((Nasporene_60_40[i,j+19]-Max_U_t_60_40[i,j])/Max_U_t_60_40[i,j])*100),Max_Relat_Drawdown_60_40[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_50_50[i,j] <- min((((Nasporene_50_50[i,j+19]-Max_U_t_50_50[i,j])/Max_U_t_50_50[i,j])*100),Max_Relat_Drawdown_50_50[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_40_60[i,j] <- min((((Nasporene_40_60[i,j+19]-Max_U_t_40_60[i,j])/Max_U_t_40_60[i,j])*100),Max_Relat_Drawdown_40_60[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_25_75[i,j] <- min((((Nasporene_25_75[i,j+19]-Max_U_t_25_75[i,j])/Max_U_t_25_75[i,j])*100),Max_Relat_Drawdown_25_75[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_10_90[i,j] <- min((((Nasporene_10_90[i,j+19]-Max_U_t_10_90[i,j])/Max_U_t_10_90[i,j])*100),Max_Relat_Drawdown_10_90[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_0_100[i,j] <- min((((Nasporene_0_100[i,j+19]-Max_U_t_0_100[i,j])/Max_U_t_0_100[i,j])*100),Max_Relat_Drawdown_0_100[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_PIS[i,j] <- min((((Nasporene_PIS[i,j+19]-Max_U_t_PIS[i,j])/Max_U_t_PIS[i,j])*100),Max_Relat_Drawdown_PIS[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Svedsko[i,j] <- min((((Nasporene_Svedsko[i,j+19]-Max_U_t_Svedsko[i,j])/Max_U_t_Svedsko[i,j])*100),Max_Relat_Drawdown_Svedsko[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Cile[i,j] <- min((((Nasporene_Cile[i,j+19]-Max_U_t_Cile[i,j])/Max_U_t_Cile[i,j])*100),Max_Relat_Drawdown_Cile[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Aging_1[i,j] <- min((((Nasporene_Aging_1[i,j+19]-Max_U_t_Aging_1[i,j])/Max_U_t_Aging_1[i,j])*100),Max_Relat_Drawdown_Aging_1[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Aging_2[i,j] <- min((((Nasporene_Aging_2[i,j+19]-Max_U_t_Aging_2[i,j])/Max_U_t_Aging_2[i,j])*100),Max_Relat_Drawdown_Aging_2[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Aging_3[i,j] <- min((((Nasporene_Aging_3[i,j+19]-Max_U_t_Aging_3[i,j])/Max_U_t_Aging_3[i,j])*100),Max_Relat_Drawdown_Aging_3[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Aging_4[i,j] <- min((((Nasporene_Aging_4[i,j+19]-Max_U_t_Aging_4[i,j])/Max_U_t_Aging_4[i,j])*100),Max_Relat_Drawdown_Aging_4[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Cerny_Melichercik[i,j] <- min((((Nasporene_Cerny_Melichercik[i,j+19]-Max_U_t_Cerny_Melichercik[i,j])/Max_U_t_Cerny_Melichercik[i,j])*100),Max_Relat_Drawdown_Cerny_Melichercik[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Discretive[i,j] <- min((((Nasporene_Discretive[i,j+19]-Max_U_t_Discretive[i,j])/Max_U_t_Discretive[i,j])*100),Max_Relat_Drawdown_Discretive[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Cross_EMA[i,j] <- min((((Nasporene_Cross_EMA[i,j+19]-Max_U_t_Cross_EMA[i,j])/Max_U_t_Cross_EMA[i,j])*100),Max_Relat_Drawdown_Cross_EMA[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_MaxMin[i,j] <- min((((Nasporene_MaxMin[i,j+19]-Max_U_t_MaxMin[i,j])/Max_U_t_MaxMin[i,j])*100),Max_Relat_Drawdown_MaxMin[i,j-1])
  }
  
  for (j in 2:9580)
  {
    Max_Relat_Drawdown_Risk_Tolerance[i,j] <- min((((Nasporene_Risk_Tolerance[i,j+19]-Max_U_t_Risk_Tolerance[i,j])/Max_U_t_Risk_Tolerance[i,j])*100),Max_Relat_Drawdown_Risk_Tolerance[i,j-1])
  }
}

# Priemerný maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
mean(Max_Relat_Drawdown_100_0[,9580])
mean(Max_Relat_Drawdown_90_10[,9580])
mean(Max_Relat_Drawdown_75_25[,9580])
mean(Max_Relat_Drawdown_60_40[,9580])
mean(Max_Relat_Drawdown_50_50[,9580])
mean(Max_Relat_Drawdown_40_60[,9580])
mean(Max_Relat_Drawdown_25_75[,9580])
mean(Max_Relat_Drawdown_10_90[,9580])
mean(Max_Relat_Drawdown_0_100[,9580])
mean(Max_Relat_Drawdown_PIS[,9580])
mean(Max_Relat_Drawdown_Svedsko[,9580])
mean(Max_Relat_Drawdown_Cile[,9580])
mean(Max_Relat_Drawdown_Aging_1[,9580])
mean(Max_Relat_Drawdown_Aging_2[,9580])
mean(Max_Relat_Drawdown_Aging_3[,9580])
mean(Max_Relat_Drawdown_Aging_4[,9580])
mean(Max_Relat_Drawdown_Cerny_Melichercik[,9580])
mean(Max_Relat_Drawdown_Discretive[,9580])
mean(Max_Relat_Drawdown_Cross_EMA[,9580])
mean(Max_Relat_Drawdown_MaxMin[,9580])
mean(Max_Relat_Drawdown_Risk_Tolerance[,9580])

# Mediánový maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
median(Max_Relat_Drawdown_100_0[,9580])
median(Max_Relat_Drawdown_90_10[,9580])
median(Max_Relat_Drawdown_75_25[,9580])
median(Max_Relat_Drawdown_60_40[,9580])
median(Max_Relat_Drawdown_50_50[,9580])
median(Max_Relat_Drawdown_40_60[,9580])
median(Max_Relat_Drawdown_25_75[,9580])
median(Max_Relat_Drawdown_10_90[,9580])
median(Max_Relat_Drawdown_0_100[,9580])
median(Max_Relat_Drawdown_PIS[,9580])
median(Max_Relat_Drawdown_Svedsko[,9580])
median(Max_Relat_Drawdown_Cile[,9580])
median(Max_Relat_Drawdown_Aging_1[,9580])
median(Max_Relat_Drawdown_Aging_2[,9580])
median(Max_Relat_Drawdown_Aging_3[,9580])
median(Max_Relat_Drawdown_Aging_4[,9580])
median(Max_Relat_Drawdown_Cerny_Melichercik[,9580])
median(Max_Relat_Drawdown_Discretive[,9580])
median(Max_Relat_Drawdown_Cross_EMA[,9580])
median(Max_Relat_Drawdown_MaxMin[,9580])
median(Max_Relat_Drawdown_Risk_Tolerance[,9580])

# Maximálny maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
max(Max_Relat_Drawdown_100_0[,9580])
max(Max_Relat_Drawdown_90_10[,9580])
max(Max_Relat_Drawdown_75_25[,9580])
max(Max_Relat_Drawdown_60_40[,9580])
max(Max_Relat_Drawdown_50_50[,9580])
max(Max_Relat_Drawdown_40_60[,9580])
max(Max_Relat_Drawdown_25_75[,9580])
max(Max_Relat_Drawdown_10_90[,9580])
max(Max_Relat_Drawdown_0_100[,9580])
max(Max_Relat_Drawdown_PIS[,9580])
max(Max_Relat_Drawdown_Svedsko[,9580])
max(Max_Relat_Drawdown_Cile[,9580])
max(Max_Relat_Drawdown_Aging_1[,9580])
max(Max_Relat_Drawdown_Aging_2[,9580])
max(Max_Relat_Drawdown_Aging_3[,9580])
max(Max_Relat_Drawdown_Aging_4[,9580])
max(Max_Relat_Drawdown_Cerny_Melichercik[,9580])
max(Max_Relat_Drawdown_Discretive[,9580])
max(Max_Relat_Drawdown_Cross_EMA[,9580])
max(Max_Relat_Drawdown_MaxMin[,9580])
max(Max_Relat_Drawdown_Risk_Tolerance[,9580])

# Minimálny maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
min(Max_Relat_Drawdown_100_0[,9580])
min(Max_Relat_Drawdown_90_10[,9580])
min(Max_Relat_Drawdown_75_25[,9580])
min(Max_Relat_Drawdown_60_40[,9580])
min(Max_Relat_Drawdown_50_50[,9580])
min(Max_Relat_Drawdown_40_60[,9580])
min(Max_Relat_Drawdown_25_75[,9580])
min(Max_Relat_Drawdown_10_90[,9580])
min(Max_Relat_Drawdown_0_100[,9580])
min(Max_Relat_Drawdown_PIS[,9580])
min(Max_Relat_Drawdown_Svedsko[,9580])
min(Max_Relat_Drawdown_Cile[,9580])
min(Max_Relat_Drawdown_Aging_1[,9580])
min(Max_Relat_Drawdown_Aging_2[,9580])
min(Max_Relat_Drawdown_Aging_3[,9580])
min(Max_Relat_Drawdown_Aging_4[,9580])
min(Max_Relat_Drawdown_Cerny_Melichercik[,9580])
min(Max_Relat_Drawdown_Discretive[,9580])
min(Max_Relat_Drawdown_Cross_EMA[,9580])
min(Max_Relat_Drawdown_MaxMin[,9580])
min(Max_Relat_Drawdown_Risk_Tolerance[,9580])

# 5 % kvantil pre maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
quantile(Max_Relat_Drawdown_100_0[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_90_10[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_75_25[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_60_40[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_50_50[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_40_60[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_25_75[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_10_90[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_0_100[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_PIS[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Svedsko[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Cile[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Aging_1[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Aging_2[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Aging_3[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Aging_4[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Cerny_Melichercik[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Discretive[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Cross_EMA[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_MaxMin[,9580], probs= 0.05)
quantile(Max_Relat_Drawdown_Risk_Tolerance[,9580], probs= 0.05)

# 95 % kvantil pre maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
quantile(Max_Relat_Drawdown_100_0[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_90_10[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_75_25[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_60_40[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_50_50[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_40_60[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_25_75[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_10_90[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_0_100[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_PIS[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Svedsko[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Cile[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Aging_1[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Aging_2[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Aging_3[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Aging_4[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Cerny_Melichercik[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Discretive[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Cross_EMA[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_MaxMin[,9580], probs= 0.95)
quantile(Max_Relat_Drawdown_Risk_Tolerance[,9580], probs= 0.95)

# Štandardná odchýlka pre maximálny relatívny prepad pre jednotlivé sporivé stratégie v II.pilieri:
sd(Max_Relat_Drawdown_100_0[,9580])
sd(Max_Relat_Drawdown_100_0[,9580])
sd(Max_Relat_Drawdown_90_10[,9580])
sd(Max_Relat_Drawdown_75_25[,9580])
sd(Max_Relat_Drawdown_60_40[,9580])
sd(Max_Relat_Drawdown_50_50[,9580])
sd(Max_Relat_Drawdown_40_60[,9580])
sd(Max_Relat_Drawdown_25_75[,9580])
sd(Max_Relat_Drawdown_10_90[,9580])
sd(Max_Relat_Drawdown_0_100[,9580])
sd(Max_Relat_Drawdown_PIS[,9580])
sd(Max_Relat_Drawdown_Svedsko[,9580])
sd(Max_Relat_Drawdown_Cile[,9580])
sd(Max_Relat_Drawdown_Aging_1[,9580])
sd(Max_Relat_Drawdown_Aging_2[,9580])
sd(Max_Relat_Drawdown_Aging_3[,9580])
sd(Max_Relat_Drawdown_Aging_4[,9580])
sd(Max_Relat_Drawdown_Cerny_Melichercik[,9580])
sd(Max_Relat_Drawdown_Discretive[,9580])
sd(Max_Relat_Drawdown_Cross_EMA[,9580])
sd(Max_Relat_Drawdown_MaxMin[,9580])
sd(Max_Relat_Drawdown_Risk_Tolerance[,9580])