#dataprep

require("tidyverse")
require("geosphere")

#---------------------------------------------------------------------
#selecionar diretorio
setwd("~/R/cidamo")

#---------------------------------------------------------------------
#importar dados, vazio como NA, string como fatores
dados = read.csv("fraudTrain.csv", sep = ",",
                 na.strings = "", stringsAsFactors = T)

dados = tibble(dados)

#---------------------------------------------------------------------
#visualizar dados
View(dados)
head(dados)
str(dados)
summary(dados)

#---------------------------------------------------------------------
#SIGNIFICADO DAS VARIAVEIS

#index - Unique Identifier for each row
#transdatetrans_time - Transaction DateTime
#cc_num - Credit Card Number of Customer
#merchant - Merchant Name
#category - Category of Merchant
#amt - Amount of Transaction
#first - First Name of Credit Card Holder
#last - Last Name of Credit Card Holder
#gender - Gender of Credit Card Holder
#street - Street Address of Credit Card Holder
#city - City of Credit Card Holder
#state - State of Credit Card Holder
#zip - Zip of Credit Card Holder
#lat - Latitude Location of Credit Card Holder
#long - Longitude Location of Credit Card Holder
#city_pop - Credit Card Holder's City Population
#job - Job of Credit Card Holder
#dob - Date of Birth of Credit Card Holder
#trans_num - Transaction Number
#unix_time - UNIX Time of transaction
#merch_lat - Latitude Location of Merchant
#merch_long - Longitude Location of Merchant
#is_fraud - Fraud Flag <--- Target Class

#---------------------------------------------------------------------
#mostra todas as linhas incompletas
dados[!complete.cases(dados),]

#---------------------------------------------------------------------
#duplicados, repetidos
x = dados[duplicated(dados$cc_num),]
x

#---------------------------------------------------------------------
#quantidade de clientes unicos
length(unique(dados$cc_num))

#---------------------------------------------------------------------
#separa data e horario da coluna trans_date_trans_time 
dados = dados %>% 
    separate(col = trans_date_trans_time, 
             into = c("date","time"),
             sep = " ")
dados


#---------------------------------------------------------------------
##TAXA - category
ratecategory = dados %>%
    group_by(category) %>%
    summarise(fraud = sum(is_fraud[is_fraud==1]),
              total = n(),
              rate = fraud/total) %>%
    arrange(desc(rate))
ratecategory

faixascategory = cut(ratecategory$rate, 
                     breaks = 10, 
                     include.lowest = TRUE,
                     labels = c(seq(1:10)))

faixascategorytb = data.frame(category = ratecategory$category, 
                              faixascategory)
faixascategorytb
table(faixascategory)

faixascategory2 = left_join(ratecategory, faixascategorytb)
faixascategory2

faixascategory3 = data.frame(category = faixascategory2$category, 
                             faixascategory = faixascategory2$faixascategory)
faixascategory3

#---------------------------------------------------------------------
##TAXA - job
ratejob = dados %>%
    group_by(job) %>%
    summarise(fraud = sum(is_fraud[is_fraud==1]),
              total = n(),
              rate = fraud/total) %>%
    arrange(job)
ratejob

sort((unique(ratejob$rate)), decreasing = TRUE)
#1
#0.0518518519
#0.0004848485
#0

faixasjobaux = (0.0518518519 - 0.0004848485)/8
faixasjobaux

faixasjob = cut(ratejob$rate, 
                breaks = c(0, seq(0.000483,0.052,faixasjobaux),1), 
                include.lowest = TRUE,
                labels = c(seq(1:10)))

faixasjobtb = data.frame(job = ratejob$job, faixasjob)
faixasjobtb
table(faixasjob)

faixasjob2 = left_join(ratejob, faixasjobtb)
faixasjob2

faixasjob3 = data.frame(job = faixasjob2$job, 
                        faixasjob = faixasjob2$faixasjob)
faixasjob3

#---------------------------------------------------------------------
##TAXA - city
totalcity = dados %>%
    count(city) %>%
    rename(total = n)
totalcity

fraudcity = dados %>%
    filter(is_fraud==1) %>%
    count(city, .drop = FALSE) %>%
    rename(fraud = n)
fraudcity

ratecity = left_join(fraudcity, totalcity) %>%
    group_by(city) %>%
    summarise(fraud = fraud,
              total = total, 
              rate = fraud / total) %>%
    arrange(city)
ratecity

sort((unique(ratecity$rate)), decreasing = TRUE)
#1
#0.0449218750
#0.0003940887
#0

faixascityaux = (0.0449218750 - 0.0003940887)/8
faixascityaux

faixascity = cut(ratecity$rate, 
                breaks = c(0, seq(0.0003940887,0.045,faixascityaux),1), 
                include.lowest = TRUE,
                labels = c(seq(1:10)))
faixascity

faixascitytb = data.frame(city = ratecity$city, faixascity)
faixascitytb
table(faixascity)

faixascity2 = left_join(ratecity, faixascitytb)
faixascity2

faixascity3 = data.frame(city = faixascity2$city, 
                         faixascity = faixascity2$faixascity)
faixascity3

#---------------------------------------------------------------------
#distancia geodesica local transacao - endereco titular

dados = dados %>%
    mutate(distGeo = distGeo(matrix(c(dados$long, dados$lat), 
                                    ncol = 2),
                             matrix(c(dados$merch_long, dados$merch_lat), 
                                    ncol=2)))

#---------------------------------------------------------------------
#inclusao das faixas na base
dados = left_join(dados, faixascategory3)
dados = left_join(dados, faixasjob3)
dados = left_join(dados, faixascity3)

#---------------------------------------------------------------------
#exportar dados pra csv
View(dados)
write.csv(dados, file = "dadosTrain.csv")
