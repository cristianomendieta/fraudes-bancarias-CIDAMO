#---------------------------------------------------------------------
#link do dataset do kaggle
#Credit Card Transactions Fraud Detection Dataset
#Simulated Credit Card Transactions generated using Sparkov
#https://www.kaggle.com/kartik2112/fraud-detection?select=fraudTrain.csv

#---------------------------------------------------------------------
#link dataprep dadosTrain.csv e dadosTest.csv
#https://drive.google.com/drive/folders/1HSPj5dL_1RDcPEj-KuE4cZBD1FEkvGjg?usp=sharing


#---------------------------------------------------------------------
#DATAPREP - TRAIN

#---------------------------------------------------------------------
#pacotes
require("tidyverse")
require("geosphere")
require("lubridate")

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
dados$date = ymd(dados$date)

#---------------------------------------------------------------------
#converte H:M:S em segundos e inclui na base
sec = period_to_seconds(hms(dados$time)) 
dados$sec = sec

#---------------------------------------------------------------------
#converte F em 0 e M em 1
dados$gender = as.character(dados$gender)
dados$gender[dados$gender == "F"] = 0
dados$gender[dados$gender == "M"] = 1
dados$gender = as.numeric(dados$gender)

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


#---------------------------------------------------------------------
#DATAPREP - TEST

#---------------------------------------------------------------------
#pacotes
require("tidyverse")
require("geosphere")
require("lubridate")

#---------------------------------------------------------------------
#selecionar diretorio
setwd("~/R/cidamo")

#---------------------------------------------------------------------
#importar dados, vazio como NA, string como fatores
dadosT = read.csv("fraudTest.csv", sep = ",",
                 na.strings = "", stringsAsFactors = T)

dadosT = tibble(dadosT)

#---------------------------------------------------------------------
#visualizar dados
View(dadosT)
head(dadosT)
str(dadosT)
summary(dadosT)

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
dadosT[!complete.cases(dadosT),]

#---------------------------------------------------------------------
#duplicados, repetidos
xT = dadosT[duplicated(dadosT$cc_num),]
xT

#---------------------------------------------------------------------
#quantidade de clientes unicos
length(unique(dadosT$cc_num))

#---------------------------------------------------------------------
#separa data e horario da coluna trans_date_trans_time 
dadosT = dadosT %>% 
    separate(col = trans_date_trans_time, 
             into = c("date","time"),
             sep = " ")
dadosT$date = ymd(dadosT$date)

#---------------------------------------------------------------------
#converte H:M:S em segundos e inclui na base
secT= period_to_seconds(hms(dadosT$time)) 
dadosT$sec = secT

#---------------------------------------------------------------------
#converte F em 0 e M em 1
dadosT$gender = as.character(dadosT$gender)
dadosT$gender[dadosT$gender == "F"] = 0
dadosT$gender[dadosT$gender == "M"] = 1
dadosT$gender = as.numeric(dadosT$gender)

#---------------------------------------------------------------------
#inclusao das faixas na base
dadosT = left_join(dadosT, faixascategory3)
dadosT = left_join(dadosT, faixasjob3)
dadosT = left_join(dadosT, faixascity3)

#---------------------------------------------------------------------
#distancia geodesica local transacao - endereco titular
dadosT = dadosT %>%
    mutate(distGeo = distGeo(matrix(c(dadosT$long, dadosT$lat), 
                                    ncol = 2),
                             matrix(c(dadosT$merch_long, dadosT$merch_lat), 
                                    ncol=2)))

#---------------------------------------------------------------------
#exportar dados pra csv
View(dadosT)
write.csv(dadosT, file = "dadosTest.csv")
