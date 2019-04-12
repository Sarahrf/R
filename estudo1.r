install.packages("dplyr") # instalaumfamosopacotedemanipulaçãodedados
install.packages("readxl")



library(dplyr)
library(readxl)

getwd()  # "C:/Users/srf0615/Documents" 

setwd("C:/Sinqia/__Sarah/R/Estudo2_dados/senado.csv")

#senado <- read_csv("senado.csv") #rstudio
senado <- read.csv(file="senado.csv", sep="\t", dec=",")

head(senado) #cabecalho
tail(senado) #rodape ultimos registros
class(senado)
str(senado)
summary(senado) # estatistica

'''
1. Leia oarquivo TA_PRECOS_MEDICAMENTOS.csv, cujoseparadoréumabarra |.
2. Leia oarquivodecolunasfixas fwf-sample.txt, cujaprimeiracoluna(nomes)temtamanhovinte,a
segunda (estado)temtamanhodezeaterceira(código)temtamanhodoze.
3. InvestigueosparâmetrosdasfunçõesdeleituradoRbase: read.csv(), read.delim() e read.fwf().
Notou asdiferençasdasfunçõesdo readr?
'''

medicamentos <- read.csv(file="TA_PRECOS_MEDICAMENTOS.csv", sep="\t", dec="|")

head(medicamentos)

fwf <- read.fwf("fwf-sample.txt", widths=c(20, 10, 12),col.names= c("nomes","estado","código"))
head(fwf)




----------------------
automatico <- list.files("C:/Users/srf0615/Documents")
length(automatico) == 0

while (length(automatico) == 0) {
  automatico <- list.files("C:/Users/srf0615/Documents/senado.csv")
  if(length(automatico) > 0) {
      print('O arquivochegou!')
      print('Inicia aleituradosdados')
      print('Faz amanipula��o')
      print('Envia emailinformandoconclus�odosc�lculos')
   } else {
     print('aguardando arquivo...')
     Sys.sleep(5)
   }
}



------------------------------------
'''
1. Verifiqueaexist�nciaderegistros NA em State. Casoexistam,crieumnovodata.frame senado2 sem
esses registroseutilize-oparaospr�ximosexerc�cios. Dica: is.na(State)

2. Quais partidosforampartedacoaliz�odogoverno?Equaisn�oforam? Dica: filter()

3. Quantossenadorestinhacadapartido?Qualtinhamais?Quaistinhammenos? Dica: group_by(),
summarise() en_distinct()

4. Qual partidovotoumais�sim�?Equalvoltoumenos�sim�? Dica: sum(Vote=='S')

5. Qual regi�odopa�stevemaisvotos�sim�?Primeiroser�necess�riocriarumacolunaregi�opara
depoiscontabilizarototaldevotosporregi�o.
Dica: mutate(Regiao=ifelse(State%in%c(�AM�,�AC�,�TO�,�PA�,�RO�,�RR�),�Norte�,
ifelse(State %in%c(�SP�,�MG�,�RJ�,�ES�),�Sudeste�,ifelse(State%in%c(�MT�,�MS�,
�GO�, �DF�),�Centro-Oeste�,ifelse(State%in%c(�PR�,�SC�,�RS�),�Sul�,�Nordeste�)))))
'''


------------------------------------
jsonlite)
