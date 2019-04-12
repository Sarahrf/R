install.packages("dplyr") # instalaumfamosopacotedemanipulaÃ§Ã£odedados
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
1. Leia oarquivo TA_PRECOS_MEDICAMENTOS.csv, cujoseparadorÃ©umabarra |.
2. Leia oarquivodecolunasfixas fwf-sample.txt, cujaprimeiracoluna(nomes)temtamanhovinte,a
segunda (estado)temtamanhodezeaterceira(cÃ³digo)temtamanhodoze.
3. InvestigueosparÃ¢metrosdasfunÃ§ÃµesdeleituradoRbase: read.csv(), read.delim() e read.fwf().
Notou asdiferenÃ§asdasfunÃ§Ãµesdo readr?
'''

medicamentos <- read.csv(file="TA_PRECOS_MEDICAMENTOS.csv", sep="\t", dec="|")

head(medicamentos)

fwf <- read.fwf("fwf-sample.txt", widths=c(20, 10, 12),col.names= c("nomes","estado","cÃ³digo"))
head(fwf)




----------------------
automatico <- list.files("C:/Users/srf0615/Documents")
length(automatico) == 0

while (length(automatico) == 0) {
  automatico <- list.files("C:/Users/srf0615/Documents/senado.csv")
  if(length(automatico) > 0) {
      print('O arquivochegou!')
      print('Inicia aleituradosdados')
      print('Faz amanipulação')
      print('Envia emailinformandoconclusãodoscálculos')
   } else {
     print('aguardando arquivo...')
     Sys.sleep(5)
   }
}



------------------------------------
'''
1. Verifiqueaexistênciaderegistros NA em State. Casoexistam,crieumnovodata.frame senado2 sem
esses registroseutilize-oparaospróximosexercícios. Dica: is.na(State)

2. Quais partidosforampartedacoalizãodogoverno?Equaisnãoforam? Dica: filter()

3. Quantossenadorestinhacadapartido?Qualtinhamais?Quaistinhammenos? Dica: group_by(),
summarise() en_distinct()

4. Qual partidovotoumais“sim”?Equalvoltoumenos“sim”? Dica: sum(Vote=='S')

5. Qual regiãodopaístevemaisvotos“sim”?Primeiroseránecessáriocriarumacolunaregiãopara
depoiscontabilizarototaldevotosporregião.
Dica: mutate(Regiao=ifelse(State%in%c(“AM”,“AC”,“TO”,“PA”,“RO”,“RR”),“Norte”,
ifelse(State %in%c(“SP”,“MG”,“RJ”,“ES”),“Sudeste”,ifelse(State%in%c(“MT”,“MS”,
“GO”, “DF”),“Centro-Oeste”,ifelse(State%in%c(“PR”,“SC”,“RS”),“Sul”,“Nordeste”)))))
'''


------------------------------------
jsonlite)
