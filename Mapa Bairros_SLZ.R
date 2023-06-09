##################################################################
##  script para fazer mapas de bairros                         ###
##  Mapa dos bairros de Sao Luis                               ###
##                                                             ###
##  desenvolvido por: Paulo Roberto Carneiro                   ###
##  contato: robertopaulocarneiro@gmail.com                    ###
##                                                             ###
##################################################################

library(sf)           #Para lidar com dados espaciais 
library(dplyr)        #Para manipula��o de dados 
library(readxl)       #Para leitura de arquivos do excel
library(ggplot2)      #Para visualiza��o de dados
library(RColorBrewer) #Para acessar paleta de cores
library(ggspatial)    #Para adicionar elementos de mapas


#Definindo diret�rio de trabalho
setwd('C:/Users/rober/OneDrive/�rea de Trabalho/Arquivos/Arquivos R/trabalhos Paulo Roberto/Mapa BairrosSLZ')
list.files() #verificando arquivos do diret�rio escolhido

read_sf('dados/ma_setores_censitarios/21SEE250GC_SIR.shp')

#atribuindo esses dados a uma vari�vel 

malha <- read_sf('dados/ma_setores_censitarios/21SEE250GC_SIR.shp')
View(malha)

#corrigindo erros de acentua��o 
malha <- read_sf('dados/ma_setores_censitarios/21SEE250GC_SIR.shp',options="ENCODING=WINDOWS-1252")
View(malha)

#filtrando apenas a cidade de Curitiba

dplyr::filter(malha,NM_MUNICIP=="S�O LU�S")

#separando a malha de Curitiba

malha.c <- dplyr::filter(malha,NM_MUNICIP=="S�O LU�S")
View(malha.c)

#separando o setor de Curitiba

malha.c <- dplyr::filter(malha,NM_MUNICIP=="S�O LU�S" & TIPO=='URBANO')
View(malha.c)

#fazendo direto usando o operador %>%

malha <- read_sf('dados/ma_setores_censitarios/21SEE250GC_SIR.shp',options="ENCODING=WINDOWS-1252") %>% 
  dplyr::filter(NM_MUNICIP=="S�O LU�S" & TIPO=='URBANO')

View(malha) #visualizando
rm(malha.c) #apagando malha.c

#plotando a malha territorial do municipio (setores censitarios)
plot(malha$geometry)

### ler arquivos de dados da popula��o para cada setor censitario

read_excel('MA_20171016/MA/Base informa�oes setores2010 universo MA/EXCEL/Basico_MA.xls')

#atribuindo a uma variavel

pop <- read_excel('MA_20171016/MA/Base informa�oes setores2010 universo MA/EXCEL/Basico_MA.xls')
View(pop)

#selecionando as colunas dos dados de interesse 

select(pop, Cod_setor, V002)

#slecionando diretamente utilizando %>%

pop <- read_excel('MA_20171016/MA/Base informa�oes setores2010 universo MA/EXCEL/Basico_MA.xls') %>% 
  select(Cod_setor, V002)
View(pop)

#### #Lendo e manipulando os dados do censo 2010 para o estado o Paran�

pop <- read_excel('MA_20171016/MA/Base informa�oes setores2010 universo MA/EXCEL/Basico_MA.xls') %>% 
  select(Cod_setor, V002) %>% #Selecionando coluna de dados
  mutate(CD_GEOCODI=as.character(Cod_setor)) #Criando nova coluna de dados

View(pop)

#### juntando as duas bases de dados

dados <- left_join(malha,pop)

#########################################
#####      Plotando os dados      #######
#########################################

class(dados) #verificando a natureza dos dados

ggplot(dados)+ #Inserindo os dados 
  geom_sf(aes(fill=V002))+ #Criando um mapa para mostrar o n�mero de habitantes para cada setor censit�rio [V002]
  scale_fill_gradientn(colours = brewer.pal(9,'Spectral'))+ #Definindo uma nova paleta de cores
  labs(fill="Popula��o")+ #Definindo o t�tulo da escala de cores
  theme_minimal() #Definindo o tema do mapa

#Categorizando os dados
dados.b <- dados %>%
  group_by(CD_GEOCODB) %>%  #Agrupando os dados por Bairros
  summarise(pop.total=sum(V002,na.rm = TRUE)) %>% #Determinando o total de habitantes por bairros
  mutate(pop.total.cat=cut(pop.total,breaks=c(200,1000,5000,10000,50000,100000,+Inf), #Categorizando os dados em intervalo de classe
                           labels=c('200-1000','1000-5000','5000-10000','10000-50000','50000-100000','>100000')))


#Fazendo um mapa de Bairros
ggplot(dados.b)+ #Inserindo os dados
  geom_sf(aes(fill=pop.total.cat))+##Criando um mapa para mostrar o n�mero de habitantes para cada Bairro [pop.total.cat]
  scale_fill_manual(values = rev(brewer.pal(6,'Spectral')))+  #Definindo uma nova paleta de cores
  labs(fill="Popula��o")+ #Definindo o t�tulo da escala de cores
  scale_x_continuous(breaks = c(-49.35,-49.27,-49.2))+ #
  annotation_scale(location='br')+ #Iserindo a escala do mapa 
  annotation_north_arrow(location="tl", #Inserindo a orienta��o do mapa
                         style=north_arrow_fancy_orienteering, #Estilo da orienta��o
                         pad_x=unit(-0.2,'cm'), #Espa�o entre a orienta��o e o eixo y
                         width = unit(1.2,'cm'),#Largura da orienta��o do mapa
                         height = unit(1.2, 'cm'))+#Altura da orienta��o do mapa
  
  theme_minimal() #Tema do gr�fico

######################################
##### Salvando o mapa         ########
######################################

ggsave(filename = "mapa_Curitiba.png", #Nome do arquivo
       width = 12, #Largura da figura 
       height = 10,#Altura da figura
       units = 'cm')#Unidade das dimens�es 