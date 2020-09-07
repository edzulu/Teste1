rm(list=ls())

setwd("C:\\Users\\ednas\\OneDrive\\Documentos\\_TRABALHO\\IFMT\\MESTRADO\\Aulas_Mestrado\\Aula Red/Aula Semana 2")
pizza = read.csv2("pizza.csv",dec=".",stringsAsFactors = T)
head(pizza)
str(pizza)
attach(pizza)


pizza2 = read.table("pizza.txt",h=T,dec=",",stringsAsFactors = T)
head(pizza2)
tail(pizza2)

#View(pizza)
str(pizza2)
attach(pizza2)

# pizza2$marca=as.factor(pizza2$marca)

pizza$proteina
pizza$gordura

attach(pizza2)
proteina
### classificação
str(pizza2)
library(psych)
describe(pizza2[,c(3,4,5,6,7,8,9)])

################################
tratamento = c("A","B","C")
repeticao = c("r1","r2","r3","r4")
triplicatas = c("t1","t2","t3")

tabela = expand.grid(tratamento,repeticao,triplicatas)
tabela

tabela$ph = rnorm(36,5,2)
tabela

################################
## gráficos
plot(proteina,gordura,
     main=" Gráfico de dispersão entre as Proteína e a Gordura",
     xlab= " valores de proteína",ylab=" valores de gordura",
     col=2:13)
legend("topleft",legend =levels(pizza2$marca))

plot(agua_100~gordura,data=pizza2,col=c(2,3,4,5,6,7,8,9,10,11,12,13)[pizza2$marca])
legend('topright',legend = levels(pizza2$marca),bty='n',col=c(2,3,4,5,6,7,8,9,10,11,12,13))
####################
plot(agua_100,col=4,lwd=5,legend='marca') # uma variável
plot(agua_100,gordura) # duas variáveis
########################
plot(proteina,gordura,type='n')
points(proteina,gordura,col=4,lwd=5)

attach(pizza)

## tapply = tabela
##   var, fator, operacao
tapply(proteina,marca,mean)
tapply(proteina,list(marca,fds_fator),mean)
tapply(proteina,list(marca,fds_fator,borda_fator),mean)

x1=tapply(cinzas,marca,sum)
x2=tapply(cinzas,marca,mean)
par(mfrow=c(1,2))
barplot(x1,col=c('#FFDEAD','red','#4169E1'),horiz=T)
barplot(x2,col=1:10,horiz=T)
par(mfrow=c(1,1))

if(!require(car)){install.packages("car",dep=T)}
library(car)
scatterplot(calorias,agua_100)
scatterplot(agua_100~calorias)
scatterplot(agua_100~calorias|marca)
scatterplot(calorias,agua_100,col = 1,
            pch=15,
            regLine = list(col="green"),
            lwd=3,
            smooth = list(col.smooth="red",
                          col.spread="blue"))

x1 = tapply(carboidratos,list(marca),mean)
pie(x1,angle=45)

library(plotrix)
pie3D(x1,theta=0.4)

pie3D(x1,explode = 0.3)
pie3D(x1,explode=0.2,
      labels = c("A","B","C","D","E","F","G","H","I","J"),
      main='Gráfico de Setores dos valores de Carboidratos x Marca')

###########################

hist(sodio,xlim=c(0,2),probability = T)
den = density(pizza2$sodio)
par(new=T)
plot(den,col="red")
################
################
boxplot(calorias~marca+fds_fator,col=2:3)
Boxplot(calorias~borda_fator+fds_fator,col=2:3)

boxplot(calorias)
boxplot.stats(calorias)
##############
## 

x1 = xtabs(~cinzas,data=pizza)
x1 = round(x1,2)
x1
bp = barplot(x1, xlab = 'Tipo',
             ylab = 'Frequencia',
             col=c('seagreen','yellowgreen'),
             ylim=c(0,14))
text(x=c(bp),y=x1,labels = x1,pos=3)
#################################
x1 =tapply(pizza2$cinzas,pizza2$marca,mean)
x1 = round(x1,2)
x1
cores=c(2,3,4,5,6,7)
bp = barplot(x1, xlab = 'Tipo',
             ylab = 'Frequencia',
             col=cores,
             ylim = c(0,6))
text(x=c(bp),y=x1,labels = x1,pos=3)
legend("topleft", legend=c("A","B","C","D","E","F","G","H","I","J"), fill=cores,
       bty = 'n')
#################################
## Pega os limites das dimensões, em x e y, do último gráfico feito.
lim <- par()$usr[4]

## Refaz o gráfico agora com espaço para o texto a ser adicionado.
barplot(x1, xlab="Tipo de câmbio",
        ylab="Frequência absoluta",
        col=c("seagreen", "yellowgreen"),
        ylim=c(0, lim*1.1))
text(x=c(bp), y=x1, labels=names(x1), pos=3)
box()
#######################################
barplot(x1, xlab="Tipo de câmbio",
        ylab="Frequência absoluta",
        col=c("seagreen", "yellowgreen",2,3,4,5,6,7,8,9),
        ylim=c(0, lim*1.1))
text(x=c(bp), y=x1, labels=x1, pos=3)
legend("topleft", legend=c("A","B","C","D","E","F","G","H","I","J"), fill=cols)
box()
###########################

x1 <- xtabs(~cinzas+gordura, data=pizza2); x1

## Cores de preenchimento para as barras.
cols <- c("seagreen", "yellowgreen")

## Barras lado a lado.
bp <- barplot(t(x1), beside=TRUE, col=cols,
              xlab="Tipo de câmbio", ylab="Frequência absoluta")
bp

## Adiciona uma legenda.
legend("topleft", legend=c("1.6","2.0"), fill=cols)

## Adiciona o texto sobre as barras.
text(x=c(bp), y=t(x), labels=x, pos=3)
########################################
library(doBy)
names(beets)
str(beets)


if (require(ggplot2)){
  qplot(agua_100, gordura, data=pizza2, col=marca) + geom_line() +
    theme(legend.position = "none") + facet_grid(pizza2$borda_fator~pizza2$marca)
} else {
  coplot(gordura ~ agua_100 | marca*borda_fator, data=pizza2)
}
#############################
library(ggplot2)
df = pizza2
df2<- df
df2$regiao<- pizza2$borda_fator
df3<-data.frame(rbind(df,df2))

ggplot(df2,aes(y=agua_100,x=proteina,color = marca))+
  geom_point()+
  facet_wrap(~marca)+
  geom_smooth(method = 'lm',se=F,colour = 'black')+
  theme_minimal()+
  scale_color_grey()+
  theme(text = element_text(family = 'Times New Roman'))

########################################
########################################
library(dplyr)
library(psych)

dados=pizza

View(dados)
glimpse(dados)


###### Tabelas de frequências de variáveis categóricas ######

# Frequências absolutas:

table(dados$borda_fator)

table(dados$fds_fator)

## Tabela cruzada com frequências absolutas:

table(dados$borda_fator, dados$fds_fator)


# Frequências relativas:

prop.table(table(dados$fds))

prop.table(table(dados$Grau_de_Instruçao))

prop.table(table(dados$borda_fator, dados$fds_fator))


############## Medidas para variáveis quantitativas ##############

# Tabela de frequências:

## Variáveis discretas:
table(dados$borda_fator)
prop.table(table(dados$borda_fator))

## Variáveis contínuas:

# Necessário criar categorias que correspondam a faixas de valores:

## Passo 1: analisar a amplitude
range(dados$agua_100)

## Passo 2 (opcional): avaliar a quantidade de categorias adequada (método Sturges)
nclass.Sturges(dados$agua_100)

## Passo 3: criação da tabela com as faixas
table(cut(dados$agua_100, seq(0, 58, l = 11)))


# Função summary - fornece média, mediana, quartis e valores mín e máx

summary(dados$agua_100)

summary(dados$Salario)


# Funções describe e describe.by (pacote 'psych') - média, desvio, erro, mediana

describe(dados$agua_100)

describeBy(dados$agua_100, group = dados$marca)

describeBy(dados$agua_100, group = dados$fds_fator)

# Usando o group_by do pacote dplyr

tabela <- dados %>% group_by(fds_fator,borda_fator) %>% 
  summarise(média = mean(proteina),
            DP = sd(proteina),
            mediana = median(proteina))
tabela


##############################################
######################### Teste de Mann-Whitney #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(dplyr)) install.packages("rstatix") # Instalação do pacote caso não esteja instalado
library(rstatix)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")



# Passo 3: Realização do teste de Mann-Whitney

wilcox.test(agua_100 ~ fds_fator, data = dados)
wilcox.test(proteina ~ fds_fator, data = dados)
wilcox.test(gordura ~fds_fator, data = dados)

# Observação:
# O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: wilcox.test(Nota_Hist ~ Posicao_Sala, data = dados, alternative="greater")
# Nesse caso, o teste verificará se é a mediana do primeiro grupo é maior que a mediana do segundo
# O R está considerando "Frente" como primeiro grupo


# Passo 4: Análise descritiva dos dados

dados %>% group_by(fds_fator) %>% 
  get_summary_stats(agua_100, gordura,proteina, type = "median_iqr")

# Dados paramétricos?
# dados %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")


# Passo 5: Visualização da distribuição
par(mfrow=c(1,2))
hist(dados$agua_100[dados$fds == "0"],
     ylab="Frequência", xlab="Nota", main="Grupo 0")
hist(dados$agua_100[dados$fds == "1"],
     ylab="Frequência", xlab="Nota", main="Grupo 1")


