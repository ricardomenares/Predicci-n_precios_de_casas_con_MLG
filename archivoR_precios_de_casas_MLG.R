library(ggplot2)
library(dplyr)
library(zoo)
library(MASS)
library(bestglm)
library(lmtest)
library(corrplot)
library(olsrr)
library(patchwork) #unir gráficos
options(warn=-1)
options(scipen = 999) 

df<-read.csv("C:/Users/ricar/Desktop/SEMESTRE 8/Modelos lineales generalizados/5. Trabajo/train.csv",header=TRUE,sep=",")
head(df)

df<-subset(df, select = c(LotFrontage, LotArea, OverallQual,OverallCond,YearBuilt,
                          YearRemodAdd, GrLivArea,
                          BsmtFullBath,FullBath,HalfBath,TotRmsAbvGrd,
                          Fireplaces,SalePrice) )
head(df)

df$YearBuilt<-2022-df$YearBuilt ##para convertir a años de antiguedad
df$YearRemodAdd<-2022-df$YearRemodAdd ##lo mismo

df$LotFrontage<-0.3048*df$LotFrontage ## para pasar de medida pies a metros
df$LotArea<-(0.3048**2)*df$LotArea ### pies a metros
df$GrLivArea<-(0.3048**2)*df$GrLivArea ## pies a metros

g1<-ggplot(df, aes(x = OverallQual)) +
  geom_bar(fill="blue") +
  theme_bw()+
  xlab("Calificación material/acabado casa") + ylab("Cantidad")+
  ggtitle("Calificación de material y acabado") + 
  theme(plot.title = element_text(hjust = 0.5)) 
g2<-ggplot(df, aes(x = OverallCond)) +
  geom_bar(fill="blue") +
  theme_bw()+
  xlab("Calificación estado general de la casa") + ylab("Cantidad")+
  ggtitle("Calificación de estado general casa") + 
  theme(plot.title = element_text(hjust = 0.5)) 
g3<-ggplot(df, aes(x = BsmtFullBath)) +
  geom_bar(fill="red") +
  theme_bw()+
  xlab("Baños completos en sótano") + ylab("Cantidad")+
  ggtitle("Baños completos en sótano en casa") + 
  theme(plot.title = element_text(hjust = 0.5)) 
g4<-ggplot(df, aes(x = FullBath)) +
  geom_bar(fill="red") +
  theme_bw()+
  xlab("Baños completos") + ylab("Cantidad")+
  ggtitle("Baños completos en casa") + 
  theme(plot.title = element_text(hjust = 0.5)) 
(g1+g2)/(g3+g4)

g5<-ggplot(df, aes(x = HalfBath)) +
  geom_bar(fill="blue")  +
  theme_bw()+
  xlab("Medios baños") + ylab("Cantidad")+
  ggtitle("Cantidad de medios baños en casa") + 
  theme(plot.title = element_text(hjust = 0.5)) 

g6<-ggplot(df, aes(x = TotRmsAbvGrd)) +
  geom_bar(fill="red")  +
  theme_bw()+
  xlab("Cantidad de habitaciones en la casa") + ylab("Cantidad")+
  ggtitle("Habitaciones en la casa") + 
  theme(plot.title = element_text(hjust = 0.5)) 

g7<-ggplot(df, aes(x = Fireplaces)) +
  geom_bar(fill="brown")  +
  theme_bw()+
  xlab("Chimeneas en la casa") + ylab("Cantidad")+
  ggtitle("Número de chimeneas en la casa") + 
  theme(plot.title = element_text(hjust = 0.5)) 

(g5+g7)/g6

summary(df["OverallQual"])
summary(df["OverallCond"])
summary(df["BsmtFullBath"])
summary(df["FullBath"])
summary(df["HalfBath"])
summary(df["TotRmsAbvGrd"])
summary(df["Fireplaces"])

sqrt(var(df["OverallQual"]))
sqrt(var(df["OverallCond"]))
sqrt(var(df["BsmtFullBath"]))
sqrt(var(df["FullBath"]))
sqrt(var(df["HalfBath"]))
sqrt(var(df["TotRmsAbvGrd"]))
sqrt(var(df["Fireplaces"]))

corsp<-cor(x=df[c(3,4,8,9,10,11,12,13)],method="spearman",use = "complete.obs") #Correlación
corrplot(corsp,method="number")

###################DATOS CONTINUOS#########################
g1<-ggplot(df, aes(x = LotFrontage)) +
  geom_histogram(fill="brown")+
  theme_bw()+
  xlab("Metros lineales de calle conectados a la casa") + ylab("Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5))
g2<-ggplot(df, aes(x = LotFrontage)) +
  geom_boxplot(col="brown")  +
  theme_bw()+
  xlab("Metros lineales de calle conectados a la casa") 
g1+g2

subset(df,df$LotFrontage>40) #analizar valores atípicos

g1<-ggplot(df, aes(x = LotArea)) +
  geom_histogram(fill="purple") +
  theme_bw()+
  xlab("Metros cuadrados de casa") + ylab("Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5))
g2<-ggplot(df, aes(x = LotArea)) +
  geom_boxplot(col="purple")  +
  theme_bw()+
  xlab("Metros cuadrados de casa") 
g1+g2

subset(df,df$LotArea>20000)

g1<-ggplot(df, aes(x = YearBuilt)) +
  geom_histogram(fill="orange") +
  theme_bw()+
  xlab("Años de antigüedad desde construcción") + ylab("Frecuencia") + 
  theme(plot.title = element_text(hjust = 0.5))
g2<-ggplot(df, aes(x = YearBuilt)) +
  geom_boxplot(col="orange")  +
  theme_bw()+
  xlab("Años de antigüedad desde construcción") 
g1+g2

g3<-ggplot(df, aes(x = YearRemodAdd)) +
  geom_histogram(fill="red") +
  theme_bw()+
  xlab("Años de antigüedad desde última remodelación") + ylab("Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5))
g4<-ggplot(df, aes(x = YearRemodAdd)) +
  geom_boxplot(col="red")  +
  theme_bw()+
  xlab("Años de antigüedad desde última remodelación") 
(g1+g2)/(g3+g4)

g1<-ggplot(df, aes(x = GrLivArea)) +
  geom_histogram(fill="black") +
  theme_bw()+
  xlab("Metros cuadrados de superficie habitable") + ylab("Frecuencia")+
  theme(plot.title = element_text(hjust = 0.5))
g2<-ggplot(df, aes(x = GrLivArea)) +
  geom_boxplot(col="black")  +
  theme_bw()+
  xlab("Metros cuadrados de superficie habitable") 
g1+g2

subset(df,df$GrLivArea>1000)

g1<-ggplot(df, aes(x = SalePrice)) +
  geom_histogram(fill="blue") +
  theme_bw()+
  xlab("Precios de casas") + ylab("Frecuencia")+
  ggtitle("Histograma y Box-Plot de precios de casas de Boston") + 
  theme(plot.title = element_text(hjust = 0.5))
g2<-ggplot(df, aes(x = SalePrice)) +
  geom_boxplot(col="blue") +
  theme_bw()+
  xlab("Precios de casas") 
g1+g2

CV1<-sd(df["SalePrice"][df["SalePrice"]<150000]) /  mean(df["SalePrice"][df["SalePrice"]<150000])
CV2<-sd(df["SalePrice"][df["SalePrice"]<300000 & df["SalePrice"]>150000]) /  mean(df["SalePrice"][df["SalePrice"]<300000 & df["SalePrice"]>150000])
CV3<-sd(df["SalePrice"][df["SalePrice"]>300000]) /  mean(df["SalePrice"][df["SalePrice"]>300000])
cat("Los coef. de variación son: ",CV1,CV2,CV3)

g1<-ggplot(df, aes(x = SalePrice)) +
  geom_histogram(fill="white",col="blue") +
  theme_bw()+
  xlab("Precios de casas") + ylab("Frecuencia")+
  ggtitle("Histograma con cortes para C.V. de precios de casas de Boston") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 150000,col="red")+
  geom_vline(xintercept = 300000,col="red")+
  annotate("rect", xmin = 0, xmax = 150000, ymin = 0, ymax = 350,
           alpha = .1,fill = "red")+
  annotate("rect", xmin = 150000, xmax = 300000, ymin = 0, ymax = 350,
           alpha = .1,fill = "green")+
  annotate("rect", xmin = 300000, xmax = 800000, ymin = 0, ymax = 350,
           alpha = .1,fill = "orange")+
  annotate("text", x = 50000, y = 300, label = "CV=0.1913")+
  annotate("text", x = 220000, y = 300, label = "CV=0.1886")+
  annotate("text", x = 400000, y = 300, label = "CV=0.1804")
g1

sd(df$LotFrontage,na.rm=TRUE)
sqrt(var(df["LotArea"]))
sqrt(var(df["YearBuilt"]))
sqrt(var(df["YearRemodAdd"]))
sqrt(var(df["GrLivArea"]))
sqrt(var(df["SalePrice"]))

summary(df["LotFrontage"])
summary(df["LotArea"])
summary(df["YearBuilt"])
summary(df["YearRemodAdd"])
summary(df["GrLivArea"])
summary(df["SalePrice"])

cor<-cor(x=df[c(1,2,5,6,7,13)],method="pearson",use = "complete.obs") #Correlación
corrplot(cor,method="number")

###########################procesamiento de modelos##############
bestglm(df,family = Gamma(link="identity"),IC = "AIC",method = "exhaustive") #modelo identidad

mod1<-glm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+GrLivArea+BsmtFullBath+
            TotRmsAbvGrd+Fireplaces
          ,family=Gamma(link = "identity"),data=df)
summary(mod1)

bestglm(df,family = Gamma(link="log"),IC = "AIC",method = "exhaustive") #modelo logaritmico

mod2<-glm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+GrLivArea+
            BsmtFullBath+Fireplaces
          ,family=Gamma(link = "log"),data=df)
summary(mod2) 

bestglm(df,family = Gamma(link="inverse"),IC = "AIC",method = "exhaustive") #modelo logaritmico

mod3<-glm(SalePrice~LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+YearRemodAdd+
            BsmtFullBath+Fireplaces
          ,family=Gamma(link = "inverse"),data=df,start=c(1,1,1,1,1,1,1,1,1), control = glm.control(maxit = 100))
summary(mod3)


###################Análisis residual#############################
par(mfrow=c(2,2))
plot(resid(mod1,type="response"), col='gray',main="Residuos de modelo Gamma con enlace identidad",xlab="Indice",ylab="Residuos")
abline(0,0,col="red")
plot(mod2$residuals, col='gray',main="Residuos de modelo Gamma con enlace logarítmico",xlab="Indice",ylab="Residuos")
abline(0,0,col="red")
plot(mod3$residuals, col='gray',main="Residuos de modelo Gamma con enlace inverso",xlab="Indice",ylab="Residuos")
abline(0,0,col="red")


par(mfrow=c(2,2))
plot(resid(mod1,type="pearson"),col="gray",main="Residuos de Pearson de modelo Gamma con enlace identidad",
     ,xlab="Indice",ylab="Residuos")
abline(0,0,col="red")
plot(resid(mod2,type="pearson"),col="gray",main="Residuos de Pearson de modelo Gamma con enlace logaritmico",
     ,xlab="Indice",ylab="Residuos")
abline(0,0,col="red")
plot(resid(mod3,type="pearson"),col="gray",main="Residuos de Pearson de modelo Gamma con enlace inverso",
     ,xlab="Indice",ylab="Residuos")
abline(0,0,col="red")

#### test Durbin Watson####
library(lmtest)
dwtest(mod1, alternative = "two.sided")
dwtest(mod2, alternative = "two.sided")
dwtest(mod3, alternative = "two.sided")




#######################Gráficos#########################
par(mfrow=c(2,2))
plot(density(df$SalePrice),main="Precio de casas con modelo Gamma(Identidad)",xlab="Precio de casa",ylab="Densidad")
lines(density(predict(mod1, type='response')), col='red')
legend(400000, 0.000004,legend=c ("Gamma(Identidad)","Precio de casas") , col=c("red","black")
       ,lty =1:1 ,cex =0.8 ,title=expression(paste(bold("Curva"))))

plot(density(df$SalePrice),main="Precio de casas con modelo Gamma(Logarítmico)",xlab="Precio de casa",ylab="Densidad")
lines(density(predict(mod2, type='response')), col='red')
legend(380000, 0.000004,legend=c ("Gamma(Logarítmico)","Precio de casas") , col=c("red","black")
       ,lty =1:1 ,cex =0.8 ,title=expression(paste(bold("Curva"))))

plot(density(df$SalePrice),main="Precio de casas con modelo Gamma(Inverso)",xlab="Precio de casa",ylab="Densidad")
lines(density(predict(mod3, type='response')), col='red')
legend(400000, 0.000004,legend=c ("Gamma(Inverso)","Precio de casas") , col=c("red","black")
       ,lty =1:1 ,cex =0.8 ,title=expression(paste(bold("Curva"))))