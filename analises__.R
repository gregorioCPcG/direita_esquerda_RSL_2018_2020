library(readxl)
df <- read_excel("df_.xlsx")
summary(df)
#verificações iniciais
cor(df$Haddad_turno2, df$esquerda_nHaddad)
cor(df$Haddad_turno1, df$Haddad_turno2)
cor(df$aumento_Haddad, df$esquerda_nHaddad)
plot(df$aumento_Haddad, df$esquerda_nHaddad)#boa pista
cor(df$aumento_Bolsonaro, df$esquerda_nHaddad)
df$log_eleitorado <- log(df$eleitorado)
cor(df$eleitorado, df$aumento_Bolsonaro)
cor(df$eleitorado, df$aumento_Haddad)
cor(df$log_eleitorado, df$aumento_Haddad)
plot(df$log_eleitorado, df$aumento_Haddad)# boa pista
plot(df$eleitorado, df$aumento_Haddad)

modelaumentoHad <- lm(aumento_Haddad ~ esquerda_nHaddad + log_eleitorado, data=df)
summary(modelaumentoHad)#bom modelo, boa pista

library(tidyverse)
df$nivel_apoio_turno1 <- ntile(df$Haddad_turno1, 2)
df$nivel_apoio_turno1 <- as.factor(df$nivel_apoio_turno1)
levels(df$nivel_apoio_turno1) <- c('menor','maior')
summary(df$nivel_apoio_turno1)
modeloAumentoHad_control <- lm(aumento_Haddad ~ nivel_apoio_turno1 +
                                 esquerda_nHaddad + log_eleitorado, data=df)
summary(modeloAumentoHad_control)# Show

df$outrosEsquerdaMenosCiro <- df$Boulos + df$MarinaSilva + df$Vera_PSTU + df$J.Goulart_PPL

modelofullAumentoHad <- lm(aumento_Haddad ~ nivel_apoio_turno1 + Ciro +
                             outrosEsquerdaMenosCiro + log_eleitorado, data=df)
summary(modelofullAumentoHad)

df$OutrosEsquerda <- df$esquerda_nHaddad
Show <- lm(aumento_Haddad ~ nivel_apoio_turno1 +
                                 OutrosEsquerda + log_eleitorado, data=df)
summary(Show)# Show
summary(df$aumento_Haddad)

# o mesmo modelo Show para votação no segundo turno
Show2 <- lm(Haddad_turno2 ~ nivel_apoio_turno1 +
             OutrosEsquerda + log_eleitorado, data=df)
summary(Show2)# Show2
summary(df$Haddad_turno2)

# analise dos interceptos
summary(Show)# Show
#Show : se o voto em outros de esquerda for zero e população baixa Haddad teria 0,85% de votos A MENOS no segundo turno
summary(Show2)# Show2
#Show2: Se o nível de apoio fosse baixo no primeiro turno, e o voto nos outros de esquerda fossem zero, Haddad receberia no segundo turno 8,48 %


cor(df$Haddad_turno2, df$ESQUERDA)
cor(df$aumento_Haddad, df$ESQUERDA)
cor(df$aumento_Haddad, df$OutrosEsquerda)

turno2 <- ggplot(df, aes(nivel_apoio_turno1, Haddad_turno2 ))
turno2 + geom_boxplot()
aumento <- ggplot(df, aes(nivel_apoio_turno1, aumento_Haddad))
aumento + geom_boxplot()

cont  <- df$Bolsonaro_turno1 + df$ESQUERDA
df$outrosDireita <- 100-cont
summary(cont)
summary(df$outrosDireita)

cor(df$aumento_Bolsonaro, df$outrosDireita)
cor(df$aumento_Haddad, df$outrosDireita)
cor(df$aumento_Haddad, df$outrosEsquerdaMenosCiro)

# modelões
modelaoBozo <- lm(Bolsonaro_turno2 ~ Bolsonaro_turno1 + outrosDireita + log_eleitorado, data=df)
summary(modelaoBozo)

modelaoHaddad <- lm(Haddad_turno2 ~ Haddad_turno1 + OutrosEsquerda + log_eleitorado, data=df)
summary(modelaoHaddad)


# Jean de Liz correlações


cor(df$Jean2020, df$ESQUERDA)
plot(df$Jean2020, df$ESQUERDA)
modeloJean <- lm(Jean2020 ~ ESQUERDA, data=df)
summary(modeloJean)


a<- ggplot(df, aes(ESQUERDA, Jean2020))
a + geom_text(label=df$bairro) + ylab("Jean de Liz 2020") +
  xlab("Voto à esquerda* primeiro turno 2018 presidente") +
  labs(caption=('*Haddad (PT), Ciro(PDT), Marina(REDE), Vera(PSTU), Boulos(PSOL) e Goulart(PPL)')) +
  geom_smooth(se=FALSE)

# Pasqualini + C.Tonet + C.Hoffman
df$DIREITA <- df$outrosDireita + df$Bolsonaro_turno1
df$NovaDireita2020 <- df$Pasqualini + df$C.Hoffman + df$C.Tonet
cor(df$NovaDireita2020, df$DIREITA)
df$DIREITA2020 <- df$NovaDireita2020 + df$J.Thome
cor(df$DIREITA2020, df$DIREITA)
modeloDireita <- lm(DIREITA2020 ~ DIREITA, data=df)
summary(modeloDireita)
b <- ggplot(df, aes(DIREITA, DIREITA2020))
b + geom_text(label=df$bairro) + ylab("Thomé, Pasqualini,Tonet e Hoffman 2020") +
  xlab("Voto à direita* primeiro turno 2018 presidente") +
  labs(caption=('*Bolsonaro (PSL), Amoedo(Novo), Alckmin(PSDB), A.Dias(Podemos),
                Eymael(PDC), Daciolo(Patriotas) e Meirelles(MDB)')) +
  geom_smooth(se=FALSE)


df$ESQUERDA2020 <- df$Jean2020
df$ESQUERDA2018 <- df$ESQUERDA
df$DIREITA2018 <- df$DIREITA
library(corrplot)
df2 <- subset(df, select=c(DIREITA2018, DIREITA2020, ESQUERDA2018,ESQUERDA2020))
matriz <- cor(df2[1:4], method = "pearson")
corrplot(matriz)
corrplot(matriz, method="square", 
         type="lower", order="hclust",
         diag=FALSE)
library(GGally)
ggpairs(df2, title="Comparando Correlações")


ab <- lm(Jean2020 ~ ESQUERDA2018 + log_eleitorado, data=df)
ac <- lm(DIREITA2020 ~ DIREITA2018 + log_eleitorado, data=df)
library(sjPlot)
tab_model(ab, ac)
summary(df2)
df2 <- subset(df, select=c(DIREITA2018,ESQUERDA2018, DIREITA2020, ESQUERDA2020))
boxplot(df2)
boxplot(df2,
        main="",
        xlab="",
        ylab="",
        col="orange","pink","orange","pink",
        border="brown", horizontal = FALSE, notch=FALSE,
        varwidth = FALSE, outwex=TRUE
)

boxplot(df2,
        main="",
        xlab="",
        ylab="",
        col="orange","pink","orange","pink",
        border="brown", horizontal = TRUE, notch=FALSE,
        varwidth = FALSE, outwex=TRUE
)
