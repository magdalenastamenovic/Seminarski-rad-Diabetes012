#Install####
install.packages("caret", dependencies = TRUE)
install.packages("paletteer")
install.packages("rpart.plot")
#Includes####
library(ggplot2)
library(paletteer)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(randomForest)
library(boot)
library(rpart.plot)
colorS = c("#88A0DCFF", "#381A61FF", "#7C4B73FF", "#ED968CFF", "#AB3329FF", "#E78429FF", "#F9D14AFF","#73652DFF")
#Functons####
cramer_v <- function(var1, var2) {
  tabela <- table(var1, var2)
  chi2 <- chisq.test(tabela)$statistic
  n <- sum(tabela)
  phi2 <- chi2 / n
  r <- nrow(tabela)
  k <- ncol(tabela)
  v <- sqrt(phi2 / min(r - 1, k - 1))
  return(as.numeric(v))
}
tukey_fun <- function(numericka_var, grupna_var) {
  model <- aov(numericka_var ~ as.factor(grupna_var))
  rezultat <- TukeyHSD(model)
  return(rezultat)
}
chi_sq_test <- function(var1, var2) {
  tabela <- table(var1, var2)
  rezultat <- chisq.test(tabela)
  return(rezultat)
}
anova_test <- function(numericka_var, grupna_var) {
  model <- aov(numericka_var ~ as.factor(grupna_var))
  return(summary(model))
}
pearson_funkcija <- function(x, y) {
  if(length(x) != length(y)) stop("Vektori moraju biti iste dužine")
  mean_x <- mean(x)
  mean_y <- mean(y)
  brojilac <- sum((x - mean_x) * (y - mean_y))
  imenilac <- sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
  return(brojilac / imenilac)
}
izvuci_sve_metrike <- function(pred, stvarni, ime_modela) {
  
  cm <- confusionMatrix(pred, stvarni, mode = "everything")
  
  
  izvestaj <- as.data.frame(cm$byClass[, c("Precision", "Recall", "F1")])
  izvestaj$Model <- ime_modela
  izvestaj$Klasa <- nivoi
  
  return(izvestaj)
}

#Dataset import#####
radni_direktorijum <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(radni_direktorijum)
cat("Radni direktorijum je postavljen na:", getwd(), "\n")

data <- read.csv("diabetes_dataset.csv")
cat("Skup podataka mozete pogledati u promenljivoj data koja se automatski otvorila u radnom okruzenju", View(data))
#Osnovne informacije o skupu####
dimenzije_skupa = dim(data)
cat("Broj featura skupa: ", dimenzije_skupa[2], "\n", "Broj opservacija: ", dimenzije_skupa[1])

str(data)
summary(data)

#Pripreme za EDA####

daNe_kategorije = c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", 
                 "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
                 "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "DiffWalk")
ordinalne_kategorije = c("GenHlth", "Education", "Income")
data[daNe_kategorije] = lapply(data[daNe_kategorije], factor, levels = c(0,1), labels = c("Ne","Da"))
data$Sex = factor(data$Sex, levels = c(0,1), labels = c("zensko","musko"))

nivoi_Diabetes_012 = sort(unique(data$Diabetes_012))
data$Diabetes_012 = factor(data$Diabetes_012, 
                            levels = nivoi_Diabetes_012, 
                            labels = c("nema dijabetes", "predijabetes", "dijabetes"))

nivoi_GenHlth = sort(unique(data$GenHlth), decreasing = TRUE)
data$GenHlth = factor(
  data$GenHlth,
  levels = nivoi_GenHlth,
  labels = c("Odlično", "Vrlo dobro", "Dobro", "Zadovoljavajuće", "Loše"),
  ordered = TRUE
)

nivoi_Income = sort(unique(data$Income))
data$Income = factor(data$Income, levels = nivoi_Income,
                            ordered = TRUE,
                            labels = c("<10.000$",
                                       "10.000-14.999$",
                                       "15.000-19.999$",
                                       "20.000-24.999$",
                                       "25.000-34.999$",
                                       "35.000-49.999$",
                                       "50.000-74.999$",
                                       ">=75.000$"))

nivoi_Education = sort(unique(data$Education))
data$Education<- factor(data$Education, levels = nivoi_Education,
                     ordered = TRUE,
                     labels = c("bez skole/isao u vrtic",
                                "osnovna skola",
                                "3-god srednja",
                                "4-god srednja",
                                "3-god fakultet",
                                "4-god fakultet"))
str(data)

##Univarijantna####

ggplot(data, aes(x = Diabetes_012, fill = Diabetes_012)) +
      geom_bar() +
      labs(title = "Distribucija kategorija Diabetes_012", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

ggplot(data, aes(x = HighBP , fill = HighBP )) +
      geom_bar() +
      labs(title = "Distribucija kategorija HighBP", subtitle = "Da li ispitanik ima visok pritisak (hipertenziju)?", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_highBP = var(as.numeric(data$HighBP))
cat("Varijansa HighBP: ", varijansa_highBP, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = HighChol , fill = HighChol )) +
      geom_bar() +
      labs(title = "Distribucija kategorija HighChol", subtitle = "Da li ispitanik ima visok holesterol?", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_highChol = var(as.numeric(data$HighChol))
cat("Varijansa HighChol: ", varijansa_highChol, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(y = BMI)) +
  geom_boxplot(fill= "#88A0DCFF") +
  labs(title = "Boxplot distribucije BMI", y = "BMI") +
  scale_fill_paletteer_d("MetBrewer::Archambault")

ggplot(data, aes(x = BMI)) +
      geom_histogram(bins = 50, fill = "#7C4B73FF", color="#381A61FF") +
      labs(title = "Distribucija BMI", x = "BMI", y = "Broj opservacija")

ggplot(data, aes(y = BMI)) +
  geom_boxplot(
    fill = "#88A0DCFF",
    outlier.color = "#381A61FF") +
  labs(
    title = "Prikaz autlajera BMI karakteristike",
    y = "BMI"
  )

donja_granica = quantile(data$BMI, 0.25) - 1.5 * IQR(data$BMI)
gornja_granica = quantile(data$BMI, 0.75) + 1.5 * IQR(data$BMI)

autlajeri_statisticki = data %>% filter(BMI < donja_granica | BMI > gornja_granica)
cat("Po IQR metodu statističkih autlajera ima " , nrow(autlajeri_statisticki), ".Što je", nrow(autlajeri_statisticki)/nrow(data)*100,"% ukupnog broja opservacija\n")

autlajeri_domenski = data %>% filter(BMI < 12 | BMI > 70)
cat("Po domenskom znanju autlajera ima " , nrow(autlajeri_domenski), ".Što je", nrow(autlajeri_domenski)/nrow(data)*100,"% ukupnog broja opservacija\n")

ggplot(data, aes(x = CholCheck , fill = CholCheck )) +
  geom_bar() +
  labs(title = "Distribucija kategorija CholCheck", subtitle = "Da li je ispitanik proverio holesterol poslednjih 5 godina?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_CholCheck = var(as.numeric(data$CholCheck))
cat("Varijansa CholCheck: ", varijansa_CholCheck, "od maksimmalne moguce 0.25\n")


ggplot(data, aes(x = HeartDiseaseorAttack  , fill = HeartDiseaseorAttack  )) +
  geom_bar() +
  labs(title = "Distribucija kategorija HeartDiseaseorAttack ", subtitle = "Da li je ispitanik imao srčani udar ili ima bolest srca?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_HeartDiseaseorAttack  = var(as.numeric(data$HeartDiseaseorAttack ))
cat("Varijansa HeartDiseaseorAttack : ", varijansa_HeartDiseaseorAttack , "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = Stroke  , fill = Stroke  )) +
  geom_bar() +
  labs(title = "Distribucija kategorija Stroke ", subtitle = "Da li je ispitanik imao moždani udar?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_Stroke  = var(as.numeric(data$Stroke ))
cat("Varijansa Stroke : ", varijansa_Stroke , "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = GenHlth , fill = GenHlth )) +
  geom_bar() +
  labs(title = "Distribucija kategorija GenHlth", subtitle = "Kako ispitanik ocenjuje svoje opste zdravlje?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

data %>% group_by(GenHlth) %>%
summarise(
          Broj_opservacija = n(),
          Procenat_opservacija = round(n() / nrow(data) * 100, 2)
)

ggplot(data, aes(x = MentHlth)) +
      geom_histogram(bins = 50, fill = "#7C4B73FF", color="#381A61FF") +
      labs(title = "Distribucija MentHlth", x = "MentHlth", y = "Broj opservacija",
      subtitle = "Koliko dana u poslednjih mesec dana je ispitanik imao stres?")

ggplot(data, aes(x = PhysHlth)) +
      geom_histogram(bins = 50, fill = "#7C4B73FF", color="#381A61FF") +
      labs(title = "Distribucija PhysHlth", x = "PhysHlth", y = "Broj opservacija",
      subtitle = "Koliko dana u poslednjih mesec dana je ispitanik imao fizicke poteskoce?")

ggplot(data, aes(x = DiffWalk  , fill = DiffWalk  )) +
  geom_bar() +
  labs(title = "Distribucija kategorija DiffWalk ", subtitle = "Da li je ispitanik ima poteškoće u kretanju ili penjanju uz stepenice?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_DiffWalk  = var(as.numeric(data$DiffWalk ))
cat("Varijansa DiffWalk : ", varijansa_DiffWalk , "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = Fruits , fill = Fruits )) +
      geom_bar() +
      labs(title = "Distribucija kategorija Fruits", subtitle = "Da li ispitanik jede jednu porciju ili više voća u toku dana?", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_fruits = var(as.numeric(data$Fruits))
cat("Varijansa Fruits: ", varijansa_fruits, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = Veggies , fill = Veggies )) +
      geom_bar() +
      labs(title = "Distribucija kategorija Veggies", subtitle = "Da li ispitanik jede jednu porciju ili više povrća u toku dana?", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_veggies = var(as.numeric(data$Veggies))
cat("Varijansa Veggies: ", varijansa_veggies, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = Smoker , fill = Smoker )) +
      geom_bar() +
      labs(title = "Distribucija kategorija Smoker", subtitle = "Da li је ispitanik konzumirao minimalno 100 cigareta u toku zivota?", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_smoker = var(as.numeric(data$Smoker))
cat("Varijansa Smoker: ", varijansa_smoker, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = HvyAlcoholConsump , fill = HvyAlcoholConsump )) +
      geom_bar() +
      labs(title = "Distribucija kategorija HvyAlcoholConsump", subtitle = "Da li је ispitanik konzumirao 7 ili više pića u toku nedelje?", x = "Kategorije", y = "Broj opservacija") +
      scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_hvyAlcoholConsump = var(as.numeric(data$HvyAlcoholConsump))
cat("Varijansa HvyAlcoholConsump: ", varijansa_hvyAlcoholConsump, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = PhysActivity  , fill = PhysActivity  )) +
  geom_bar() +
  labs(title = "Distribucija kategorija PhysActivity ", subtitle = "Da li je ispitanik imao rekreativnu fizičku aktivnost poslednjih 30 dana?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_PhysActivity  = var(as.numeric(data$PhysActivity ))
cat("Varijansa PhysActivity : ", varijansa_PhysActivity , "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = AnyHealthcare , fill = AnyHealthcare )) +
  geom_bar() +
  labs(title = "Distribucija kategorija AnyHealthcare", subtitle = "Da li ispitanik ima neki vid zdravstvene zastite?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_аnyHealthcare = var(as.numeric(data$AnyHealthcare))
cat("Varijansa AnyHealthcare: ", varijansa_аnyHealthcare, "od maksimmalne moguce 0.25\n")

ggplot(data, aes(x = NoDocbcCost , fill = NoDocbcCost )) +
  geom_bar() +
  labs(title = "Distribucija kategorija NoDocbcCost", subtitle = "Da li ispitanik imao potrebu za doktorom, a nije moga to da priušti?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_noDocbcCost = var(as.numeric(data$NoDocbcCost))
cat("Varijansa NoDocbcCost: ", varijansa_noDocbcCost, "od maksimmalne moguce 0.25\n")


ggplot(data, aes(x = Income, fill = Income)) +
  geom_bar() +
  labs(
    title = "Distribucija kategorija Income",
    y = "Broj opservacija",
  ) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = colorS)+
  theme(legend.position = "bottom",
        axis.text.x  = element_blank())

prop.table(table(data$Income))*100

ggplot(data, aes(x = Sex , fill = Sex )) +
  geom_bar() +
  labs(title = "Distribucija kategorija Sex", subtitle = "Kol pola je ispitanik?", x = "Kategorije", y = "Broj opservacija") +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

varijansa_sex = var(as.numeric(data$Sex))
cat("Varijansa Sex: ", varijansa_sex, "od maksimmalne moguce 0.25\n")

prop.table(table(data$Age))*100
ggplot(data, aes(x = Age )) +
  geom_histogram(bins = 50, fill = "#7C4B73FF", color="#381A61FF") +
  labs(title = "Distribucija Age ", x = "Age ", y = "Broj opservacija",
       subtitle = "Starost ispitanika")

ggplot(data, aes(x = Education, fill = Education)) +
  geom_bar() +
  labs(
    title = "Distribucija kategorija Education",
    y = "Broj opservacija",
  ) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = colorS)+
  theme(legend.position = "bottom",
        axis.text.x  = element_blank())


##Bivarijanta####
ggplot(data, aes(x = Diabetes_012, fill = HighBP))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija HighBP po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Povišen krvni pritisak"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data %>% count(Diabetes_012, HighBP), 
       aes(x = Diabetes_012, y = HighBP, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija HighBP po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "HighBP",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data$HighBP, data$Diabetes_012)
cramer_v(data$HighBP, data$Diabetes_012)


ggplot(data, aes(x = Diabetes_012, fill = HighChol))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija HighChol po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Povišen holesterol"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position="bottom")

ggplot(data %>% count(Diabetes_012, HighChol), 
       aes(x = Diabetes_012, y = HighChol, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija HighChol po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "HighChol",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data$HighChol, data$Diabetes_012)
cramer_v(data$HighChol, data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = CholCheck))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija CholCheck po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Proveren holesterol poslednjih 5 godina"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data %>% count(Diabetes_012, CholCheck), 
       aes(x = Diabetes_012, y = CholCheck, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija CholCheck po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "CholCheck",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data$CholCheck, data$Diabetes_012)
cramer_v(data$CholCheck, data$Diabetes_012)


vrenostiBoxPlot <- data%>%
  group_by(Diabetes_012)%>%
  summarise(
    Q1 = quantile(BMI, 0.25),
    median = median(BMI),
    Q3 = quantile(BMI, 0.75),
  )%>%
  pivot_longer(
    cols = c(Q1, median, Q3)
  )

ggplot(data, aes(x = Diabetes_012, y = BMI, fill = Diabetes_012))+
  geom_boxplot()+
  geom_point(
    data = vrenostiBoxPlot,
    aes(x = Diabetes_012, y = value),
    shape = 22,
    size = 5,
    fill = "white",
    color = "#E78429FF"
  )+
  geom_text(
    data = vrenostiBoxPlot,
    aes(x = Diabetes_012, y = value, label = round(value, 1)),
    size = 3
  )+
  labs(
    title = "Raspodela BMI po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "BMI"
  )+
  scale_fill_paletteer_d("MetBrewer::Archambault")+
  theme(legend.position = "none")

BMI_bezdomeskihAutlajera = data %>%
  filter(BMI >= 12 & BMI <= 70)

BMI_bezStatistickihAutlajera = data %>%
  filter(BMI >= donja_granica & BMI <= gornja_granica)

BMI_bezdomenskih_sabrani = bind_rows(
  data %>% 
    select(BMI, Diabetes_012) %>% 
    mutate(Skup = "originalni skup"),
  
  BMI_bezdomeskihAutlajera %>% 
    select(BMI, Diabetes_012) %>% 
    mutate(Skup = "bez domenskih autlajera"),
)
statistickeVrednostiBezDomenskih = BMI_bezdomenskih_sabrani %>%
  group_by(Skup, Diabetes_012) %>%
  summarise(
    Q1 = quantile(BMI, 0.25),
    Median = quantile(BMI, 0.5),
    Q3 = quantile(BMI, 0.75)
  )

ggplot(BMI_bezdomenskih_sabrani, aes(x = Diabetes_012, y = BMI, fill = Diabetes_012)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Skup)+
  geom_text(
    data = statistickeVrednostiBezDomenskih,
    aes(x = Diabetes_012, y = Q1, label = round(Q1, 1)),
    vjust = 1.5, color = "blue", size = 3,
  ) +
  geom_text(
    data = statistickeVrednostiBezDomenskih,
    aes(x = Diabetes_012, y = Median, label = round(Median, 1)),
    vjust = -0.5, color = "red", size = 3
  ) +
  geom_text(
    data = statistickeVrednostiBezDomenskih,
    aes(x = Diabetes_012, y = Q3, label = round(Q3, 1)),
    vjust = -1.5, color = "darkgreen", size = 3
  ) +
  labs(
    title = "Uporedjivanje uticaja domenskih autlejera karakteristike BMI na Diabetes_012",
    y = "BMI",
    x = "Dijabetes"
  )+
  theme(legend.position = "none")+
  scale_fill_paletteer_d("MetBrewer::Archambault")


BMI_bezstatistickih_sabrani <- bind_rows(
  data %>% 
    select(BMI, Diabetes_012) %>% 
    mutate(Skup = "originalni skup"),
  
  BMI_bezStatistickihAutlajera %>% 
    select(BMI, Diabetes_012) %>% 
    mutate(Skup = "bez statistickih autlajera"),
)
statistickeVrednostiBezStatistickih <- BMI_bezstatistickih_sabrani %>%
  group_by(Skup, Diabetes_012) %>%
  summarise(
    Q1 = quantile(BMI, 0.25),
    Median = quantile(BMI, 0.5),
    Q3 = quantile(BMI, 0.75)
  )

ggplot(BMI_bezstatistickih_sabrani, aes(x = Diabetes_012, y = BMI, fill = Diabetes_012)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Skup)+
  geom_text(
    data = statistickeVrednostiBezStatistickih,
    aes(x = Diabetes_012, y = Q1, label = round(Q1, 1)),
    vjust = 1.5, color = "blue", size = 3,
  ) +
  geom_text(
    data = statistickeVrednostiBezStatistickih,
    aes(x = Diabetes_012, y = Median, label = round(Median, 1)),
    vjust = -0.5, color = "red", size = 3
  ) +
  geom_text(
    data = statistickeVrednostiBezStatistickih,
    aes(x = Diabetes_012, y = Q3, label = round(Q3, 1)),
    vjust = -1.5, color = "darkgreen", size = 3
  ) +
  labs(
    title = "Uporedjivanje uticaja statistickih autlejera karakteristike BMI na Diabetes_012",
    y = "BMI",
    x = "Dijabetes"
  )+
  theme(legend.position = "none")+
  scale_fill_paletteer_d("MetBrewer::Archambault")

ggplot(data, aes(x = BMI, fill = Diabetes_012)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Gustina raspodele BMI po kategorijama Diabetes_012",
    x = "BMI",
    y = "Gustina",
    fill = "Diabetes_012"
  ) +
  scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_vline(xintercept = c(25, 30), linetype = "dashed", color = "black", size = 1)

anova_test(data$BMI, data$Diabetes_012)
tukey_fun(data$BMI, data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = Smoker))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija Smoker po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Pušač"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position = "bottom")

ggplot(data %>% count(Diabetes_012, Smoker), 
       aes(x = Diabetes_012, y = Smoker, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija Smoker po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "Smoker",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")
  

chi_sq_test(data$Smoker, data$Diabetes_012)
cramer_v(data$Smoker, data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = Stroke))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija Stroke po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Moždani udar"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position = "bottom")

ggplot(data %>% count(Diabetes_012, Stroke), 
       aes(x = Diabetes_012, y = Stroke, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija Stroke po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "Stroke",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data$Stroke, data$Diabetes_012)
cramer_v(data$Stroke, data$Diabetes_012)


ggplot(data, aes(x = Diabetes_012, fill = HeartDiseaseorAttack))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija HeartDiseaseorAttack po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Bolest srca/Srčani udar"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position = "bottom")

ggplot(data %>% count(Diabetes_012, HeartDiseaseorAttack), 
       aes(x = Diabetes_012, y = HeartDiseaseorAttack, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija HeartDiseaseorAttack po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "HeartDiseaseorAttack",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data$HeartDiseaseorAttack , data$Diabetes_012)
cramer_v(data$HeartDiseaseorAttack , data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = PhysActivity))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija PhysActivity po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Fizička aktivnost"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position = "bottom")

ggplot(data %>% count(Diabetes_012, PhysActivity), 
       aes(x = Diabetes_012, y = PhysActivity, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija PhysActivity po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "PhysActivity",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data$PhysActivity , data$Diabetes_012)
cramer_v(data$PhysActivity , data$Diabetes_012)


ggplot(data, aes(x = Diabetes_012, fill = Fruits))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija Fruits po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Konzumacija voća"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position = "bottom")

ggplot(data %>% count(Diabetes_012, Fruits), 
       aes(x = Diabetes_012, y = Fruits, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija Fruits po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "Fruits",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")
chi_sq_test(data$Fruits , data$Diabetes_012)
cramer_v(data$Fruits , data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = Veggies))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija Veggies po kategorijama Diabetes_012",
    y = "Broj opservacija",
    fill = "Konzumacija povrća"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")+
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  )+
  theme(legend.position = "bottom")

ggplot(data %>% count(Diabetes_012, Veggies), 
       aes(x = Diabetes_012, y = Veggies, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija Veggies po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "Veggies",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")
chi_sq_test(data$Veggies, data$Diabetes_012)
cramer_v(data$Veggies, data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = HvyAlcoholConsump)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS)+
  labs(title = "Distribucija HvyAlcoholConsump u odnosu na Diabetes_012",
       x = NULL,
       y = "Broj ispitanika",
       fill ="Konzumira dosta alkohola")
ggplot(data %>% count(Diabetes_012, HvyAlcoholConsump), 
       aes(x = Diabetes_012, y = HvyAlcoholConsump, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija konzumacije alkohola po tipovima dijabetesa",
    x = "Diabetes_012",
    y = "HvyAlcoholConsump",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$HvyAlcoholConsump, data$Diabetes_012)
cramer_v(data$HvyAlcoholConsump, data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill = AnyHealthcare)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija klasa AnyHealthcare u odnosu na klase Diabetes_012",
       x = NULL,
       y = "Broj opservacija",
       fill = "Ima z. negu")
ggplot(data %>% count(Diabetes_012, AnyHealthcare), 
       aes(x = Diabetes_012, y = AnyHealthcare, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija klasa AnyHealthcare po tipovima Diabetes_012",
    x = "Diabetes_012",
    y = "AnyHealthcare",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$AnyHealthcare, data$Diabetes_012)
cramer_v(data$AnyHealthcare, data$Diabetes_012)
ggplot(data, aes(x = Diabetes_012, fill = NoDocbcCost)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija klasa NoDocbcCost u odnosu na tipove Diabetes_012",
       x = NULL,
       y = "Broj opservacija",
       fill = "Imao mogućnost")
ggplot(data %>% count(Diabetes_012, NoDocbcCost), 
       aes(x = Diabetes_012, y = NoDocbcCost, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija klasa NoDocbcCost u odnosu na tipove Diabetes_012",
    x = "Diabetes_012",
    y = "NoDocbcCost",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$NoDocbcCost, data$Diabetes_012)
cramer_v(data$NoDocbcCost, data$Diabetes_012)
ggplot(data, aes(x = Diabetes_012, fill = GenHlth)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija GenHlth u odnosu na tipove Diabetes_012",
       x = NULL,
       y = "Broj opservacija",
       fill = "Ocena")
ggplot(data %>% count(Diabetes_012, GenHlth), 
       aes(x = Diabetes_012, y = GenHlth, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija klasa GenHlth u odnosu na tipove Diabetes_012",
    x = "Diabetes_012",
    y = "GenHlth",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$GenHlth, data$Diabetes_012)
cramer_v(data$GenHlth, data$Diabetes_012)

ggplot(data, aes(x = as.factor(Diabetes_012), y = MentHlth, fill = as.factor(Diabetes_012))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values=colorS) +
  labs(title = "Distribucija MentHlth po tipovima Diabetes_012", fill = "Dijabetes")

ggplot(data, aes(x = MentHlth, fill = Diabetes_012)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values =colorS) +
  labs(title = "Histogram MentHlth u odnosu na Diabetes_012",
       y = "Broj opservacija",
       fill = "Dijabetes")
anova_test(data$MentHlth,data$Diabetes_012)
tukey_fun(data$MentHlth,data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, y = PhysHlth, fill = Diabetes_012)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values=colorS) +
  labs(title = "Distribucija PhysHlth po tipovima Diabetes_012",
       fill = "Dijabetes")

ggplot(data, aes(x = PhysHlth, fill = Diabetes_012)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values =colorS) +
  labs(title = "Histogram PhysHlth u odnosu na Diabetes_012",
       y = "Broj opservacija",
       fill = "Dijabetes")
anova_test(data$PhysHlth,data$Diabetes_012)
tukey_fun(data$PhysHlth,data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill =DiffWalk )) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija DiffWalk u odnosu na tipove Diabetes_012",
       x = NULL,
       y = "Broj opservacija",
       fill = "Ima poteškoća")
ggplot(data %>% count(Diabetes_012, DiffWalk), 
       aes(x = Diabetes_012, y = DiffWalk, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija klasa DiffWalk u odnosu na tipove Diabetes_012",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$DiffWalk, data$Diabetes_012)
cramer_v(data$DiffWalk, data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill =Sex )) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija polova u odnosu na tipove Diabetes_012",
       y = "Broj opservacija",
       fill = "Pol")

ggplot(data %>% count(Diabetes_012, Sex), 
       aes(x = Diabetes_012, y = Sex, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija polova u odnosu na tipove Diabetes_012",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])

chi_sq_test(data$Diabetes_012,data$Sex)
cramer_v(data$Diabetes_012,data$Sex)

ggplot(data, aes(x = Diabetes_012, y = Age, fill = Diabetes_012)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values=colorS) +
  labs(title = "Distribucija godina po tipovima Diabetes_012",
       fill = "Dijabetes")

ggplot(data, aes(x = Age, fill = Diabetes_012)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values =colorS) +
  labs(title = "Histogram godina u odnosu na Diabetes_012",
       y = "Broj opservacija",
       fill = "Dijabetes")
anova_test(data$Age,data$Diabetes_012)
tukey_fun(data$Age,data$Diabetes_012)

ggplot(data, aes(x = Diabetes_012, fill =Education )) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija klasa Education u odnosu na tipove Diabetes_012",
       y = "Broj opservacija",
       fill = "Nivo obrazovanja")

ggplot(data %>% count(Diabetes_012, Education), 
       aes(x = Diabetes_012, y = Education, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija nivoa obrazovanja u odnosu na tipove Diabetes_012",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$Diabetes_012,data$Education)
cramer_v(data$Diabetes_012,data$Education)

prop.table(table(data$Education,data$Diabetes_012), margin = 2)*100

ggplot(data, aes(x = Diabetes_012, fill =Income )) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija nivoa primanja u odnosu na tipove Diabetes_012",
       y = "Broj opservacija",
       fill = "Primanja")
ggplot(data %>% count(Diabetes_012, Income), 
       aes(x = Diabetes_012, y = Income, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija visine primanja u odnosu na tipove Diabetes_012",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])

chi_sq_test(data$Diabetes_012,data$Income)
cramer_v(data$Diabetes_012,data$Income)


tabla_Incom_Diabetes_broj <- table(data$Income, data$Diabetes_012)
prop.table(tabla_Incom_Diabetes_broj, margin = 2) * 100

ggplot(data, aes(x = Education, fill =Income )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija nivoa primanja u odnosu na nivo obrazovanje",
       y = "Broj opservacija",
       fill = "Primanja")
ggplot(data %>% count(Education, Income), 
       aes(x = Education, y = Income, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija visine primanja u odnosu na tipove nivo obrazovanja",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$Education,data$Income)
cramer_v(data$Education,data$Income)

ggplot(data, aes(x = CholCheck, fill =HighChol )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija HighChol u odnosu na CholCheck",
       y = "Broj opservacija",
       fill = "Visok holesterol")
ggplot(data %>% count(CholCheck,HighChol), 
       aes(x = CholCheck, y = HighChol, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija HighChol u odnosu na CholCheck",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$CholCheck,data$HighChol)
cramer_v(data$CholCheck,data$HighChol)

ggplot(data, aes(x = DiffWalk, y = PhysHlth, fill = DiffWalk)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values=colorS) +
  labs(title = "Distribucija PhysHlth u odnosu na DiffWalk",
       fill = "Poteškoće u kretanju")
ggplot(data, aes(x = PhysHlth, fill = DiffWalk)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values =colorS) +
  labs(title = "Histogram dana teskoba u odnosu na poteškoće u kretanju",
       y = "Broj opservacija",
       fill = "Ima poteškoće")
anova_test(data$PhysHlth,data$DiffWalk)
tukey_fun(data$PhysHlth,data$DiffWalk)

ggplot(data, aes(x = HighBP, fill =HeartDiseaseorAttack )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija pojave srčanih oboljenja u odnosu na hipertenziju",
       y = "Broj opservacija",
       fill = "Srčana oboljenja")
ggplot(data %>% count(HighBP, HeartDiseaseorAttack), 
       aes(x = HighBP, y =HeartDiseaseorAttack, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija pojave srčanih oboljenja u odnosu na hipertenziju",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$HighBP,data$HeartDiseaseorAttack)
cramer_v(data$HighBP,data$HeartDiseaseorAttack)

ggplot(data, aes(x = HeartDiseaseorAttack, y = BMI, fill = HeartDiseaseorAttack)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values=colorS) +
  labs(title = "Distribucija BMI u odnosu na srčana oboljenja",
       fill = "Srčane poteškoće")
ggplot(data, aes(x =BMI, fill = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values =colorS) +
  labs(title = "Histogram BMI u odnosu na srčane tegobe",
       y = "Broj opservacija",
       fill = "Ima poteškoće")
anova_test(data$BMI,data$HeartDiseaseorAttack)
tukey_fun(data$BMI,data$HeartDiseaseorAttack)

ggplot(data, aes(x = PhysActivity, fill =DiffWalk )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija DiffWalk u odnosu na PhysActivity",
       y = "Broj opservacija",
       fill = "Ima poteškoća u hodanju")
ggplot(data %>% count(PhysActivity, DiffWalk), 
       aes(x = PhysActivity, y =DiffWalk, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija DiffWalk u odnosu na PhysActivity",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$PhysActivity,data$DiffWalk)
cramer_v(data$PhysActivity,data$DiffWalk)

ggplot(data, aes(x = Stroke, y = MentHlth, fill = Stroke)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values=colorS) +
  labs(title = "Distribucija MentHlth u odnosu na Stroke",
       fill = "Imao udar")
ggplot(data, aes(x =MentHlth, fill = Stroke)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values =colorS) +
  labs(title = "Histogram MentHlth u odnosu na Stroke",
       y = "Broj opservacija",
       fill = "Imao udar")
anova_test(data$MentHlth,data$Stroke)
tukey_fun(data$MentHlth,data$Stroke)

ggplot(data, aes(x = Stroke, fill =HighChol )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija HighChol u odnosu na Stroke",
       y = "Broj opservacija",
       fill = "Ima holesterol")
ggplot(data %>% count(Stroke, HighChol), 
       aes(x = Stroke, y =HighChol, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija HighChol u odnosu na Stroke",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$Stroke,data$HighChol)
cramer_v(data$Stroke,data$HighChol)

ggplot(data, aes(x = HeartDiseaseorAttack, fill =HighChol )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija HighChol u odnosu na HeartDisease",
       y = "Broj opservacija",
       fill = "Ima holesterol")
ggplot(data %>% count(HeartDiseaseorAttack, HighChol), 
       aes(x = HeartDiseaseorAttack, y =HighChol, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija HighChol u odnosu na HeartDisease",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data$HeartDiseaseorAttack,data$HighChol)
cramer_v(data$HeartDiseaseorAttack,data$HighChol)
library(ggplot2)
ggplot(data,aes(x=Age,y=BMI))+geom_point()
pearson_funkcija(data$Age,data$BMI)

ggplot(data,aes(x=PhysHlth,y=BMI))+geom_point()
pearson_funkcija(data$PhysHlth,data$BMI)

ggplot(data,aes(x=MentHlth,y=BMI))+geom_point()
pearson_funkcija(data$MentHlth,data$BMI)

ggplot(data,aes(x=Age,y=PhysHlth))+geom_point()
pearson_funkcija(data$Age,data$PhysHlth)

#Ciscenje podataka#############

data_clean <- data[data$BMI >= 12 & data$BMI <= 70, ]

print(data.frame(
  Skup = c("Originalni podaci", "Nakon čišćenja BMI"),
  Broj_opservacija = c(nrow(data), nrow(data_clean))
))

#Transformacija podataka####

nivoi_Age = sort(unique(data_clean$Age))
data_clean$Age = factor(data_clean$Age, levels = nivoi_Age, ordered = TRUE)
str(data_clean$Age)

#Feature Engeenering####
category_physHlth = c("nema problema", "blagi problemi", "umereni problemi", "teski problemi")
interval_physHlth = c(0, 5, 15, 30)
data_clean$PhysHlthCat = NA
data_clean$PhysHlthCat[data_clean$PhysHlth == interval_physHlth[1]] = category_physHlth[1]
data_clean$PhysHlthCat[data_clean$PhysHlth > interval_physHlth[1] & interval_physHlth[2] <= 5] = category_physHlth[2]
data_clean$PhysHlthCat[data_clean$PhysHlth > interval_physHlth[2] & interval_physHlth[3] <= 15] = category_physHlth[3]
data_clean$PhysHlthCat[data_clean$PhysHlth > interval_physHlth[3] & interval_physHlth[4] <= 30] = category_physHlth[4]

data_clean$PhysHlthCat <- factor(data_clean$PhysHlthCat, levels = category_physHlth,ordered = TRUE)
str(data_clean$PhysHlthCat)

category_mentHlth = c("nema problema", "blagi problemi", "umereni problemi", "teski problemi")
interval_mentHlth = c(0, 5, 15, 30)
data_clean$MentHlthCat = NA
data_clean$MentHlthCat[data_clean$MentHlth == interval_mentHlth[1]] = category_mentHlth[1]
data_clean$MentHlthCat[data_clean$MentHlth > interval_mentHlth[1] & interval_mentHlth[2] <= 5] = category_mentHlth[2]
data_clean$MentHlthCat[data_clean$MentHlth > interval_mentHlth[2] & interval_mentHlth[3] <= 15] = category_mentHlth[3]
data_clean$MentHlthCat[data_clean$MentHlth > interval_mentHlth[3] & interval_mentHlth[4] <= 30] = category_mentHlth[4]

data_clean$MentHlthCat <- factor(data_clean$MentHlthCat, levels = category_mentHlth,ordered = TRUE)
str(data_clean$MentHlthCat)

prop.table(table(data$Education,data$Diabetes_012), margin = 1)*100

nivoi_Education = levels(data_clean$Education)
data_clean$EducationCat = factor(data_clean$Education,
                             levels = nivoi_Education,
                             labels = c("Nisko",
                                        "Osnovno",
                                        "Srednje",
                                        "Srednje",
                                        "Visoko",
                                        "Visoko")
)
nivoi_Income = levels(data_clean$Income)
data_clean$IncomeCat = factor(data_clean$Income, 
                              levels = nivoi_Income, 
                              labels = c("Niska", 
                                         "Niska", 
                                         "Niska", 
                                         "Niska", 
                                         "Nisko-srednja", 
                                         "Nisko-srednja",
                                         "Srednja",
                                         "Visoka"))
ggplot(data_clean, aes(x = IncomeCat, fill = IncomeCat)) + 
  geom_bar() + 
  labs(title = "Distribucija kategorija IncomeCat", x = "Kategorije", y = "Broj opservacija") + 
  geom_text( 
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.8), 
    vjust = -0.3 ) + 
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

data_clean$AgeCat <- factor(data_clean$Age, levels = 1:13)

levels(data_clean$AgeCat) <- c(rep("Mladi", 4), rep("Zreli", 4), rep("Stariji", 2), rep("Seniori", 3))
ggplot(data_clean, aes(x = AgeCat, fill = AgeCat)) + 
  geom_bar() + 
  labs(title = "Distribucija kategorija AgeCat", x = "Kategorije", y = "Broj opservacija") + 
  geom_text( 
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.8), 
    vjust = -0.3 ) + 
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")


data_clean$CardioRiskScore = as.numeric(data_clean$HighBP == "Da") +
                            as.numeric(data_clean$HighChol == "Da") +
                            as.numeric(data_clean$HeartDiseaseorAttack == "Da")

nivoi_CardioRiskScore = sort(unique(data_clean$CardioRiskScore))
category_cardioRiskScore = c("nema rizika", "nizak rizik", "umeren rizik", "visok rizik")
data_clean$CardioRiskScore = factor(data_clean$CardioRiskScore,
                                    levels = nivoi_CardioRiskScore,
                                    labels = category_cardioRiskScore,
                                    ordered = TRUE)
str(data_clean$CardioRiskScore)

ggplot(data_clean, aes(x = CardioRiskScore, fill = CardioRiskScore)) +
  geom_bar() +
  labs(title = "Distribucija kategorija CardioRiskScore", x = "Kategorije", y = "Broj opservacija") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

data_clean$LifestyleRiskScore = as.numeric(data_clean$Smoker == "Da") +
                                as.numeric(data_clean$HvyAlcoholConsump == "Da") +
                                as.numeric(data_clean$PhysActivity == "Da")

nivoi_LifestyleRiskScore = sort(unique(data_clean$LifestyleRiskScore))
category_lifestyleRiskScore = c("nema rizika", "nizak rizik", "umeren rizik", "visok rizik")
data_clean$LifestyleRiskScore = factor(data_clean$LifestyleRiskScore,
                                    levels = nivoi_LifestyleRiskScore,
                                    labels = category_lifestyleRiskScore,
                                    ordered = TRUE)
str(data_clean$LifestyleRiskScore)

ggplot(data_clean, aes(x = LifestyleRiskScore, fill = LifestyleRiskScore)) +
  geom_bar() +
  labs(title = "Distribucija kategorija LifestyleRiskScore", x = "Kategorije", y = "Broj opservacija") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

data_clean$HealthScore = (as.numeric(data_clean$GenHlth) - 1) +
  (as.numeric(data_clean$PhysHlthCat) - 1) +
  (as.numeric(data_clean$MentHlthCat) - 1)

ggplot(data_clean, aes(x = HealthScore)) +
  geom_histogram(bins = 50, fill = "#7C4B73FF", color="#381A61FF") +
  labs(title = "Distribucija HealthScore", x = "HealthScore", y = "Broj opservacija")

summary(data_clean$HealthScore)

category_HealthScore = c("nizak rizik", "umeren rizik", "visok rizik", "ekstremni rizik")
interval_HealthScore = c(-1, 2, 5, 8, 10)
data_clean$HealthScore <- cut(data_clean$HealthScore,
                                 breaks = interval_HealthScore,
                                 labels = category_HealthScore,
                                 ordered_result = TRUE)

ggplot(data_clean, aes(x = HealthScore, fill = HealthScore)) +
  geom_bar() +
  labs(title = "Distribucija kategorija HealthScore", x = "Kategorije", y = "Broj opservacija") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

data_clean$DietScore = as.numeric(data_clean$Fruits == "Da") +
                      as.numeric(data_clean$Veggies == "Da")

nivoi_DietScore = sort(unique(data_clean$DietScore))
category_DietScore = c("nezdrava", "umereno zdrava", "zdrava")
data_clean$DietScore = factor(data_clean$DietScore,
                                    levels = nivoi_DietScore,
                                    labels = category_DietScore,
                                    ordered = TRUE)
str(data_clean$DietScore)

ggplot(data_clean, aes(x = DietScore, fill = DietScore)) +
  geom_bar() +
  labs(title = "Distribucija kategorija DietScore", x = "Kategorije", y = "Broj opservacija") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")


unique(data_clean$EducationCat)
unique(data_clean$IncomeCat)

data_clean$SocioEconomicStatus  = (as.numeric(data_clean$EducationCat) - 1) +
  (as.numeric(data_clean$IncomeCat) - 1) 
ggplot(data_clean, aes(x = SocioEconomicStatus)) +
  geom_histogram(bins = 50, fill = "#7C4B73FF", color="#381A61FF") +
  labs(title = "Distribucija SocioEconomicStatus", x = "SocioEconomicStatus", y = "Broj opservacija")

summary(data_clean$SocioEconomicStatus)

category_SocioEconomicStatus = c("nizak", "srednji", "visok")
interval_SocioEconomicStatus = c(-1, 3, 5, 6)
data_clean$SocioEconomicStatus <- cut(data_clean$SocioEconomicStatus,
                                      breaks = interval_SocioEconomicStatus,
                                      labels = category_SocioEconomicStatus,
                                      ordered_result = TRUE)

str(data_clean$SocioEconomicStatus)
ggplot(data_clean, aes(x = SocioEconomicStatus, fill = SocioEconomicStatus)) +
  geom_bar() +
  labs(title = "Distribucija kategorija SocioEconomicStatus", x = "Kategorije", y = "Broj opservacija") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  scale_fill_paletteer_d("MetBrewer::Archambault") + theme(legend.position="none")

#Bivarijantna analiza transformisanih####
ggplot(data_clean, aes(x = PhysHlthCat, fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija PhysHlthCat po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, PhysHlthCat), 
       aes(x = Diabetes_012, y = PhysHlthCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija PhysHlthCat po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "PhysHlthCat",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$PhysHlthCat, data_clean$Diabetes_012)
cramer_v(data_clean$PhysHlthCat, data_clean$Diabetes_012)


ggplot(data_clean, aes(x = MentHlthCat, fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija MentHlthCat po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, MentHlthCat), 
       aes(x = Diabetes_012, y = MentHlthCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija MentHlthCat po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "MentHlthCat",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$MentHlthCat, data_clean$Diabetes_012)
cramer_v(data_clean$MentHlthCat, data_clean$Diabetes_012)

  ggplot(data_clean, aes(x = EducationCat, fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija EducationCat po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, EducationCat), 
       aes(x = Diabetes_012, y = EducationCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija EducationCat po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "EducationCat",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$EducationCat, data_clean$Diabetes_012)
cramer_v(data_clean$EducationCat, data_clean$Diabetes_012)

ggplot(data_clean, aes(x = Diabetes_012, fill =IncomeCat )) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija IncomeCat u odnosu na PhysActivity",
       y = "Broj opservacija",
       fill = "Nivo primanja")

ggplot(data_clean %>% count(Diabetes_012, IncomeCat), 
       aes(x = Diabetes_012, y = IncomeCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija primanja u odnosu na tipove Diabetes_012",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])

chi_sq_test(data_clean$IncomeCat, data_clean$Diabetes_012)
cramer_v(data_clean$IncomeCat, data_clean$Diabetes_012)

ggplot(data_clean, aes(x = Diabetes_012, fill =AgeCat)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = colorS) +
  labs(title = "Distribucija AgeCat u odnosu na PhysActivity",
       y = "Broj opservacija",
       fill = "Starosna dob")

ggplot(data_clean %>% count(Diabetes_012, AgeCat), 
       aes(x = Diabetes_012, y = AgeCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija starosne dobi u odnosu na tipove Diabetes_012",
    fill = "Broj opservacija"
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2])

chi_sq_test(data_clean$AgeCat, data_clean$Diabetes_012)
cramer_v(data_clean$AgeCat, data_clean$Diabetes_012)

  ggplot(data_clean, aes(x = CardioRiskScore, fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija CardioRiskScore po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, CardioRiskScore), 
       aes(x = Diabetes_012, y = CardioRiskScore, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija CardioRiskScore po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "CardioRiskScore",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$CardioRiskScore, data_clean$Diabetes_012)
cramer_v(data_clean$CardioRiskScore, data_clean$Diabetes_012)


ggplot(data_clean, aes(x = LifestyleRiskScore, fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija LifestyleRiskScore po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, LifestyleRiskScore), 
       aes(x = Diabetes_012, y = LifestyleRiskScore, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija LifestyleRiskScore po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "LifestyleRiskScore",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$LifestyleRiskScore, data_clean$Diabetes_012)
cramer_v(data_clean$LifestyleRiskScore, data_clean$Diabetes_012)


ggplot(data_clean, aes(x = HealthScore, fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija HealthScore po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, HealthScore), 
       aes(x = Diabetes_012, y = HealthScore, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija HealthScore po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "HealthScore",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$HealthScore, data_clean$Diabetes_012)
cramer_v(data_clean$HealthScore, data_clean$Diabetes_012)


ggplot(data_clean, aes(x = SocioEconomicStatus , fill = Diabetes_012 ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija SocioEconomicStatus  po kategorijama Diabetes_012",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(Diabetes_012, SocioEconomicStatus), 
       aes(x = Diabetes_012, y = SocioEconomicStatus, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija SocioEconomicStatus po kategorijama Diabetes_012",
    x = "Diabetes_012",
    y = "SocioEconomicStatus",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$SocioEconomicStatus, data_clean$Diabetes_012)
cramer_v(data_clean$SocioEconomicStatus, data_clean$Diabetes_012)

ggplot(data_clean, aes(x = EducationCat, fill =IncomeCat )) + 
  geom_bar(position = "dodge")+ scale_fill_manual(values = colorS) + 
  labs(title = "Distribucija nivoa primanja u odnosu na nivo obrazovanje", y = "Broj opservacija", 
       fill = "Primanja") 
ggplot(data_clean %>% count(EducationCat, IncomeCat),  
       aes(x = EducationCat, y = IncomeCat, fill = n)) + 
  geom_tile() + 
  geom_text(aes(label = n)) + 
  labs( 
    title = "Distribucija visine primanja u odnosu na tipove nivo obrazovanja", 
    fill = "Broj opservacija" 
  )+ 
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data_clean$EducationCat,data_clean$IncomeCat) 
cramer_v(data_clean$EducationCat,data_clean$IncomeCat) 

ggplot(data_clean, aes(x = MentHlthCat, fill = Stroke ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija MentHlthCat po kategorijama Stroke",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")
ggplot(data_clean %>% count(Stroke, MentHlthCat), 
       aes(x = Stroke, y = MentHlthCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija MentHlthCat po kategorijama Stroke",
    x = "Diabetes_012",
    y = "MentHlthCat",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$MentHlthCat, data_clean$Stroke)
cramer_v(data_clean$MentHlthCat, data_clean$Stroke)


  ggplot(data_clean, aes(x = PhysHlthCat, fill = DiffWalk ))  +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribucija kategorija PhysHlthCat po kategorijama DiffWalk",
    y = "Broj opservacija"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault") +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.3
  ) +
  theme(legend.position="bottom")

ggplot(data_clean %>% count(DiffWalk, PhysHlthCat), 
       aes(x = DiffWalk, y = PhysHlthCat, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(
    title = "Distribucija kategorija PhysHlthCat po kategorijama Stroke",
    x = "DiffWalk",
    y = "PhysHlthCat",
    fill = "Broj opservacija"
    
  )+
  scale_fill_gradient(low = colorS[1], high = colorS[2]) +
  theme(legend.position="bottom")

chi_sq_test(data_clean$PhysHlthCat, data_clean$DiffWalk)
cramer_v(data_clean$PhysHlthCat, data_clean$DiffWalk)

ggplot(data_clean, aes(x = AgeCat, fill =HealthScore )) + 
  
  geom_bar(position = "dodge")+ 
  
  scale_fill_manual(values = colorS) + 
  
  labs(title = "Distribucija zdravstvenog stanja a u odnosu na starosnu grupu", 
       
       y = "Broj opservacija", 
       
       fill = "Zdravstveno stanje")

ggplot(data_clean %>% count(AgeCat, HealthScore),  
       aes(x = AgeCat, y = HealthScore, fill = n)) + 
  geom_tile() + 
  geom_text(aes(label = n)) + 
  labs( 
    title = "Distribucija zdravstvenog stanja a u odnosu na starosnu grupu", 
    fill = "Broj opservacija" 
  )+ 
  scale_fill_gradient(low = colorS[1], high = colorS[2])
chi_sq_test(data_clean$AgeCat,data_clean$HealthScore) 
cramer_v(data_clean$AgeCat,data_clean$HealthScore) 

#Feature Selection####

dim(data_clean)
names(data_clean)

selected_features <- c("BMI", "Stroke", "DiffWalk", "CardioRiskScore", 
                       "LifestyleRiskScore", "HealthScore", 
                       "SocioEconomicStatus", "AgeCat", "Diabetes_012")

data_selected <- data_clean[, selected_features]
dim(data_selected)

#Test/Train split####
raspodela_Diabetes_012 = data_selected %>%
                          count(Diabetes_012) %>%
                          mutate(Udeo = round(n / sum(n) * 100, 2))

print(raspodela_Diabetes_012)

set.seed(83762021)
train_index <- createDataPartition(
  data_selected$Diabetes_012,
  p = 0.8,
  list = FALSE
)

data_train = data_selected[train_index, ]
data_test  = data_selected[-train_index, ]

raspodela_trening = data_train %>%
              count(Diabetes_012) %>%
              mutate(Udeo = round(n / sum(n) * 100, 2))%>% 
              mutate(Skup = "Trening skup")
raspodela_test = data_test %>%
              count(Diabetes_012) %>%
              mutate(Udeo = round(n / sum(n) * 100, 2))%>% 
              mutate(Skup = "Test skup")

raspodela_Diabetes_012 = data_selected %>%
              count(Diabetes_012) %>%
              mutate(Udeo = round(n / sum(n) * 100, 2))%>% 
              mutate(Skup = "Ceo skup")

raspodela_tabele <- bind_rows(raspodela_trening, raspodela_test, raspodela_Diabetes_012)

ggplot(raspodela_tabele, aes(x = Diabetes_012, y = Udeo, fill = Skup)) +
  geom_col(position = "dodge") +
  labs(
    title = "Uporedna raspodela kategorija po skupovima",
    y = "Procenat (%)",
    x = "Diabetes_012"
  ) + scale_fill_paletteer_d("MetBrewer::Archambault")
#Balansirnje####

print(raspodela_trening)

predijabetes = subset(data_train, Diabetes_012 == "predijabetes")
dijabetes = subset(data_train, Diabetes_012 == "dijabetes")
nemadijabetes = subset(data_train, Diabetes_012 == "nema dijabetes")
set.seed(83762021)

predijabetes_oversampling = predijabetes[sample(nrow(predijabetes), 15000, replace = TRUE), ]
nemadijabetes_undersampling = nemadijabetes[sample(nrow(nemadijabetes), 30000), ]

data_train_balanced = rbind(predijabetes_oversampling, dijabetes, nemadijabetes_undersampling)



raspodela_data_train_balanced = data_train_balanced %>%
  count(Diabetes_012) %>%
  mutate(Udeo = round(n / sum(n) * 100, 2))

dim(data_train_balanced)
#Unakrsna validacija####

set.seed(83762021)

model_1 = glm(
  Diabetes_012 ~ BMI + Stroke + DiffWalk + CardioRiskScore +
    LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat,
  data = data_train_balanced,
  family = binomial
)

cv_error_model1 = cv.glm(data_train_balanced, model_1, K = 10)$delta[1]

model_2 = glm(
  Diabetes_012 ~ BMI * HealthScore + Stroke + DiffWalk + CardioRiskScore +
    LifestyleRiskScore + SocioEconomicStatus + AgeCat,
  data = data_train_balanced,
  family = binomial
)
cv_error_model2 = cv.glm(data_train_balanced, model_2, K = 10)$delta[1]

stepen = 1:4
cv_error_model3 = numeric(length(stepen))

for (s in stepen) {
  model <- glm(
    Diabetes_012 ~ poly(BMI, s) + Stroke + DiffWalk + CardioRiskScore +
      LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat,
    data = data_train_balanced,
    family = binomial
  )
  cv = cv.glm(data_train_balanced, model, K = 10)
  cv_error_model3[s]  =  cv$delta[1]
}

cv_error_model4 = numeric(length(stepen))
for (s in stepen) {
  model <- glm(
    Diabetes_012 ~ poly(BMI, s)*HealthScore + Stroke + DiffWalk + CardioRiskScore +
      LifestyleRiskScore  + SocioEconomicStatus + AgeCat,
    data = data_train_balanced,
    family = binomial
  )
  cv = cv.glm(data_train_balanced, model, K = 10)
  cv_error_model4[s]  =  cv$delta[1]
}
cv_rezultati_logisticka = data.frame(
  Model = c(
    "Model 1: linearni",
    "Model 2: BMI × HealthScore",
    paste0("Model 3: poly(BMI,", stepen, ")"),
    paste0("Model 4: poly(BMI,", stepen, ") × HealthScore")
  ),
  CV_Error = c(
    cv_error_model1,
    cv_error_model2,
    cv_error_model3,
    cv_error_model4
  )
)
print(cv_rezultati_logisticka)

k = 10
podskupovi <- createFolds(data_train_balanced$Diabetes_012, k = k, list = TRUE, returnTrain = FALSE)
cv_error_cart <- numeric(k)
cv_error_rf <- numeric(k)

for(i in 1:k){
  test_idx <- podskupovi[[i]]
  train_idx <- setdiff(1:nrow(data_train_balanced), test_idx)
  
  model_cart <- rpart(Diabetes_012 ~ BMI + Stroke + DiffWalk + CardioRiskScore +
                        LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat,
                      data = data_train_balanced[train_idx, ], method = "class")
  
  preds <- predict(model_cart, data_train_balanced[test_idx, ], type = "class")
  cv_error_cart[i] <- mean(preds != data_train_balanced$Diabetes_012[test_idx])
}




for(i in 1:k){
  test_idx <- podskupovi[[i]]
  train_idx <- setdiff(1:nrow(data_train_balanced), test_idx)
  
  model_rf <- randomForest(Diabetes_012 ~ BMI + Stroke + DiffWalk + CardioRiskScore +
                             LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat,
                           data = data_train_balanced[train_idx, ],
                           ntree = 500)
  
  preds <- predict(model_rf, data_train_balanced[test_idx, ])
  cv_error_rf[i] <- mean(preds != data_train_balanced$Diabetes_012[test_idx])
}


mean_cv_error_cart <- mean(cv_error_cart)
mean_cv_error_rf <- mean(cv_error_rf)

# Možete napraviti tabelu sa svim rezultatima
results_rf_cart <- data.frame(
  Model = c("CART", "Random Forest"),
  CV_Error = c(mean_cv_error_cart, mean_cv_error_rf)
)
results_rf_cart

#Finalno treniranje####
lr_model=Diabetes_012 ~ poly(BMI, 4) + Stroke + DiffWalk + CardioRiskScore + LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat
lr_model = glm( Diabetes_012 ~ BMI + Stroke + DiffWalk + CardioRiskScore + LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat,   data = data_train_balanced,  family = binomial )
summary(lr_model)


cart_model <- rpart(Diabetes_012 ~ BMI + Stroke + DiffWalk + CardioRiskScore + 
                      LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat, 
                    data = data_train_balanced, 
                    method = "class")

rpart.plot(cart_model)
rf_model<-randomForest(Diabetes_012 ~ BMI + Stroke + DiffWalk + CardioRiskScore + 
                         LifestyleRiskScore + HealthScore + SocioEconomicStatus + AgeCat, 
                       data = data_train_balanced, 
                       ntree = 500,
                       importance = TRUE)
print(rf_model$confusion)

#Predikcija i metrike####
lm_preds <- predict(lr_model, newdata = data_test)
cart_preds <- predict(cart_model, newdata = data_test, type = "class")
rf_preds <- predict(rf_model, newdata = data_test)

nivoi <- levels(as.factor(data_test$Diabetes_012))
stvarni_f <- factor(data_test$Diabetes_012, levels = nivoi)
cart_f    <- factor(cart_preds,             levels = nivoi)
rf_f      <- factor(rf_preds,               levels = nivoi)


glm_preds_zaokruzeno <- round(lm_preds)
glm_preds_zaokruzeno[glm_preds_zaokruzeno < 0] <- 0
glm_preds_zaokruzeno[glm_preds_zaokruzeno > 2] <- 2
glm_f <- factor(nivoi[glm_preds_zaokruzeno + 1], levels = nivoi)


finalna_tabela <- rbind(
  izvuci_sve_metrike(glm_f, stvarni_f, "Logisticka Regresija"),
  izvuci_sve_metrike(cart_f, stvarni_f, "Stablo (CART)"),
  izvuci_sve_metrike(rf_f, stvarni_f, "Random Forest")
)

finalna_tabela[is.na(finalna_tabela)] <- 0
print(finalna_tabela)

