library(dplyr)
library(ggplot2)
library(caret)

# Dataframe
df <- read.csv("C:/Agus/2023 2do cuatri/analitica descriptiva/datasets/sleep/Sleep_health_and_lifestyle_dataset.csv")

# Dataframe numericas

df_num<-df %>% select(where(is.numeric))

# Analisis de las variables numericas

summary(df_num)

# Scatter plot de variables

pairs(df_num)

# Histogramas y QQPlots

ggplot(df, aes(x = Sleep.Duration)) +
  geom_histogram( fill = "#A4D3EE", color = "black") +
  labs(title = "Histograma de Sleep Duration", x = "Valores", y = "Frecuencia")
qqnorm(df$Sleep.Duration)

ggplot(df, aes(x = Physical.Activity.Level)) +
  geom_histogram( fill = "#A4D3EE", color = "black") +
  labs(title = "Histograma de Physical Activity", x = "Valores", y = "Frecuencia")
qqnorm(df$Physical.Activity.Level)

ggplot(df, aes(x = Daily.Steps)) +
  geom_histogram( fill = "#A4D3EE", color = "black") +
  labs(title = "Histograma de Daily Steps", x = "Valores", y = "Frecuencia")
qqnorm(df$Daily.Steps)

ggplot(df, aes(x = Quality.of.Sleep)) +
  geom_histogram( fill = "#A4D3EE", color = "black") +
  labs(title = "Histograma de Quality Sleep", x = "Valores", y = "Frecuencia")
qqnorm(df$Quality.of.Sleep)

ggplot(df, aes(x = Stress.Level)) +
  geom_histogram( fill = "#A4D3EE", color = "black") +
  labs(title = "Histograma de Stress Level", x = "Valores", y = "Frecuencia")
qqnorm(df$Stress.Level)


# Outliers de variables numericas y boxplots

for (col in colnames(df_num)){
  bp<-boxplot(df_num[,col],main=col)
  print(paste("Cantidad de outliers de ",col,":",length(bp$out)))
}

# Solo la variable Heart.Rate posee outliers, imputamos con mediana.

Q1_hr<-quantile(df$Heart.Rate, 0.25)
Q3_hr<-quantile(df$Heart.Rate, 0.75)
RIC_hr <- Q3_hr-Q1_hr
df$Heart.Rate[df$Heart.Rate<(Q1_hr - 1.5 * RIC_hr)|df$Heart.Rate>(Q3_hr + 1.5 * RIC_hr)]<-median(df$Heart.Rate)

boxplot(df$Heart.Rate) # No hay mas outliers


# Valores nulos

na<-sum(is.na(df)) # 0 valores nulos

# Categorizacion de Blood Pressure

division<-strsplit(df$Blood.Pressure,split="/")

df$systolic<-sapply(division, function(x)x[1])
df$diastolic<-sapply(division, function(x)x[2])

df$systolic<-as.numeric(df$systolic)
df$diastolic<-as.numeric(df$diastolic)

df$Blood_Pressure_Category <- ifelse(df$systolic < 120 & df$diastolic < 80, "Normal",
                                     ifelse((df$systolic >= 120 & df$systolic <= 129) & df$diastolic < 80, "Elevado",
                                            ifelse((df$systolic >= 130 & df$systolic <= 139) | (df$diastolic >= 80 & df$diastolic <= 89), "Hipertension 1",
                                                   ifelse(df$systolic >= 140 | df$diastolic >= 90, "Hipertension 2",
                                                          ifelse(df$systolic > 180 & df$diastolic >= 120, "Crisis hipertensiva", "Muerto")))))

unique(df$Blood_Pressure_Category) 
# Quedan las variables: 
# Normal
# Elevado
# Hipertension 1
# Hipertension 2


#--------------------------------------
# SUB-OBJETIVOS

# TRABAJOS QUE TIENDEN A DORMIR MENOS

# occupation y sleep duration
unique(df$Occupation)

#Hipotesis coloquial:"Las personas con la profesion i duermen menos que el resto de las profesiones"
#Formal: "El promedio de horas de sueño de las personas con profesion i es menor que el de las otras profesiones

#H0: mean(Sleep.Duration|Occupation==i)>=mean(Sleep.Duration|Occupation!=i)
#H1: mean(Sleep.Duration|Occupation==i)<mean(Sleep.Duration|Occupation!=i)

#Gráficos:
df %>% ggplot(aes(x=Occupation, y = Sleep.Duration))+geom_boxplot()
df %>% ggplot(aes(fill=Occupation, x = Sleep.Duration))+geom_density(alpha = 0.4)

jobs<-unique(df$Occupation)
jobs_more_sleep<-c()
for (i in jobs){
  if(nrow(df[df$Occupation==i,])>1){
    t<-t.test(df[df$Occupation==i,]$Sleep.Duration,df[df$Occupation!=i,]$Sleep.Duration, alternative = 'greater')
    if (t$p.value<0.05){
      jobs_more_sleep<-c(jobs_more_sleep,i)
    }
  }
}
jobs_more_sleep
# Hay evidencia estadistica para afirmar que solo las profesiones de Ingenieros y Abogados duermen mas.

# TRABAJOS QUE TIENDEN AL SEDENTARISMO

scatter_f <- function(df, x, y, z) {
  ggplot(df, aes(x = {{x}}, y = {{y}})) +
    geom_point(aes(color = {{z}})) +
    labs(x = deparse(substitute(x)), y = deparse(substitute(y))) +
    scale_color_discrete(name = as_label(enquo(z))) +
    theme_minimal()
}

scatter_f(df, df$Sleep.Duration, df$Physical.Activity.Level, df$Occupation)

#Analizar la relación entre las horas y la calidad del sueño con el BMI y la presión en sangre, con el objetivo de buscar patrones entre el bienestar físico y la calidad del sueño.
# Exploración gráfica
# Boxplot para Sleep Duration vs BMI Category
ggplot(df, aes(x=`BMI.Category`, y=`Sleep.Duration`)) +
  geom_boxplot(aes(fill=`BMI.Category`)) +
  labs(title="Distribución de Horas de Sueño por Categoría de BMI",
       x="Categoría de BMI",
       y="Horas de Sueño") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position="none")
# categoría "Overweight" tiende a tener una menor mediana de horas de sueño en comparación a otras
# categoría "Normal" muestra mayor variabilidad en las horas de sueño


# Boxplot para Sleep Duration vs Blood Pressure Category
ggplot(df, aes(x=`Blood_Pressure_Category`, y=`Sleep.Duration`)) +
  geom_boxplot(aes(fill=`Blood_Pressure_Category`)) +
  labs(title="Distribución de Horas de Sueño por Categoría de Presión Arterial",
       x="Categoría de Presión Arterial",
       y="Horas de Sueño") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position="none")
# "Hipertensión 2" muestra menor mediana de horas de sueño en comparación con otras 
# categorías "Normal" y "Elevado" tienen medianas similares de horas de sueño

# Para continuar genero una nueva variable que integre cantidad y calidad de sueño
df$Sleep_Score <- df$`Sleep.Duration` * df$`Quality.of.Sleep`

# Repito análisis gráfico y veo si incorporar calidad generó cambios

# Boxplot para Sleep Score vs BMI Category
ggplot(df, aes(x=`BMI.Category`, y=`Sleep_Score`)) +
  geom_boxplot(aes(fill=`BMI.Category`)) +
  labs(title="Distribución de Sleep Score por Categoría de BMI",
       x="Categoría de BMI",
       y="Sleep Score") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position="none")
# categoría "Obeso" tiende a tener una menor mediana de sleep score en comparación a otras
# "Normal" y "Sobrepeso" muestran medianas similares de Sleep Score, 
# pero hay una mayor variabilidad en el grupo "Normal".

# Boxplot para Sleep Score vs Blood Pressure Category
ggplot(df, aes(x=`Blood_Pressure_Category`, y=`Sleep_Score`)) +
  geom_boxplot(aes(fill=`Blood_Pressure_Category`)) +
  labs(title="Distribución de Horas de Sueño por Categoría de Presión Arterial",
       x="Categoría de Presión Arterial",
       y="Sleep score") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(legend.position="none")
# "Hipertensión 2" muestra menor mediana de sleep score en comparación con otras 
# categorías "Normal" y "Elevado" tienen medianas similares de sleep score


# Análisis de la relación

# One Way ANOVA: Nos permite ver la relación enntre una categórica y una numérica

# BMI
# H coloquial: las medias de sleep score no varían entre categorías de BMI
# H0: No existen diferencias significativas en las medias de sleep score entre categorias de bmi
# H1: existen diferencias significativas en las medias de sleep score entre categorias de bmi

anova_bmi <- aov(Sleep_Score ~ `BMI.Category`, data = df)
summary(anova_bmi)

# p-value significativo, hay evidencia significativa para rechazar la hipótesis nula y concluir que
# hay diferencias significativas en el Sleep Score entre al menos dos de las categorías de BMI


# Blood Pressure
# H coloquial: las medias de sleep score no varían entre categorías de Blood Pressure
# H0: No existen diferencias significativas en las medias de sleep score entre categorias de Blood Pressure
# H1: existen diferencias significativas en las medias de sleep score entre categorias de Blood Pressure

anova_bp <- aov(Sleep_Score ~ Blood_Pressure_Category, data = df)
summary(anova_bp)

# p-value significativo, hay evidencia significativa para rechazar la hipótesis nula y concluir que
# hay diferencias significativas en el Sleep Score entre al menos dos de las categorías de Blood Pressure

# Two Way ANOVA: Nos permite ver el efecto conjunto de dos categóricas sobre una numérica

# H coloquial: las dos H previas + no hay interración entre las categóricas en su efecto sobre Sleep Score
# H0: las dos H0 previas + el efecto de BMI sobre Sleep Score es independiente al nivel de Blood Pressure y viceversa
# H1: las dos H1 previas + el efecto de al menos una de las categóricas sobre Sleep Score es dependiente del nivel de la otra

anova_twoway <- aov(Sleep_Score ~ `BMI.Category` * Blood_Pressure_Category, data = df)
summary(anova_twoway)

# p-value significativo para los tres análisis:
# efecto de ambas categórcas sobre sleep score de forma independiente es significativo (consistente con los One Way ANOVA previos)
# el efecto de una de las variables sobre Sleep Score depende del nivel de la otra variable, 
# existe relación entre las dos variables categóricas


#Conclusion

# Ambos factores, BMI.Category y Blood_Pressure_Category, tienen un efecto significativo sobre el Sleep Score 
# Además, hay una interacción significativa entre ellos, lo que sugiere que el efecto de uno de los factores 
# sobre el Sleep Score no es constante y varía según el nivel del otro factor.


