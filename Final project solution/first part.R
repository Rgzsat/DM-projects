#install.packages("factoextra")
install.packages("caret")
install.packages("naniar")
install.packages("lares")
library(lares)
library("readxl")
library("scatterplot3d") # load
library("car")
library("rgl")

#####DATASET HERE: https://www.vertica.com/python/examples/battery/

##########DATA PREPROCESSING

df = read.csv("dataset")
library(dplyr) #using DPLYR
df_discharge=df[which(df[,'type']=='discharge'),] #
df_charge= df[which(df[,'type']=='charge'),] #

corr_cross(df_charge, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 5 # display top 10 couples of variables (by correlation coefficient)
)

corr_cross(df_discharge, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top =5 # display top 10 couples of variables (by correlation coefficient)
)

kpi_ch_cols= c('Voltage_measured', 'Current_measured', 'Temperature_measured'
               ,'Current_charge', 'Voltage_charge', 'Time')
kpi_dch_cols= c('Voltage_measured', 'Current_measured', 'Temperature_measured'
                ,'Current_charge', 'Voltage_charge', 'Capacity', 'Time')

kpi_charge=df_charge[,kpi_ch_cols]
kpi_discharge=df_discharge[,kpi_dch_cols]
t_charge= df_charge[,"Time"] #TIME FOR CHARGING
t_discharge= df_discharge[,"Time"] #TIME FOR DISCHARGING

##########PCA and additional correlations

library(naniar)
library(factoextra)
library("corrplot")
library(ggbiplot)
library(caret)

#FOR CHARGING
head(kpi_charge)
n_miss(kpi_charge)
prop_miss(kpi_charge)
gg_miss_var(kpi_charge)

nearZeroVar(kpi_charge, names = TRUE, 
            freqCut = 2, uniqueCut = 20) #low variance

pca_ch=prcomp(kpi_charge, center = TRUE,scale. = TRUE)
summary(pca_ch)
predict(pca_ch, kpi_charge)
fviz_eig(pca_ch, addlabels = TRUE,  hjust = 0, main = "Principal Component
         Analysis- Charge")+ylim(0, 80)
var=get_pca_var(pca_ch)
corrplot(var$cos2, is.corr = FALSE)#Cos2 for the variables" 
corrplot(var$cor, is.corr = FALSE)#"Correlations between variables and dimensions"

corrplot(var$contrib, is.corr = FALSE)#"contributions of the variables" 

fviz_pca_var(pca_ch,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             # Avoid text overlapping
)

#FOR DISCHARGING 
head(kpi_discharge)
n_miss(kpi_discharge)
prop_miss(kpi_discharge)
gg_miss_var(kpi_discharge)

nearZeroVar(kpi_discharge, names = TRUE, 
            freqCut = 2, uniqueCut = 20) #low variance

pca_disch=prcomp(kpi_discharge, center = TRUE,scale. = TRUE)
summary(pca_disch)
predict(pca_disch, kpi_discharge)
fviz_eig(pca_disch, addlabels = TRUE,  hjust = 0, main = "Principal Component
         Analysis- Discharge")+ylim(0, 60)
var2=get_pca_var(pca_disch)
corrplot(var2$cos2, is.corr = FALSE)#Cos2 for the variables" 
corrplot(var2$cor, is.corr = FALSE)#"Correlations between variables and dimensions"

corrplot(var2$contrib, is.corr = FALSE)#"contributions of the variables" 

fviz_pca_var(pca_disch,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             # Avoid text overlapping
