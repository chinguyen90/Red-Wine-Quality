#Rename variables
colnames(RedWineData_Stat6509Project)=c("FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Denisty", "pH", "Sulphates", "Alcohol", "Quality")

#examine the data
hist(RedWineData_Stat6509Project$Quality,main="Distribution of red wine quality", col = c("pink"))

str(RedWineData_Stat6509Project)
table(RedWineData_Stat6509Project$Quality)

#more informative scatterplot matrix
install.packages("psych")
library(psych)
pairs.panels(RedWineData_Stat6509Project[c("Quality","FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Denisty", "pH", "Sulphates", "Alcohol")])


# Run AIC to choose variables for model
null=lm(Quality~1, data=RedWineData_Stat6509Project)

#Additive models
full = lm(Quality~FixedAcidity+VolatileAcidity+CitricAcid+ResidualSugar+Chlorides+FreeSulfurDioxide+TotalSulfurDioxide+Denisty+pH+Sulphates+Alcohol, data=RedWineData_Stat6509Project)

step(null, scope=list(lower=null, upper=full),
     direction="forward")

model1=lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
            TotalSulfurDioxide + Chlorides + pH + FreeSulfurDioxide, 
          data = RedWineData_Stat6509Project)
summary(model1)

#Diagnostics with Model 1
par(mfrow=c(2,2)) 
plot(model1, which=1:4)


#With Interactions
full2 = lm(Quality~FixedAcidity*VolatileAcidity*CitricAcid*ResidualSugar*Chlorides*FreeSulfurDioxide*TotalSulfurDioxide*Denisty*pH*Sulphates*Alcohol, data=RedWineData_Stat6509Project)
summary(full2)
step(null, scope=list(lower=null, upper=full2),
     direction="forward")
model2=lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
            TotalSulfurDioxide + Chlorides + pH + CitricAcid + FreeSulfurDioxide + 
            Alcohol:Sulphates + Sulphates:TotalSulfurDioxide + VolatileAcidity:TotalSulfurDioxide + 
            Sulphates:CitricAcid + TotalSulfurDioxide:CitricAcid + Chlorides:FreeSulfurDioxide, 
          data = RedWineData_Stat6509Project)
summary(model2)
#Diagnostics with Model 2
par(mfrow=c(2,2)) 
plot(model2, which=1:4)

#Check if interaction needed
anova(model1,model2)

#model 3 interaction removing insignificant variables
model3=lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
            TotalSulfurDioxide + Chlorides + pH + CitricAcid + FreeSulfurDioxide + 
            Alcohol:Sulphates + Sulphates:TotalSulfurDioxide + VolatileAcidity:TotalSulfurDioxide, 
          data = RedWineData_Stat6509Project)
summary(model3)

#Compare models
anova(model1,model3)

#Choose model 3 over model 1
#Diagnostics for model 3
par(mfrow=c(2,2)) 
plot(model3, which=1:4)

#Improve the model 3
#Check outliers
install.packages("car")
library(car)
par(mfrow=c(1,1)) 
qqPlot(model3, id.n=2)

outlierTest(model3)

#Outliers with high leverage
influencePlot(model3)

#removing highest leverage outliers FINAL MODEL
newdata1 <- RedWineData_Stat6509Project[c(-363,-1300),]
model3c = lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
               TotalSulfurDioxide + Chlorides + pH + CitricAcid + FreeSulfurDioxide + 
               Alcohol:Sulphates + Sulphates:TotalSulfurDioxide + VolatileAcidity:TotalSulfurDioxide, 
             data = newdata1)
summary(model3c)

#Check outlier distribution with the final model
influencePlot(model3c)

#Diagnostics for model 3
par(mfrow=c(2,2)) 
plot(model3c, which=1:4)

#Diagnostics Model 3c
par(mfrow=c(2,2))
plot(model3c, which=1:4)
shapiro.test(model3c$residuals)
ncvTest(model3c)
