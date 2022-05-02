library(readxl)
# library(lme4)

green <- read_excel(filename<-"green_010_hfsm3_50#1.xlsm", sheet<-"curves processed")
strain <- green$`A: Strain`
stress <- green$`A: Stress`
df <- data.frame(stress<-stress, strain<-strain)
plot(strain, stress)

model <- lm(stress~strain, data=df)
summary(model)

x <- seq(from=0, to=0.4, by=0.001)
y <- predict(object<-model, newdata<-data.frame(strain<-x))
lines(x, y, col='red')
length(x)
length(y)
y
length(model)
summary(model)
