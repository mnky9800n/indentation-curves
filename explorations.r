library(readxl)
# library(lme4)
# library(ggplot2)

green <- read_excel(filename<-"green_010_hfsm3_50#1.xlsm", sheet<-"curves processed")
strain <- green$`A: Strain`
stress <- green$`A: Stress`
df <- data.frame(stress=stress, strain=strain)
plot(strain, stress)
# ggplot(strain, stress)

# linear naive model
# the line whre its like
# $ data.frame(strain<-x)
# is because the model A) requires a data.frame object
# and B) because it requires that data.frame to have a
# column called "strain"
model <- lm(stress~strain, data=df)
x <- seq(from=0, to=0.4, by=0.001)
y <- predict(object<-model, newdata<-data.frame(strain<-x))
lines(x, y, col='red', lty=1)

# linear model, <0.1 strain
df_leq01 <- df[df$strain < 0.1,]
model <- lm(stress~strain, data=df_leq01)
y <- predict(object<-model, newdata<-data.frame(strain<-x))
lines(x, y, col='aquamarine', lty=1, lwd=2)

# polynomial naive model
# df_leq01 <- df[df$strain < 0.1,]
model <- lm(stress~poly(strain, 3), data=df)
y <- predict(object<-model, newdata<-data.frame(strain<-x))
lines(x, y, col='gold', lty=1, lwd=2)


legend(x<-0.02, y<-50, legend<-c('naive, linear', 'strain<0.1, linear', 'polynomial'), lty=1, col=c('red', 'aquamarine', 'gold'))

