data(mtcars)
data = mtcars
data$am[data$am==0] <- "Automatic"
data$am[data$am==1] <- "Manual"
data$am <- as.factor(data$am)
str(data)

t.test(data$mpg ~ data$am)

rmodel1 = lm(mpg ~ am,data)
summary(rmodel1)

rmodel2 = lm(mpg ~ am + wt + qsec,data)
summary(rmodel2)

anova(rmodel1,rmodel2)

library(ggplot2)
library(reshape2)
corheatmap = round(cor(mtcars),2)
corheatmap[lower.tri(corheatmap)]<- NA
melted <- melt(corheatmap)
melted <- na.omit(melted)


ggplot(data = melted, aes(Var2, Var1, fill = value))+
  ggtitle("Correlation Heatmap")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", 
  high = "red", mid = "white",
  midpoint = 0, limit = c(-1,1), name="Correlation")+
  theme_minimal()+coord_fixed()

par(mfrow=c(2,2))
plot(rmodel2)