# importing datasets. Change file path as needed.

library(readr)
neg = read_csv("Study2_UltrasoundZIKVnegative.csv")
pos = read_csv("Study2_UltrasoundZIKVpositive.csv")
View(neg)



# set up data frame

baby_size = data.frame(zika=c(rep("negative", nrow(neg)),rep("positive", nrow(pos))),
                       weeks = c(neg$gestationalAgeWks, pos$gestationalAgeWks),
                       diam = c(neg$biparietalDiameter, pos$biparietalDiameter),
                       head = c(neg$headCircumference, pos$headCircumference),
                       weight = c(neg$estimatedFetalWeight, pos$estimatedFetalWeight))

# overall plot
library(ggplot2)
library(gridExtra)

weight_plot = ggplot(baby_size, aes(x=weeks, y=weight)) + geom_point(aes(col=zika)) + theme(legend.position = "none")
diam_plot = ggplot(baby_size, aes(x=weeks, y=diam)) + geom_point(aes(col=zika)) + theme(legend.position = "none")
head_plot =ggplot(baby_size, aes(x=weeks, y=head)) + geom_point(aes(col=zika))

grid.arrange(weight_plot, diam_plot, head_plot, nrow=2)

#distribution plots

weight_dist = ggplot(baby_size, aes(x=weight)) + geom_histogram()
diam_dist = ggplot(baby_size, aes(x=diam)) + geom_histogram()
head_dist = ggplot(baby_size, aes(x=head)) + geom_histogram()

grid.arrange(weight_dist, diam_dist, head_dist, nrow=2)

#ancova computation 
ancova<-lm(diam_dist~weeks*weeks, data=baby_size)
ancova<-lm(count~diam*weeks, data=baby_size)


diam.model <- lm(diam ~ weeks + zika + weeks:zika, data = baby_size)
#residuals vs fitted
plot(diam.model, add.smooth = FALSE, which = 1)
#normal distribution and consider amount of variance 
plot(diam.model, which = 2)
#scale
plot(diam.model, add.smooth = FALSE, which = 3)
#run test 
anova(diam.model)



