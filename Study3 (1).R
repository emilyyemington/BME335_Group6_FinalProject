# Import data
library(readr)
neg = read_csv("Desktop/BME335/Final Project/Study3_BirthOutcomeZIKVnegative.csv")
pos = read_csv("Desktop/BME335/Final Project/Study3_BirthOutcomeZIKVpositive.csv")

# for some reason, all of the data in the original sets are doubled, so this halves them
neg = neg[1:(nrow(neg)/2),]
pos = pos[1:(nrow(pos)/2),]

# find slopes and SEs
gestage.neg.headCircum.lm <- lm(headCircumference~gestationalAgeWks, neg)
summary(gestage.neg.headCircum.lm)
gestage.pos.headCircum.lm <- lm(headCircumference~gestationalAgeWks, pos)
summary(gestage.pos.headCircum.lm)

# slopes and SE
posHeadCir = 0.4148
negHeadCir = 0.4893
b=posHeadCir
beta0=negHeadCir
seb=0.0504

# t test
tHeadCirc <- ((b-beta0)/seb)
tHeadCirc
df=125-2
pt(tHeadCirc, df)