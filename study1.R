# Emily Yemington, Group 6, BME335

library(readr)
neg = read_csv("Desktop/BME335/Final Project/Study1_UltrasoundZIKVnegative.csv")
pos = read_csv("Desktop/BME335/Final Project/Study1_UltrasoundZIKVpositive.csv")
View(neg)

#combine negative and positive data 

zika_data = data.frame(zika=c(rep("negative", nrow(neg)),rep("positive", nrow(pos))),
                weeks = c(neg$gestationalAgeWks, pos$gestationalAgeWks),
                ultra = c(neg$ultrasoundFinding, pos$ultrasoundFinding)
                )
View(zika_data)

#plot
library(ggplot2)
library(gridExtra)

# plot with all diagnoses (not final plot)

ggplot(zika_data, aes(x=ultra, y=weeks)) + geom_jitter(aes(col=zika)) + labs(x="Pregnancy Outcome", y="Time of Diagnosis (weeks)", title="Pregnancy Outcome and Zika Status", colour="Zika status")

#refactor as normal / abnormal instead of all diagnoses, + new plot

zika_data["ultra"][zika_data["ultra"]!="normal"] = "abnormal"
ggplot(zika_data, aes(x=ultra, y=weeks)) + geom_jitter(aes(col=zika)) + labs(x="Pregnancy Outcome", y="Time of Diagnosis (weeks)", title="Pregnancy Outcome and Zika Status", colour="Zika status")

# Chi squared per half:

half1_data = subset(zika_data, weeks<20, select=c(zika, ultra))
half2_data = subset(zika_data, weeks>=20, select=c(zika, ultra))

for (half_data in list(half1_data,half2_data)){
  num_pos_ab = nrow(subset(half_data, zika=="positive" & ultra =="abnormal", select=c(zika, ultra)))
  num_neg_ab = nrow(subset(half_data, zika=="negative" & ultra =="abnormal", select=c(zika, ultra)))
  num_pos_norm = nrow(subset(half_data, zika=="positive" & ultra =="normal", select=c(zika, ultra)))
  num_neg_norm = nrow(subset(half_data, zika=="negative" & ultra =="normal", select=c(zika, ultra)))
  
  cont_table = matrix(c(num_pos_norm,num_neg_norm, num_pos_ab, num_neg_ab), nrow=2, ncol=2, byrow=T)
  rownames(cont_table) = c("Normal", "Abnormal")
  colnames(cont_table) = c("Positive", "Negative")
  print(cont_table)
  
  print(chisq.test(cont_table))
}


