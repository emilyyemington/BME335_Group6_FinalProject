library(readr)
apgar <- read_csv("~/UT Documents/Sophomore Fall/Statistics/Final Project/Study4_ApgarScores.csv")

# restructure data to sort by times

zika_df = data.frame(zika=apgar$babyStatus, score = (apgar$oneMin + apgar$fiveMin + apgar$tenMin)/3)

#create separate data frames for exposed and unexposed babies
split_zika_df = split(zika_df, zika_df$zika)
exposed_df = data.frame(score=split_zika_df$`ZIKV-exposed`$score)
unexposed_df = data.frame(score=split_zika_df$`ZIKV-unexposed`$score)

#plot
library(ggplot2)
library(gridExtra)

#plot our visualization data
exposed_plot = ggplot(exposed_df, aes(x=score)) + geom_histogram(bins=25) +xlim(c(0,10)) + 
  labs(x="Score", y="Frequency", title="Apgar Scores for ZIKV Exposed Children")

unexposed_plot = ggplot(unexposed_df, aes(x=score)) + geom_histogram(bins=25) +xlim(c(0,10)) + 
  labs(x="Score", y="Frequency", title="Apgar Scores for Unexposed Children")

grid.arrange(exposed_plot, unexposed_plot, nrow=1, ncol = 2)

#measure the difference in variance
exposed_var = var(exposed_df)
unexposed_var = var(unexposed_df)
variance_difference = exposed_var / unexposed_var

#print output
paste0("Exposed Variance: " , exposed_var)
paste0("Unexposed Variance: " , unexposed_var)
paste0("Variance Difference: " , variance_difference , " fold")

# calculate the W and p-value of the data using the Mann-Whitney U Test
welch_test = t.test(exposed_df, unexposed_df)
welch_test
