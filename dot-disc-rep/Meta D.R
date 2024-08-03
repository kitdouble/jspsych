########################################################################################
########################################################################################
# A script to pre-process Meta D data from jsPsych

########################################################################################
########################################################################################

# Load packages ----
library(tidyr)
library(dplyr)
library(metaSDT)
devtools::source_url("https://github.com/kitdouble/grab.data/blob/main/grab.data.R?raw=TRUE")


# Load data ----
mydata <- grab.data("~/Downloads/osfstorage-archive-4/")

# Select Columns
mydata <- subset(mydata, trial_type == "Summary Trial" & feedback == "False", select = c("participant_id", "rt", "response",  
                                                                                         "trialnum", "blocknum", "stimdevi", "correct", "target_left", "confidence"))
# Remove those with < 55% of performance
cor <- aggregate(correct ~ participant_id, mydata, mean)
cor <- subset(cor, correct < 0.55)
mydata <- mydata[!mydata$participant_id %in% cor$participant_id,]


# Remove those with no variance in confidence
var_conf <- aggregate(confidence ~ participant_id, mydata, sd)
var_conf <- subset(var_conf, confidence == 0)
mydata <- mydata[!mydata$participant_id %in% var_conf$participant_id,]

metad <- matrix(NA, nrow = length(unique(mydata$participant_id)), ncol = 4)



for(i in 1:length(unique(mydata$participant_id))){
  
  ID = unique(mydata$participant_id)[i]
  print(ID)
  df <- mydata[mydata$participant_id == ID,]
  
  # Meta D
  (y <- table(factor(df$confidence, levels = 1:6), df$response, df$target_left))
  nr_s1 <- c(rev(y[,1,1]),y[,2,1])
  nr_s2 <- c(rev(y[,1,2]),y[,2,2])
  
  
  # Adjustment
  # adj_f = 1/length(nr_s1);
  # nr_s1 = nr_s1 + adj_f;
  # nr_s2 = nr_s2 + adj_f;
  
  fit_MLE <- fit_meta_d_SSE(nr_s1,nr_s2, add_constant = T)
  metad[i,] <- c(ID, fit_MLE$M_ratio[1], fit_MLE$meta_da[1], fit_MLE$da[1])
}

metad <- as.data.frame(metad)
colnames(metad) <- c("participant_id", "M_ratio", "meta_da", "da")
aggdata <- metad
aggdata$M_ratio <- as.numeric(aggdata$M_ratio)
aggdata$meta_da <- as.numeric(aggdata$meta_da)
aggdata$da <- as.numeric(aggdata$da)

# Add mean accuracy
aggacc <- aggregate(correct ~ participant_id, mydata, mean)
aggdata <- merge(aggdata, aggacc, by = "participant_id")


# Add mean confidence
conf <- aggregate(confidence ~ participant_id, mydata, mean)
aggdata <- merge(aggdata, conf, by = "participant_id")


# Remove Outliers

aggdata <- subset(aggdata, M_ratio > 0 )
aggdata <- subset(aggdata, da > 0 )

# Save to File
write.csv(aggdata, "~/Desktop/metad.csv", row.names = F, na = "")




