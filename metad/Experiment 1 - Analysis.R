# Load Packages ------------------------------------------------------
library(ggplot2)
library(effsize)
library(afex)
library(emmeans)
library(cowplot)
library(dplyr)
library(stringr)
library(tidyr)
library(naniar)
library(tidyverse)
library(metaSDT)
library(sjPlot)
devtools::source_url("https://github.com/kitdouble/grab.data/blob/main/grab.data.R?raw=TRUE")

# Load Data ------------------------------------------------------
suppressMessages(mydata <- download_osf_data("24efc", add_id = F))
mydata <- subset(mydata, trial_type == "Summary Trial")

mydata$rt <- as.numeric(mydata$rt)

#------------------------------------------------------
# DOT DISCRIMINATION TASK ----
#------------------------------------------------------

metad <- subset(mydata, stimdevi != "")

# Descriptive Statistics
length(unique(metad$participant_id)) # n
 metad %>%
  summarise(across(c(confidence, correct, rt), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))))

 # Outliers
out <-  metad %>%
   group_by(participant_id) %>% 
   summarise(across(c(confidence, correct, rt), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))))

metad <- metad[!metad$participant_id %in% out[out$correct_mean < 0.5 | out$confidence_sd ==0,"participant_id"],]
length(unique(metad$participant_id)) # n


# Select Columns
metad <- subset(metad, trial_type == "Summary Trial" & feedback == "False", select = c("participant_id", "rt", "response",  
                                                                                         "trialnum", "blocknum", "stimdevi", "correct", "target_left", "confidence"))

# Calculate Meta D'
metadsum <- matrix(NA, nrow = length(unique(metad$participant_id)), ncol = 4)
for(i in 1:length(unique(metad$participant_id))){
  
  ID = unique(metad$participant_id)[i]
  print(ID)
  df <- metad[metad$participant_id == ID,]
  
  # Meta D
  (y <- table(factor(df$confidence, levels = 1:6), df$response, df$target_left))
  nr_s1 <- c(rev(y[,1,1]),y[,2,1])
  nr_s2 <- c(rev(y[,1,2]),y[,2,2])
  
  
  # Adjustment
  # adj_f = 1/length(nr_s1);
  # nr_s1 = nr_s1 + adj_f;
  # nr_s2 = nr_s2 + adj_f;
  
  fit_MLE <- fit_meta_d_SSE(nr_s1,nr_s2, add_constant = T)
  metadsum[i,] <- c(ID, fit_MLE$M_ratio[1], fit_MLE$meta_da[1], fit_MLE$da[1])
}

metadsum <- as.data.frame(metadsum)
colnames(metadsum) <- c("participant_id", "M_ratio", "meta_da", "da")
metadsum$M_ratio <- as.numeric(metadsum$M_ratio)
metadsum$meta_da <- as.numeric(metadsum$meta_da)
metadsum$da <- as.numeric(metadsum$da)


#------------------------------------------------------
# RAVENS TASK ----
#------------------------------------------------------

rpm <- subset(mydata, stimdevi == "")

# Descriptive Statistics
length(unique(rpm$participant_id)) # n
rpm %>%
  summarise(across(c(confidence, correct, rt), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))))

# Outliers
out <-  rpm %>%
  group_by(participant_id) %>% 
  summarise(across(c(confidence, correct, rt), list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))))

rpm <- rpm[!rpm$participant_id %in% out[out$correct_mean < 0.125,"participant_id"],]
length(unique(rpm$participant_id)) # n

# Agrregate
rpm <-  rpm %>%
  group_by(participant_id, condition) %>% 
  summarise(across(c(correct), list(mean = ~ mean(.x, na.rm = TRUE))))


#------------------------------------------------------
# ANALYSIS ----
#------------------------------------------------------

# Merge
mydata <- merge(metadsum, rpm, by = "participant_id", all = T)

# T-test
aggregate(correct_mean ~ condition, mydata, mean)
t.test(correct_mean ~ condition, mydata, var.eqal = T)

# Regression
summary(mod1 <- lm(correct_mean ~ condition*M_ratio, mydata))

# Plot
plot_model(mod1, "int")

