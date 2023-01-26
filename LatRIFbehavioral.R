####### Project: Lateralized RIF   #######
####### Behavioral data            #######

#rm(list = ls())

library(psych)
library(dplyr)
library(tidyr)
library(rstatix) # for anova_test
library(ggplot2)

## set working directory
setwd("~/Documents/LatRIF")

## load data
lat.rif.file <- "~/Documents/LatRIF/RIF_Subj_4-27_EprimeData.csv"
lat.rif.table <- read.csv(lat.rif.file, header = TRUE, sep = ";")
## make Subject and Retrieval.round factors
lat.rif.table$Subject <- factor(lat.rif.table$Subject)
lat.rif.table$Retrieval.round <- factor(lat.rif.table$Retrieval.round)

## Check resulting table and data
View(lat.rif.table)
summary(lat.rif.table)
describe(lat.rif.table)

# summary show Encoding side question RT over 1500, check to make sure it is only subject 4
#lat.rif.table$Subject[which(lat.rif.table$EncodeSideQ.RT > 1500)]

## Create subsets for each phase: encoding, retrieval and test
encoding.subset <- subset(lat.rif.table, BlockList == 1)
retrieval.subset <- subset(lat.rif.table, BlockList == 2)
test.subset <- subset(lat.rif.table, BlockList == 4)

#### Check for outliers in the overall memory performance for each phase ####
## Create datasets with mean recall for each subject
enc.meanrec.subj <- encoding.subset %>%
  group_by(Subject) %>%
  summarise(mean_siderec = mean(EncodeSideQ.ACC))
retr.meanrec.subj <- retrieval.subset %>%
  group_by(Subject) %>%
  summarise(mean_wordrec = mean(Retrieval..item.accuracy), mean_siderec = mean(RpSideQ.ACC), mean_siderecofwordrec = (sum(Retrieval..item.and.side.correct)/sum(Retrieval..item.accuracy)))
test.meanrec.subj <- test.subset %>%
  group_by(Subject) %>%
  summarise(mean_wordrec = mean(Test..item.accuracy), mean_siderec = mean(TestSideQ.ACC), mean_siderecofwordrec = (sum(Test..side.and.item.correct)/sum(Test..item.accuracy)))
## boxplots of mean recall
boxplot(enc.meanrec.subj$mean_siderec)
boxplot(retr.meanrec.subj$mean_wordrec)
boxplot(retr.meanrec.subj$mean_siderecofwordrec, xlab = "retrieval", ylab = "correct side out of correct words")
boxplot(test.meanrec.subj$mean_wordrec)
boxplot(test.meanrec.subj$mean_siderecofwordrec)

## Find subject number of outliers
#out_retr_sideword <- boxplot(retr.meanrec.subj$mean_siderecofwordrec)$out
#retr.meanrec.subj$Subject[which(retr.meanrec.subj$mean_siderecofwordrec %in% out_retr_sideword)]
#out_enc_side <- boxplot(enc.meanrec.subj$mean_siderec)$out
#enc.meanrec.subj$Subject[which(enc.meanrec.subj$mean_siderec %in% out_enc_side)]

######################################################  
#### Selective retrieval phase ####
#### Overall memory performance ####
## Mean recall of word and corrrectly recalled side out of correctly recalled words,in percent.
colidx = which(colnames(retr.meanrec.subj) %in% c("mean_wordrec", "mean_siderecofwordrec"))
round(sapply(retr.meanrec.subj[colidx], FUN = mean) * 100, 2)
## sd
round(sapply(retr.meanrec.subj[colidx], FUN = sd) * 100, 2)
rm(colidx)

#### Recall performance over the three retrieval rounds ####
## Data set with mean of each retreival round as variables
retr.meanrec.rounds <- retrieval.subset %>%
  group_by(Subject, Retrieval.round) %>%
  summarise(mean_wordrec = mean(Retrieval..item.accuracy), mean_side = mean(RpSideQ.ACC), mean_siderecofwordrec = (mean(Retrieval..item.and.side.correct)/mean(Retrieval..item.accuracy)))
retr.meanrec.rounds <- retr.meanrec.rounds %>%
  pivot_wider(names_from = Retrieval.round, values_from = c(mean_wordrec, mean_side, mean_siderecofwordrec))
  
## Mean recall of word and corrrectly recalled side out of correctly recalled words for each retrieval round
round(sapply(retr.meanrec.rounds[-1], FUN = mean) * 100, 2)
## sd
round(sapply(retr.meanrec.rounds[-1], FUN = sd) * 100, 2)

## Mean RT for correctly recalled word and side perhaps?

## Compare recall over retrieval rounds with ANOVA
## Check assumptions
boxplot(retr.meanrec.rounds$mean_wordrec_1)
boxplot(retr.meanrec.rounds$mean_wordrec_2)
boxplot(retr.meanrec.rounds$mean_wordrec_3)
shapiro.test(retr.meanrec.rounds$mean_wordrec_1)
shapiro.test(retr.meanrec.rounds$mean_wordrec_2)
shapiro.test(retr.meanrec.rounds$mean_wordrec_3)
## long format
long.retr.meanrec.rounds <- retrieval.subset %>%
  group_by(Retrieval.round, Subject) %>%
  summarise(mean_wordrec = mean(Retrieval..item.accuracy))
long.retr.meanrec.rounds <- data.frame(long.retr.meanrec.rounds)
#Repeated ANOVA using anova_test in rstatix, gives sphericity test as well
retr.rounds.aov <- anova_test(data = long.retr.meanrec.rounds, dv = mean_wordrec, wid = Subject, within = Retrieval.round)
retr.rounds.aov
get_anova_table(retr.rounds.aov)
# post hoc, holm corrected
post.hoc.rounds <- long.retr.meanrec.rounds %>% pairwise_t_test(mean_wordrec ~ Retrieval.round, paired = TRUE, p.adjust.method = "holm")
post.hoc.rounds

######################################################
#### Test phase ####
## Mean recall of word and correctly recalled side out of correctly recalled words in percent
colidx = which(colnames(test.meanrec.subj) %in% c("mean_wordrec", "mean_siderecofwordrec"))
round(sapply(test.meanrec.subj[colidx], FUN = mean) * 100, 2)
## sd
round(sapply(test.meanrec.subj[colidx], FUN = sd) * 100, 2)
rm(colidx)

#### Mean recall performance for each item ####
## Dataset with mean recall for each item
test.meanrec.items <- test.subset %>%
  group_by(Subject, Item) %>%
  summarise(mean_wordrec = mean(Test..item.accuracy), mean_side = mean(TestSideQ.ACC), mean_siderecofwordrec = (mean(Test..side.and.item.correct)/mean(Test..item.accuracy)))
test.meanrec.items <- test.meanrec.items %>%
  pivot_wider(names_from = Item, values_from = c(mean_wordrec, mean_side, mean_siderecofwordrec))
## Add a variable for RIF index, (NrpHigh - RpMinus)/NrpHigh
for (i in 1:length(test.meanrec.items$Subject)) {
  test.meanrec.items$RIFindex[i] <- ((test.meanrec.items$mean_wordrec_NrpHigh[i] - test.meanrec.items$mean_wordrec_RpMinus[i])/test.meanrec.items$mean_wordrec_NrpHigh[i])
}
rm(i)

## Outliers in word recall for each item
boxplot(test.meanrec.items$mean_wordrec_NrpHigh, xlab = "Test: Nrp high")
boxplot(test.meanrec.items$mean_wordrec_NrpLow)
boxplot(test.meanrec.items$mean_wordrec_RpMinus)
boxplot(test.meanrec.items$mean_wordrec_RpPlus, xlab = "Test: Rp plus")
## Find subject number of outliers
out_NrpHigh <- boxplot(test.meanrec.items$mean_wordrec_NrpHigh)$out
test.meanrec.items$Subject[which(test.meanrec.items$mean_wordrec_NrpHigh %in% out_NrpHigh)]
out_RpPLus <- boxplot(test.meanrec.items$mean_wordrec_RpPlus)$out
test.meanrec.items$Subject[which(test.meanrec.items$mean_wordrec_RpPlus %in% out_RpPLus)]

## Mean recall of word and correctly recalled side out of correctly recalled words for each item
round(sapply(test.meanrec.items[-1], FUN = mean) * 100, 2)
## sd
round(sapply(test.meanrec.items[-1], FUN = sd) * 100, 2)

## Plot word recall for each item
## long format
long.item <- test.subset %>%
  group_by(Item, Subject) %>%
  summarise(mean_wordrec = mean(Test..item.accuracy),  mean_siderecofwordrec = (mean(Test..side.and.item.correct)/mean(Test..item.accuracy)))
long.item <- data.frame(long.item)
## histogram with ggplot
ggplot(long.item, aes(x=factor(Item), y=mean_wordrec)) + stat_summary(fun="mean", geom="bar") + xlab("Items") + ylab("word recall")

#### Paired t tests ####
## datasets for t test
long.nrphigh.rpmin <- subset(long.item, Item == "NrpHigh"| Item == "RpMinus")
long.nrplow.rpplus <- subset(long.item, Item == "NrpLow" | Item == "RpPlus")
long.nrplow.nrphigh <- subset(long.item, Item == "NrpLow" | Item == "NrpHigh")

## NrpLow vs. RpPlus, testing effect
## Check normality of diff
diff <- with(long.nrplow.rpplus, mean_wordrec[Item == "NrpLow"] - mean_wordrec[Item == "RpPlus"])
shapiro.test(diff)
## t test
t.test(mean_wordrec ~ Item, data = long.nrplow.rpplus, paired = TRUE)

## NrpHigh vs. NrpLow, manipulation of high/low accessibility in memory
diff <- with(long.nrplow.nrphigh, mean_wordrec[Item == "NrpLow"] - mean_wordrec[Item == "NrpHigh"])
shapiro.test(diff)
## t test
t.test(mean_wordrec ~ Item, data = long.nrplow.nrphigh, paired = TRUE)

#### RIF effect ####
## NrpHigh vs. RpMinus
## Word recall
## Check normality of diff
diff <- with(long.nrphigh.rpmin, mean_wordrec[Item == "NrpHigh"] - mean_wordrec[Item == "RpMinus"])
shapiro.test(diff)
## t test
t.test(mean_wordrec ~ Item, data = long.nrphigh.rpmin, paired = TRUE)

## Compare correct side out of correct words
## Check normality of diff
diff <- with(long.nrphigh.rpmin, mean_siderecofwordrec[Item == "NrpHigh"] - mean_siderecofwordrec[Item == "RpMinus"])
shapiro.test(diff)
## t test
# t.test(mean_wordrec ~ Item, data = long.nrphigh.rpmin, paired = TRUE)
wilcox.test(mean_siderecofwordrec ~ Item, data = long.nrphigh.rpmin, paired = TRUE)

## Did the presentation side affect recall performance? ####
long.presentation.side <- test.subset %>%
  group_by(Correct.SubTrial., Subject) %>%
  summarise(mean_wordrec = mean(Test..item.accuracy),  mean_siderecofwordrec = (mean(Test..side.and.item.correct)/mean(Test..item.accuracy)), mean_sidrec = mean(TestSideQ.ACC))
long.presentation.side <- data.frame(long.presentation.side)
## Word recall
## Check normality of diff
diff <- with(long.presentation.side, mean_wordrec[Correct.SubTrial. == "{LEFTARROW}"] - mean_wordrec[Correct.SubTrial. == "{RIGHTARROW}"])
shapiro.test(diff)
## t test
t.test(mean_wordrec ~ Correct.SubTrial., data = long.presentation.side, paired = TRUE)
## Correctly recalled side out of correctly recalled words.
## Check normality of diff
diff <- with(long.presentation.side, mean_siderecofwordrec[Correct.SubTrial. == "{LEFTARROW}"] - mean_siderecofwordrec[Correct.SubTrial. == "{RIGHTARROW}"])
shapiro.test(diff)
## t test
t.test(mean_siderecofwordrec ~ Correct.SubTrial., data = long.presentation.side, paired = TRUE)
rm(diff)
## Get mean recall for left/right, correctly recalled side out of correctly recalled words.
long.presentation.side %>%
  group_by(Correct.SubTrial.) %>%
  summarise(mean(mean_siderecofwordrec) * 100)

#################################
#### Output interference ####
## Does presentation order at test influence memory performance?
## add a variable for presentation order
subjvar <- unique(test.subset$Subject)
blockvar <- unique(test.subset$BlockCounter)
# keep track of subjects, blocks and order
nr <- 1
subji <- 1
blocki <- 1
for (rowsi in 1: length(test.subset$Subject)) {
  if (test.subset$Subject[rowsi] == subjvar[subji]) {
    if (test.subset$BlockCounter[rowsi] == blockvar[blocki]) {
      test.subset$test_order[rowsi] <- nr
      nr <- nr + 1
    } else {
      blocki <- blocki + 1
      nr <- 1
      test.subset$test_order[rowsi] <- nr
      nr <- nr + 1
    }
  } else {
    subji <- subji + 1
    blocki <- 1
    nr <- 1
    test.subset$test_order[rowsi] <- nr
    nr <- nr +1
  }
}
rm(subjvar,blockvar, subji, blocki, rowsi, nr)
  
## Get mean word recall for each item at each presentation order 
test.order.item <- test.subset %>%
  group_by(Item, test_order) %>%
  summarise(mean_wordrec = mean(Test..item.accuracy), n = n())


