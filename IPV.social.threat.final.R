library(reader)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(haven)
library(lmtest)
library(olsrr)
library(interactions)
library(ggplot2)
library(apaTables)
library(psych)
library(ggsignif)
library(sandwich)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
savepath<-c("./_images/")

#### Eye Tracking Data Preparation ####

#pull in fixation dataset
all.fixation<-read.csv("datasets/complete.fixation.dataset.csv",header=T)
length(unique(all.fixation$RECORDING_SESSION_LABEL)) #216 participants have fixation data

##remove trials of the fixation cross (all of the odd numbered trials)
remove.cross.trials <- all.fixation %>%
  filter(TRIAL_INDEX %% 2 == 0)
length(unique(remove.cross.trials$RECORDING_SESSION_LABEL)) #215 participants

#examining why one participant was dropped after removing fixation cross trials
all_ids <- unique(all.fixation$RECORDING_SESSION_LABEL) #216
filtered_ids <- unique(remove.cross.trials$RECORDING_SESSION_LABEL) #215
missing_id <- setdiff(all_ids, filtered_ids)
missing_id # participant 1293 only had one recorded trial which was a fixation trial

#renumber trials after removing fixation trials and rename participant ID variable in fixation dataset
fixation.final <- remove.cross.trials %>%
  group_by(RECORDING_SESSION_LABEL) %>%
  mutate(New_Trial = as.integer(factor(TRIAL_INDEX, 
                                       levels = sort(unique(TRIAL_INDEX))))) %>%
  ungroup() %>% relocate(New_Trial, .after = TRIAL_INDEX) %>%
  rename(
  id = RECORDING_SESSION_LABEL)
rm(remove.cross.trials) #remove datasets no longer needed

#remove trials with blinks immediately before and after fixations
fixation.no.blinks <- fixation.final %>%
  filter(CURRENT_FIX_BLINK_AROUND == "NONE")

length(unique(fixation.no.blinks$id)) #213 participants (2 participants lost at this step)

#### Merge Fixation Dataset with Stimuli Dataset ####
stimuli.order<-read.csv("datasets/stimuliorder.csv",header=T) #pulling in stimuli dataset (extracted from E-Prime)

#merge stimuli dataset with fixation dataset
fixation.stimuli <- fixation.no.blinks %>%
  left_join(stimuli.order, by = "New_Trial")

#checking that datasets merged correctly
  names(fixation.stimuli)
  table(fixation.stimuli$AngryLocation, useNA = "ifany")
rm(stimuli.order, fixation.no.blinks) #removing datasets no longer needed
  
#### Merge Demographics and Visit Datasets with Fixation Dataset####
visit<-read.csv("datasets/visit.csv",header=T) #pulling in visit dataset

#merge visit number with fixation dataset
fixation.stimuli <- fixation.stimuli %>%
  left_join(visit, by = "id")
  
#check to make sure merge is correct
names(fixation.stimuli)
table(fixation.stimuli$visit, useNA = "ifany")
sum(is.na(fixation.stimuli$visit))
fixation.stimuli %>%
    filter(is.na(visit)) %>%
    distinct(id)

#calculate age at visit 3 ONLY for participants that completed eye tracking during visit 3
V3<-read.csv("datasets/V3.csv",header=T) #pull in visit 3 data
DOB<-read.csv("datasets/DOB.csv",header=T) #pull in date of birth dataset of all participants

#merging date of birth dataset with list of participants who completed eye tracking during visit 3
V3 <- V3 %>%
  left_join(DOB, by = "id")

#get rid of NAs (empty rows) in merged dataset
V3 <- V3 %>%
  filter(!if_all(everything(), ~ .x == "" | is.na(.x)))

#fix format of dates
V3 <- V3 %>%
  mutate(
    DOB = mdy(DOB),
    mri.date = mdy(mri.date))

#create an age variable using date of birth and date of eye tracking visit
V3 <- V3 %>%
  mutate(age = floor(time_length(interval(DOB, mri.date), "years")))

V1<-read.csv("datasets/V1.csv",header=T) #pull in visit 1 data
V1.age<-read.csv("datasets/V1_age.csv",header=T) #pull in dataset specifying participants' ages at vist 1

#get rid of NAs (empty rows) in visit 1 dataset
V1 <- V1 %>%
  filter(!if_all(everything(), ~ .x == "" | is.na(.x)))

#merge age into visit 1 data matching only the ids of participants who did eye tracking at visit 1
V1 <- V1 %>%
  left_join(V1.age, by = "id")

V3$age <- as.integer(V3$age) #change age to integer for correct merging

#combine V1 and V3 age datasets to create age dataset for all participants who did eye tracking
combined.age <- bind_rows(
  V1 %>% select(id, age, visit),
  V3 %>% select(id, age, visit))
length(unique(combined.age$id)) #216 participants (correctly matches initial number)

#combine age and demographic datasets
demo<-read.csv("datasets/demo.csv",header=T) #pull in demographics dataset
full.demo <- combined.age %>% #combine demographic dataset with age dataset
  left_join(demo, by = "id")

full.demo$visit <- NULL #remove visit column from dataset (no longer needed)
rm(V1, V3, V1.age, visit, combined.age, DOB, demo) #remove extra datasets

length(unique(full.demo$id)) #216 participants (correctly matches initial number)

#combine demographics dataset with fixation dataset
fixation.demo <- fixation.stimuli %>%
  left_join(full.demo, by = "id")

length(unique(fixation.demo$id)) #213 participants (correctly matches number in line 50)

#### ET Data Pre-Processing Pipeline ####
#creating new dataset with fixations less than 80 ms removed
table(fixation.demo$CURRENT_FIX_DURATION < 80) #36822 fixations are less than 100 ms
fixation.demo.rm <- fixation.demo %>%
  filter(CURRENT_FIX_DURATION >= 80) #dataset with fixations less than 100 ms removed

#view overall fixation counts across participants in a separate dataset
fixation.counts <- fixation.demo.rm %>%
  group_by(id) %>%
  summarise(n_fixations = n()) #fixation counts range from 22 to 1227 for 213 participants

#create data subset with only Angry-Neutral Trials
fixation.demo.condition <- fixation.demo.rm %>%
  filter(AngryLocation %in% c("R", "L"))
n_distinct(fixation.demo.condition$id) #213 participants, none lost

#create subset of trials with fixations on faces ONLY
fixation.demo.face <- fixation.demo.condition %>%
  filter(CURRENT_FIX_INTEREST_AREA_LABEL %in% c("RightFace", "LeftFace"))

#view fixation counts for fixations on faces in A-N Condition in a separate dataset
fixation.demo.count <- fixation.demo.face %>%
  group_by(id) %>%
  summarise(n_fixations = n()) #counts range from 1 to 367 for 208 participants (5 participants lost)

#histogram of final fixation counts by participant across entire experiment
ggplot(data = fixation.demo.count, aes(x = n_fixations)) + 
  geom_histogram(binwidth = 15, color = "lightblue", fill = "lightblue")

#create a new variable that indicates whether fixations were on Angry or Neutral faces
fixation.demo.face <- fixation.demo.face %>%
  mutate(fixation.emotion = case_when(
    CURRENT_FIX_INTEREST_AREA_LABEL == "RightFace" & AngryLocation == "R" ~ "Angry",
    CURRENT_FIX_INTEREST_AREA_LABEL == "LeftFace"  & AngryLocation == "L" ~ "Angry",
    CURRENT_FIX_INTEREST_AREA_LABEL == "RightFace" & AngryLocation == "L" ~ "Neutral",
    CURRENT_FIX_INTEREST_AREA_LABEL == "LeftFace"  & AngryLocation == "R" ~ "Neutral",
    TRUE ~ NA_character_)) 
#if CURRENT_FIX_INTEREST_AREA_LABEL matches AngryLocation, it's a fixation on an angry face
#if CURRENT_FIX_INTEREST_AREA_LABEL does not match AngryLocation, it's a fixation on a neutral face

#checking for errors when creating new variable fixation.emotion
table(fixation.demo.face$CURRENT_FIX_INTEREST_AREA_LABEL,
      fixation.demo.face$AngryLocation,
      fixation.demo.face$fixation.emotion,  useNA = "ifany") #table showing overlap
fixation.demo.face %>%
  select(CURRENT_FIX_INTEREST_AREA_LABEL, AngryLocation, fixation.emotion) %>%
  head(20) #show the first 20 rows for the three variables

#calculate how many angry-neutral trials have high-quality fixations on face vs. trials excluded
fixation.calculate.missing <- fixation.demo.face %>%
  distinct(id, New_Trial) %>%
  group_by(id) %>%
  summarise(n.angry.trials = n(),
            n.excludedtrials = 68 - n.angry.trials,
            percent.excluded = (68 - n.angry.trials) / 68 * 100)
mean(fixation.calculate.missing$n.angry.trials)

#histogram of trials with face fixations in A-N condition
ggplot(data = fixation.calculate.missing, aes(x = n.angry.trials)) + 
  geom_histogram(binwidth = 5, color = "lightblue", fill = "lightblue")

#create data subset of participants with at least 30 angry-neutral trials
filtered.data <- fixation.demo.face %>%
  inner_join(fixation.calculate.missing, by = "id") %>%
  filter(n.angry.trials >= 30)
n_distinct(filtered.data$id) #139 participants with at least 30 angry-neutral trials

#create data subset of participants with at least 30 angry-neutral trials
excluded.data <- fixation.demo.face %>%
  inner_join(fixation.calculate.missing, by = "id") %>%
  filter(n.angry.trials < 30)
n_distinct(excluded.data$id) #69 participants excluded for having fewer than 30 high-quality angry-neutral trials

#average number of trials for included participants
average.trial.kept <- fixation.calculate.missing %>%
  filter(n.angry.trials >= 30)
mean(fixation.calculate.missing$n.angry.trials) #38.56

rm(fixation.counts, fixation.demo, fixation.demo.count, fixation.demo.condition,
   fixation.demo.face, fixation.demo.rm, fixation.final, fixation.stimuli) #removing datasets no longer needed

#### Create IPV exposure composite variable ####
trauma <- read_sav("datasets/LV1_UCLA_CGandY_v09_current.sav") #pull in raw trauma dataset
trauma$id <- as.integer(trauma$id) #change id to an integer for later merging
trauma.sub <- trauma %>% select(id, "count_TE_vw_or",
"ucla_cv_cg",  "ucla_cv_ch", "ucla_dv_cg", "ucla_dv_ch",
"ucla_pas_cg", "ucla_pas_ch", "ucla_sab_cg", "ucla_sab_ch",
"ucla_pab_cg", "ucla_pab_ch", "ucla_neg_cg", "ucla_neg_ch", "ucla_eab_cg", "ucla_eab_ch",
"ucla_sas_cg", "ucla_sas_ch", "ucla_bully_cg", "ucla_bully_ch", "ucla_abd_cg", "ucla_abd_ch",
"ucla_ter_cg", "ucla_ter_ch", "ucla_war_cg", "ucla_war_ch", "ucla_expl_cg", "ucla_expl_ch") #create a smaller subset of study relevant variables

#pull in visit data again for participants that met ET quality standards
visit<-read.csv("datasets/visit.csv",header=T) #pulling in visit dataset again
clean.visit <- fixation.calculate.missing %>%
  left_join(visit, by = "id") %>%
  filter(n.angry.trials >= 30) #only keeping participants with quality data

#merge with trauma dataset with visit data to determine percentage of participants who completed eyetracking at visit 1 vs 3
visit.trauma <- clean.visit %>% left_join(trauma.sub, by = "id")
#if NA (missing) for child and parent trauma report, remove from dataset (total missing = 12 participants)
visit.trauma <- visit.trauma %>%
  filter(!is.na(count_TE_vw_or)) #127 participants with eye tracking and trauma data
#calculate how many teens completed eye tracking in Visit 1 vs 3
table(visit.trauma$visit)
prop.table(table(visit.trauma$visit)) #Visit 1 = 51 teens (40%), Visit 3 = 76 teens (60%)

#calculate demographics for eye tracking and sample
demo.trauma <- visit.trauma %>% left_join(full.demo, by = "id")
prop.table(table(demo.trauma$gender)) #43% female, 57% male
table(demo.trauma$race) #61% White/Caucasian, 27% Black/African American, 8% multiracial, 3% other, 1% Asian, 1% Native American or Alaskan Native
prop.table(table(demo.trauma$ethnicity)) #87% Non-hispanic, 13% Hispanic
demo.trauma %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)) #mean age = 12.9, standard deviation age = 2.80

#calculating endorsement of the following less common events
table(visit.trauma$ucla_abd_cg) #no endorsement - abduction caregiver
table(visit.trauma$ucla_abd_ch) #no endorsement - abduction child
table(visit.trauma$ucla_ter_cg) #no endorsement - terrorism caregiver
table(visit.trauma$ucla_ter_ch) #no endorsement - terrorism child
table(visit.trauma$ucla_war_cg) #no endorsement - war caregiver
table(visit.trauma$ucla_war_ch) #no endorsement - war child
table(visit.trauma$ucla_expl_cg) #no endorsement - exploitation caregiver
table(visit.trauma$ucla_expl_ch) #no endorsement - exploitation child

#Creating a new composite variable for child OR adult report IPV in remaining events
pairs <- list(
  cv   = c("ucla_cv_cg", "ucla_cv_ch"),
  dv   = c("ucla_dv_cg", "ucla_dv_ch"),
  pas  = c("ucla_pas_cg", "ucla_pas_ch"),
  sab  = c("ucla_sab_cg", "ucla_sab_ch"),
  pab  = c("ucla_pab_cg", "ucla_pab_ch"),
  eab  = c("ucla_eab_cg", "ucla_eab_ch"),
  sas  = c("ucla_sas_cg", "ucla_sas_ch"),
  bully= c("ucla_bully_cg", "ucla_bully_ch"))

vars <- c("cv","dv","pas","sab","pab","eab","sas","bully")

for (v in vars) {
  cg <- paste0("ucla_", v, "_cg")
  ch <- paste0("ucla_", v, "_ch")
  new <- paste0("ucla_", v)
  
  visit.trauma[[new]] <- pmax(visit.trauma[[cg]], visit.trauma[[ch]], na.rm = TRUE)
}

visit.trauma$ipv.composite <- rowSums(visit.trauma[paste0("ucla_", vars)], na.rm = TRUE)
rm(trauma, trauma.sub, pairs, fixation.calculate.missing, clean.visit)

#create a smaller subset with ipv composite for later merging
ipv.sub <- visit.trauma %>% select(id,"visit", "ipv.composite")

#### Merge prosocial behavior variable w/ IPV ####
peq <- read_sav("datasets/PEQ_LV123_v01.sav") #pull in prosocial behaviors variable (PEQ)
#creating a subset of only outcome variables
peq.subset <- peq %>%
  select(id, peq_prosocgive_sum_lv1, peq_prosocgive_sum_lv3, peq_21_lv1, peq_24_lv1, peq_28_lv1, peq_33_lv1, peq_36_lv1,
         peq_21_lv3, peq_24_lv3, peq_28_lv3, peq_33_lv3, peq_36_lv3)
peq.subset$id <- as.integer(peq.subset$id) #change id to integer

#merge prosocial behavior with IPV dataset
ipv.prosocial <- ipv.sub %>% left_join(peq.subset, by = "id")

#making sure it merged correctly
unique.ids <- ipv.prosocial %>%
  summarise(unique.ids = n_distinct(id))
unique.ids #127 (correctly merged)

#create new peq by visit that corresponds to when each participant completed their eye tracking visit
ipv.prosocial <- ipv.prosocial %>%
  mutate(
    peq.by.visit = case_when(
      visit == 1 ~ peq_prosocgive_sum_lv1,
      visit == 3 ~ peq_prosocgive_sum_lv3,
      TRUE ~ NA_real_))
###IMPORTANT: this will drop 3 participants who don't have a PEQ self-report that matches their eye tracking visit

#internal reliability calculation for PEQ measure for corresponding visit
ipv.prosocial <- ipv.prosocial %>%
  mutate(
    peq_21 = case_when(visit == 1 ~ peq_21_lv1, visit == 3 ~ peq_21_lv3, TRUE ~ NA_real_),
    peq_24 = case_when(visit == 1 ~ peq_24_lv1, visit == 3 ~ peq_24_lv3, TRUE ~ NA_real_),
    peq_28 = case_when(visit == 1 ~ peq_28_lv1, visit == 3 ~ peq_28_lv3, TRUE ~ NA_real_), 
    peq_33 = case_when(visit == 1 ~ peq_33_lv1, visit == 3 ~ peq_33_lv3, TRUE ~ NA_real_),
    peq_36 = case_when(visit == 1 ~ peq_36_lv1, visit == 3 ~ peq_36_lv3, TRUE ~ NA_real_))
alpha(ipv.prosocial[, c("peq_21", "peq_24", "peq_28", "peq_33", "peq_36")])

ipv.prosocial <- subset(ipv.prosocial,
                            select = -c(peq_21, peq_24, peq_28, peq_33, peq_36,
                                        peq_prosocgive_sum_lv1, peq_prosocgive_sum_lv3, peq_21_lv1, peq_24_lv1, peq_28_lv1, peq_33_lv1, peq_36_lv1,
                                        peq_21_lv3, peq_24_lv3, peq_28_lv3, peq_33_lv3, peq_36_lv3))

rm(peq, peq.subset) #removing unneeded datasets

#### Merge anxiety variable ####
anx.lv1 <- read_sav("datasets/CHARM_LV1_PROMIS_PA_y_GP.sav") #pull in anxiety data from visit 1
anx.lv1 <- anx.lv1 %>% rename(id = Child_ID) 
anx.lv1$id <- as.integer(anx.lv1$id) 
anx.lv1.sub <- anx.lv1 %>% select(id, PA_T_1, pa_01_1, pa_02_1, pa_03_1, pa_04_1, pa_05_1, pa_06_1, pa_07_1, pa_08_1)

anx.lv3 <- read_sav("datasets/CHARM_LV3_PROMIS_PA_y_GP.sav") #pull in anxiety data from visit 3
anx.lv3 <- anx.lv3 %>% rename(id = Child_ID) 
anx.lv3$id <- as.integer(anx.lv3$id)
anx.lv3.sub <- anx.lv3 %>% select(id, PA_T_3, pa_01_3, pa_02_3, pa_03_3, pa_04_3, pa_05_3, pa_06_3, pa_07_3, pa_08_3)

#merge anxiety variables from visit 1 and visit 3 with IPV dataset
ipv.prosocial.anx <- ipv.prosocial %>% 
                      left_join(anx.lv1.sub, by = "id")  %>%
                      left_join(anx.lv3.sub, by = "id")

#create new anxiety variable by visit that corresponds to when participant completed eye tracking visit
ipv.prosocial.anx <- ipv.prosocial.anx %>%
  mutate(
    anx.by.visit = case_when(
      visit == 1 ~ PA_T_1,
      visit == 3 ~ PA_T_3,
      TRUE ~ NA_real_))
###IMPORTANT: this will drop 1 participant who doesn't have PROMIS anxiety self-report that matches their ET visit

#internal reliability calculation for anxiety measure by visit
ipv.prosocial.anx <- ipv.prosocial.anx %>%
  mutate(
    pa_01 = case_when(visit == 1 ~ pa_01_1, visit == 3 ~ pa_01_3, TRUE ~ NA_real_),
    pa_02 = case_when(visit == 1 ~ pa_02_1, visit == 3 ~ pa_02_3, TRUE ~ NA_real_),
    pa_03 = case_when(visit == 1 ~ pa_03_1, visit == 3 ~ pa_03_3, TRUE ~ NA_real_), 
    pa_04 = case_when(visit == 1 ~ pa_04_1, visit == 3 ~ pa_04_3, TRUE ~ NA_real_),
    pa_05 = case_when(visit == 1 ~ pa_05_1, visit == 3 ~ pa_05_3, TRUE ~ NA_real_),
    pa_06 = case_when(visit == 1 ~ pa_06_1, visit == 3 ~ pa_06_3, TRUE ~ NA_real_),
    pa_07 = case_when(visit == 1 ~ pa_07_1, visit == 3 ~ pa_07_3, TRUE ~ NA_real_),
    pa_08 = case_when(visit == 1 ~ pa_08_1, visit == 3 ~ pa_08_3, TRUE ~ NA_real_))
alpha(ipv.prosocial.anx[, c("pa_01", "pa_02", "pa_03", "pa_04", "pa_05", "pa_06", "pa_07", "pa_08")])

ipv.prosocial.anx <- subset(ipv.prosocial.anx,
                                select = -c(pa_01, pa_02, pa_03, pa_04, pa_05, pa_06, pa_07, pa_08,
                                            PA_T_1, pa_01_1, pa_02_1, pa_03_1, pa_04_1, pa_05_1, pa_06_1, pa_07_1, pa_08_1,
                                            PA_T_3, pa_01_3, pa_02_3, pa_03_3, pa_04_3, pa_05_3, pa_06_3, pa_07_3, pa_08_3))

rm(anx.lv1, anx.lv1.sub, anx.lv3, anx.lv3.sub) #removing unneeded datasets

#### Merge externalizing variable ####
ext <- read_sav("datasets/LV123_BPC_y_cg.sav") #pulling in externalizing variable
ext$id <- as.integer(ext$id) 
ext.sub <- ext %>% select(id, bpc_sum_y_lv1, bpc_sum_y_LV3,
                          bpc_1_lv1, bpc_2_lv1, bpc_3_lv1, bpc_4_lv1, bpc_5_lv1, bpc_6_lv1,
                          bpc_1_lv3, bpc_2_lv3, bpc_3_lv3, bpc_4_lv3, bpc_5_lv3, bpc_6_lv3)

#merge with full dataset
ipv.prosocial.anx.ext <- ipv.prosocial.anx %>% 
  left_join(ext.sub, by = "id")

#create new externalizing variable by visit that corresponds to ET session visit
ipv.prosocial.anx.ext <- ipv.prosocial.anx.ext %>%
  mutate(
    ext.by.visit = case_when(
      visit == 1 ~ bpc_sum_y_lv1,
      visit == 3 ~ bpc_sum_y_LV3,
      TRUE ~ NA_real_))
###IMPORTANT: this will drop 2 participants who don't have a externalizing self-report that matches their ET lab visit

#internal reliability calculation for externalizing measure by visit
ipv.prosocial.anx.ext <- ipv.prosocial.anx.ext %>%
  mutate(
    bpc_1 = case_when(visit == 1 ~ bpc_1_lv1, visit == 3 ~ bpc_1_lv3, TRUE ~ NA_real_),
    bpc_2 = case_when(visit == 1 ~ bpc_2_lv1, visit == 3 ~ bpc_2_lv3, TRUE ~ NA_real_),
    bpc_3 = case_when(visit == 1 ~ bpc_3_lv1, visit == 3 ~ bpc_3_lv3, TRUE ~ NA_real_), 
    bpc_4 = case_when(visit == 1 ~ bpc_4_lv1, visit == 3 ~ bpc_4_lv3, TRUE ~ NA_real_),
    bpc_5 = case_when(visit == 1 ~ bpc_5_lv1, visit == 3 ~ bpc_5_lv3, TRUE ~ NA_real_),
    bpc_6 = case_when(visit == 1 ~ bpc_6_lv1, visit == 3 ~ bpc_6_lv3, TRUE ~ NA_real_))

alpha(ipv.prosocial.anx.ext[, c("bpc_1", "bpc_2", "bpc_3", "bpc_4", "bpc_5", "bpc_6")])
alpha(ipv.prosocial.anx.ext[, c("bpc_1_lv1", "bpc_2_lv1", "bpc_3_lv1", "bpc_4_lv1", "bpc_5_lv1", "bpc_6_lv1")])
alpha(ipv.prosocial.anx.ext[, c("bpc_1_lv3", "bpc_2_lv3", "bpc_3_lv3", "bpc_4_lv3", "bpc_5_lv3", "bpc_6_lv3")])

ipv.prosocial.anx.ext <- subset(ipv.prosocial.anx.ext,
                                select = -c(bpc_sum_y_lv1, bpc_sum_y_LV3,
                                            bpc_1, bpc_2, bpc_3, bpc_4, bpc_5, bpc_6,
                                            bpc_1_lv1, bpc_2_lv1, bpc_3_lv1, bpc_4_lv1, bpc_5_lv1, bpc_6_lv1,
                                            bpc_1_lv3, bpc_2_lv3, bpc_3_lv3, bpc_4_lv3, bpc_5_lv3, bpc_6_lv3))

rm(ext, ext.sub, ipv.prosocial, ipv.prosocial.anx) #removing unneeded datasets

#### Calculate ET Metrics ####

### mean first fixation attention bias
#create a dataset with ONLY the first fixation on each trial per participant (first fixation to a face)
first.fix <- filtered.data %>%
  arrange(id, New_Trial, CURRENT_FIX_START) %>%
  group_by(id, New_Trial) %>%
  slice(1) %>%
  ungroup()

#create scores that indicate the proportion of trials with first fixation to angry compared with neutral
fix.bias.scores <- first.fix %>%
  group_by(id) %>%
  summarise(
    fix.bias.angry = mean(fixation.emotion == "Angry"))

#merge fixation scores w/self-report
ipv.prosocial.anx.ext.ET1 <- ipv.prosocial.anx.ext %>% 
  left_join(fix.bias.scores, by = "id")

### mean angry fixation dwell time bias

#calculate dwell time on angry vs neutral faces
trial.dwell <- filtered.data %>%
  group_by(id, New_Trial, fixation.emotion) %>%
  summarise(total.dwell = sum(CURRENT_FIX_DURATION), .groups = "drop")

#change to wide format
trial.wide <- trial.dwell %>%
              pivot_wider(names_from = fixation.emotion,
                          values_from = total.dwell,
                          values_fill = 0)

#calculate bias score of dwell time on angry versus any face
trial.bias <- trial.wide %>%
  mutate(
    dwell.prop.angry = Angry / (Angry + Neutral))

#calculate mean of bias scores of dwell time on angry versus any face per participant
dwell.bias.scores <- trial.bias %>%
  group_by(id) %>%
  summarise(mean.dwell.angry = mean(dwell.prop.angry, na.rm = TRUE))

#merge dwell scores w/self-report and demographics into final dataset
final.dataset <- ipv.prosocial.anx.ext.ET1 %>% 
  left_join(dwell.bias.scores, by = "id")

final.dataset <- final.dataset %>% 
  left_join(full.demo, by = "id")

final.dataset <- final.dataset %>%
  mutate(gender.code = recode(gender.code, `1` = 0, `2` = 1)) #make males = 0 and females = 1

#### Main Analyses ####

#Prepare for analyses

#visually inspect IPV composite
hist(final.dataset$ipv.composite, 
     main = "IPV Exposure Count", 
     xlab = "IPV Exposure Count", 
     col = "skyblue", 
     breaks = 10) #IPV exposure count has few participants with 3 and 4 IPV exposure types
#create a new three-level categorical variable ("ipv.levels") of 0 IPV exposure, 1 IPV exposure, 2 + IPV exposure
final.dataset <- final.dataset %>% 
  mutate(ipv.levels = dplyr::recode(ipv.composite, 
                          "0" = "0", 
                          "1" = "1",
                          "2" = "2",
                          "3" = "2",
                          "4" = "2"))

#create new centered attention variables
final.dataset <- final.dataset %>%
  mutate(fixation.attention.c = fix.bias.angry - 0.5) %>%
  mutate(dwell.attention.c = mean.dwell.angry - 0.5)

##check assumptions for linear regression models

##anxiety models
#initial attentional bias
plot(final.dataset$fixation.attention.c, final.dataset$anx.by.visit)
model <- lm(anx.by.visit ~ gender.code + ipv.levels * fixation.attention.c, data = final.dataset)
raintest(model) #linearity not violated (p > .05)
plot(model, which = 1)
ols_plot_resid_fit(model)
bptest(model) #homoscedasticity assumption violated (p = . 008) -> use HC3
shapiro.test(residuals(model)) #normality violated
plot(model, which = 2) #normality: slight tail deviation
car::vif(model) #no multicollinearity violation (VIF < 3)
plot(model, which = 4) #3 influential observations (24, 87, 121)

#examining .5 screening threshold for Cook's Distance
sort(cooks.distance(model), decreasing = TRUE)[1:11] #.1 - .03; does not exceed
final.dataset[c(87, 121, 24, 64, 124, 120, 79, 11, 57, 5, 77), ] #visually examining

#sustained attentional bias
plot(final.dataset$dwell.attention.c, final.dataset$anx.by.visit)
model <- lm(anx.by.visit ~ gender.code + ipv.levels * dwell.attention.c, data = final.dataset)
raintest(model) #linearity not violated (p > .05)
plot(model, which = 1)
ols_plot_resid_fit(model)
bptest(model) #homoscedasticity assumption violated
shapiro.test(residuals(model)) #normality violated
plot(model, which = 2) #normality: slight tail deviation
car::vif(model) #no multicollinearity violation (VIF < 3)
plot(model, which = 4) #3 influential observations (120, 64, 56)

#examining .5 screening threshold for Cook's Distance
sort(cooks.distance(model), decreasing = TRUE)[1:11] #.39 - .02; does not exceed
final.dataset[c(120, 59, 64, 34, 112, 6, 45, 23, 77, 126, 40), ] #visually examining

##externalizing models
#initial attentional bias
plot(final.dataset$fixation.attention.c, final.dataset$ext.by.visit)
model <- lm(ext.by.visit ~ gender.code + ipv.levels * fixation.attention.c, data = final.dataset)
raintest(model) #linearity violated -> USE POLYNOMIAL REGRESSION?
model <- lm(ext.by.visit ~ gender.code + ipv.levels * fixation.attention.c + I(fixation.attention.c^2), data = final.dataset)
plot(model, which = 1)
ols_plot_resid_fit(model)
bptest(model) #homoscedasticity assumption held (p > .05)
shapiro.test(residuals(model)) #normality violated
plot(model, which = 2) #normality: slight tail deviation
car::vif(model) #no multicollinearity violation (< 3)
plot(model, which = 4) #3 influential observations (24, 99, 98)

#examining .5 screening threshold for Cook's Distance
sort(cooks.distance(model), decreasing = TRUE)[1:11] #1.47 - .03 (one exceeds: #id: 1056)
final.dataset[c(24, 99, 98, 121, 79, 81, 29, 23, 96, 64, 126), ]

#compare linear vs quadratic model
model.linear <- lm(ext.by.visit ~ gender.code + ipv.levels * fixation.attention.c,
                   data = final.dataset)
model.quad <- lm(ext.by.visit ~ gender.code + ipv.levels * fixation.attention.c +
                   I(fixation.attention.c^2),
                 data = final.dataset)
anova(model.linear, model.quad) #quadratic model is a better fit (p = .01)

#sustained attentional bias
plot(final.dataset$dwell.attention.c, final.dataset$ext.by.visit)
model <- lm(ext.by.visit ~ gender.code + ipv.levels * dwell.attention.c, data = final.dataset)
raintest(model) #linearity violated -> USE POLYNOMIAL REGRESSION?
model <- lm(ext.by.visit ~ gender.code + ipv.levels * dwell.attention.c + I(dwell.attention.c^2), data = final.dataset)
plot(model, which = 1)
ols_plot_resid_fit(model)
bptest(model) #homoscedasticity assumption held (p > .05)
shapiro.test(residuals(model)) #normality violated
plot(model, which = 2) #normality: slight tail deviation
car::vif(model) #no multicollinearity violation (< 3)
plot(model, which = 4) #3 influential observations (24, 99, 23)

#examining .5 screening threshold for Cook's Distance
sort(cooks.distance(model), decreasing = TRUE)[1:11] #.22 - .02 (does not exceed)
final.dataset[c(24, 99, 23, 126, 56, 81, 96, 67, 89, 22, 64), ]

#compare linear vs quadratic model
model.linear <- lm(ext.by.visit ~ gender.code + ipv.levels * dwell.attention.c,
                   data = final.dataset)
model.quad <- lm(ext.by.visit ~ gender.code + ipv.levels * dwell.attention.c +
                   I(dwell.attention.c^2),
                 data = final.dataset)
anova(model.linear, model.quad) #quadratic does not improve model (p = .10)

##prosocial models
#initial attention bias
plot(final.dataset$fixation.attention.c, final.dataset$peq.by.visit)
model <- lm(peq.by.visit ~ gender.code + ipv.levels * fixation.attention.c, data = final.dataset)
raintest(model) #linearity not violated (p > .05)
plot(model, which = 1)
bptest(model) #homoscedasticity assumption held (p > .05)
shapiro.test(residuals(model)) #normality violated
plot(model, which = 2) #normality: slight upper tail deviation
car::vif(model) #no multicollinearity violation (VIF < 3)
plot(model, which = 4) #3 influential observations (11, 77, 87)

#examining .5 screening threshold for Cook's Distance
sort(cooks.distance(model), decreasing = TRUE)[1:11] #.08 - .03
final.dataset[c(87, 77, 11, 82, 126, 7, 120, 96, 41, 34, 22), ]

#sutained attention bias
plot(final.dataset$dwell.attention.c, final.dataset$peq.by.visit)
model <- lm(peq.by.visit ~ gender.code + ipv.levels * dwell.attention.c, data = final.dataset)
raintest(model) #linearity not violated (p > .05)
plot(model, which = 1)
bptest(model) #homoscedasticity assumption held (p > .05)
shapiro.test(residuals(model)) #normality not violated
plot(model, which = 2) #normality: minimal deviation
car::vif(model) #no multicollinearity violation (< 3)
plot(model, which = 4) #3 influential observations (23, 40, 56)

#examining .5 screening threshold for Cook's Distance
sort(cooks.distance(model), decreasing = TRUE)[1:11] #.16 - .03
final.dataset[c(56, 23, 40, 41, 77, 87, 99, 120, 22, 11, 25), ]

##run models (gender as a covariate)
#anxiety initial attention - exploratory
model <- lm(anx.by.visit ~ gender.code + ipv.levels * fixation.attention.c, data = final.dataset)
summary(model)
vc <- vcovHC(model, type = "HC3") 
coeftest(model, vcov = vc)
coefci(model, vcov. = vc, level = 0.95)

#anxiety sustained attention - confirmatory
model <- lm(anx.by.visit ~ gender.code + ipv.levels * dwell.attention.c, data = final.dataset)
summary(model)
vc <- vcovHC(model, type = "HC3") 
coeftest(model, vcov = vc)
coefci(model, vcov. = vc, level = 0.95)

#externalizing initial attention - confirmatory
model <- lm(ext.by.visit ~ gender.code + ipv.levels * fixation.attention.c + I(fixation.attention.c^2), data = final.dataset)
summary(model)
confint(model, level = 0.95)

#sensitivity analyses removing externalizing outlier based on Cook's distance
model.sensitivity <- lm(ext.by.visit ~ gender.code + ipv.levels * fixation.attention.c + I(fixation.attention.c^2),
                        data = final.dataset,
                        subset = rownames(final.dataset) != "24") #removing 1056 (weakens the effect from .01 to .05)
summary(model.sensitivity)
confint(model.sensitivity, level = 0.95)

#externalizing sustained attention - confirmatory
model <- lm(ext.by.visit ~ gender.code + ipv.levels * dwell.attention.c, data = final.dataset)
summary(model)
confint(model, level = 0.95)

#Holm-Bonferroni corrected p values for three confirmatory models? no significant interactions

#prosocial initial attention - exploratory
model <- lm(peq.by.visit ~ gender.code + ipv.levels * fixation.attention.c, data = final.dataset)
summary(model)
confint(model, level = 0.95)

#prosocial sustained attention - exploratory
model <- lm(peq.by.visit ~ gender.code + ipv.levels * dwell.attention.c, data = final.dataset)
summary(model)
confint(model, level = 0.95)

sim_slopes(model,
           pred = dwell.attention.c,
           modx = ipv.levels,
           johnson_neyman = FALSE, 
           confint = TRUE)
table(final.dataset$ipv.levels)

#effect sizes in partial R2
#IPV 2+ -> externalizing
t.value <- 3.13
df <- 117
es <- t.value^2 / (t.value^2 + df)
es #0.077

#IPV 2+ x sustained atten. -> prosocial
t.value <- 2.12
df <- 117
es <- t.value^2 / (t.value^2 + df)
es #0.036

#IPV 2+ x sustained atten. -> prosocial slopes
t.value <- 2.56
df <- 117
es <- t.value^2 / (t.value^2 + df)
es #0.053

t.value <- -1.39
df <- 117
es <- t.value^2 / (t.value^2 + df)
es #0.02

t.value <- -0.62
df <- 117
es <- t.value^2 / (t.value^2 + df)
es #0.003

#summary stats by IPV Levels
final.dataset %>%
  group_by(ipv.levels) %>%
  summarise(
    Mean = mean(age, na.rm = TRUE),
    SD = sd(age, na.rm = TRUE),
    Median = median(age, na.rm = TRUE),
    Min = min(age, na.rm = TRUE),
    Max = max(age, na.rm = TRUE))

final.dataset %>%
  group_by(ipv.levels) %>%
  summarise(
    attn_M = mean(fixation.attention.c, na.rm = TRUE),
    attn_SD = sd(fixation.attention.c, na.rm = TRUE),
    attn_Min = min(fixation.attention.c, na.rm = TRUE),
    attn_Max = max(fixation.attention.c, na.rm = TRUE),
    attn_D_M = mean(dwell.attention.c, na.rm = TRUE),
    attn_D_SD = sd(dwell.attention.c, na.rm = TRUE),
    attn_D_Min = min(dwell.attention.c, na.rm = TRUE),
    attn_D_Max = max(dwell.attention.c, na.rm = TRUE))

final.dataset %>%
  count(ipv.levels, gender) %>%
  group_by(ipv.levels) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

final.dataset %>%
  count(ipv.levels, race) %>%
  group_by(ipv.levels) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

final.dataset %>%
  count(ipv.levels, ethnicity) %>%
  group_by(ipv.levels) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

#ipv exposure levels predicting externalizing behaviors figure
ggplot(final.dataset, aes(x = ipv.levels, y = ext.by.visit)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.5) +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "IPV Types of Exposure",
    y = "Externalizing Behaviors") +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank()) +
  geom_signif(
  comparisons = list(c("1", "2+"),
                     c("0", "2+")),
  map_signif_level = TRUE,
  textsize = 5,
  y_position = c(7.5, 8.5, 9.5),
  tip_length = 0.03)
ggsave(filename="ExtByIPV.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

#quadratic main effect figure of initial attention on externalizing behaviors
ggplot(final.dataset, aes(x = fixation.attention.c, y = ext.by.visit)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) +
  labs(x = "Initial Attentional Bias (centered)",
       y = "Externalizing Behaviors") +
  theme_classic(base_size = 18)
ggsave(filename="attentionExt.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))

#interaction plots for Prosocial Behaviors
interact_plot(model,
  pred = dwell.attention.c,
  modx = ipv.levels,
  legend.main = "IPV Exposure",
  x.label = "Sustained Attention (centered)",
  y.label = "Prosocial Behaviors Toward Peers",
  plot.points = FALSE,
  interval = TRUE,
  int.width = 0.95,
  colors = c("orange", "#0072B2", "#CC79A7"),
  line.thickness = 1.6) +
  theme_classic() +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.grid.minor = element_blank())
ggsave(filename="PEQinteraction.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

interact_plot(model,
              pred = dwell.attention.c,
              modx = ipv.levels,
              interval = TRUE,
              int.width = 0.95,
              facet.modx = TRUE,
              modx.labels = c("No IPV Exposure", "1 IPV Exposure", "2+ IPV Exposures"),
              legend.main = "IPV Exposure",
              x.label = "Sustained Attention (centered)",
              y.label = "Prosocial Behaviors Toward Peers",
              plot.points = TRUE,
              colors = c("#0072B2", "#009E73", "#CC79A7"),
              line.thickness = 1.6) +
  theme_classic() +
  theme(
    text = element_text(size = 24),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 20),
    panel.grid.minor = element_blank())
ggsave(filename="PEQinteraction.panels.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

#correlation matrix descriptive stats
cor.matrix <- final.dataset %>% select("id", "age", "anx.by.visit", "ext.by.visit", "peq.by.visit", "fix.bias.angry", "mean.dwell.angry")
cor.matrix$age <- as.numeric(cor.matrix$age)
colnames(cor.matrix) <- c("id", "Age", "Anxiety Symptoms", "Externalizing Behaviors", 
"Prosocial Behaviors", "Initial Attention", "Sustained Attention")
apa.cor.table(cor.matrix, filename = "correlation_table.doc")

#IPV figure depicting the proportion of endorsed interpersonal violence exposure types
trauma.vars <- c("ucla_cv", "ucla_dv", "ucla_pas", "ucla_sab", "ucla_pab", "ucla_eab", "ucla_sas", "ucla_bully")

visit.trauma <- visit.trauma %>% 
  mutate(ipv.levels = dplyr::recode(ipv.composite, 
                                    "0" = "0", 
                                    "1" = "1",
                                    "2" = "2+",
                                    "3" = "2+",
                                    "4" = "2+"))

visit.trauma.long <- visit.trauma %>%
  pivot_longer(
    cols = all_of(trauma.vars),
    names_to = "trauma.type",
    values_to = "endorsed") %>%
  filter(endorsed == 1)

ggplot(visit.trauma.long, aes(x = factor(ipv.levels), fill = trauma.type)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.2) +
  labs(
    x = "Number of Interpersonal Violence Exposures",
    y = "Proportion",
    fill = "Interpersonal Violence",
    title = "Interpersonal Violence Type By Exposure Count") +
  scale_fill_manual(
    values = c(
      "ucla_bully" = "#1b9e77",
      "ucla_cv"    = "#E07A5F",
      "ucla_dv"    = "#7570b3",
      "ucla_eab"   = "#FF6B6B",
      "ucla_pab"   = "#66a61e",
      "ucla_pas"   = "#e6ab02",
      "ucla_sab"   = "#0099FF",
      "ucla_sas"   = "#F15BB5"),
    labels = c(
      "ucla_pas"   = "Physical Assault",
      "ucla_pab"   = "Physical Abuse",
      "ucla_eab"   = "Emotional Abuse",
      "ucla_sab"   = "Sexual Abuse",
      "ucla_sas"   = "Sexual Assault",
      "ucla_dv"    = "Witnessing Domestic Violence",
      "ucla_cv"    = "Community Violence",
      "ucla_bully" = "Bullying")) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    plot.title = element_text(size = 18, face = "bold"))
ggsave(filename="TraumaDistribution.twogroups.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

#extra figures - distributions of variables

#anxiety distribution
final.dataset %>%
  ggplot( aes(x=anx.by.visit)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(x = "Anxiety Symptoms") +
  theme_minimal()
ggsave(filename="AnxDistribution.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

#externalizing behaviors distribution
final.dataset %>%
  ggplot( aes(x=ext.by.visit)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(x = "Externalizing Behaviors") +
  theme_minimal()
ggsave(filename="ExtDistribution.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

#prosocial behaviors distribution
final.dataset %>%
  ggplot( aes(x=peq.by.visit)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(x = "Prosocial Behaviors Towards Peers") +
  theme_minimal()
ggsave(filename="ProsocialBehaviorDistribution.png",plot=last_plot(), path=savepath,scale=1,limitsize=F,
       dpi=300,width=10,height=5.6,units=c("in"))  

#age distribution
ggplot(final.dataset, aes(age)) +
  geom_bar() +
  scale_x_binned()

#### Do demographics differ for participants excluded versus included? ####

all.participants <- all.fixation %>% 
    distinct(RECORDING_SESSION_LABEL) %>%
    rename(id = RECORDING_SESSION_LABEL) #dataset of all participant ids

included.participants <- final.dataset %>% 
    distinct(id) #dataset of included participants ids

excluded.participants <- all.participants %>%
  anti_join(included.participants, by = "id") #create dataset of excluded participants ids based on included participants ids

excluded.participants <- excluded.participants %>%
  left_join(full.demo, by = "id") #pull demographics for excluded participants

excluded.participants %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age   = min(age, na.rm = TRUE),
    max_age   = max(age, na.rm = TRUE)) #age: m = 12.68, sd = 2.69, range = 8-18

excluded.participants %>%
  count(gender) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) #51% female, 49% male

excluded.participants %>%
  count(race) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) #47% AA, %47% White, 6% multiracial

excluded.participants %>%
  count(ethnicity) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) #93% not Hispanic, 7% Hispanic

#### Comparing main variables for participants excluded versus included based on eyetracking data ####

all.participants <- all.fixation %>% 
  distinct(RECORDING_SESSION_LABEL) %>%
  rename(id = RECORDING_SESSION_LABEL) #dataset of all participant ids

included.participants <- final.dataset %>% 
  distinct(id) #dataset of included participants ids

excluded.participants <- all.participants %>%
  anti_join(included.participants, by = "id") #create dataset of excluded participants ids (n = 92) based on included participants ids

excluded.participants <- excluded.participants %>%
  left_join(full.demo, by = "id") #pull demographics for excluded participants

#merge trauma data
trauma <- read_sav("datasets/LV1_UCLA_CGandY_v09_current.sav") #pull in raw trauma dataset
trauma$id <- as.integer(trauma$id) #change id to an integer for later merging
trauma.sub <- trauma %>% select(id, "count_TE_vw_or",
                                "ucla_cv_cg",  "ucla_cv_ch", "ucla_dv_cg", "ucla_dv_ch",
                                "ucla_pas_cg", "ucla_pas_ch", "ucla_sab_cg", "ucla_sab_ch",
                                "ucla_pab_cg", "ucla_pab_ch", "ucla_neg_cg", "ucla_neg_ch", "ucla_eab_cg", "ucla_eab_ch",
                                "ucla_sas_cg", "ucla_sas_ch", "ucla_bully_cg", "ucla_bully_ch", "ucla_abd_cg", "ucla_abd_ch",
                                "ucla_ter_cg", "ucla_ter_ch", "ucla_war_cg", "ucla_war_ch", "ucla_expl_cg", "ucla_expl_ch") #create a smaller subset of study relevant variables

#pull in visit data again for participants and merge with excluded participant list
visit<-read.csv("datasets/visit.csv",header=T) #pulling in visit dataset again
excluded.participants.visit <- excluded.participants %>%
  left_join(visit, by = "id")

#merge with trauma dataset with visit data
excluded.participants.visit <- excluded.participants.visit %>%
  left_join(trauma.sub, by = "id")

#if NA (missing) for child and parent trauma report, remove from dataset (total missing = 23 participants)
excluded.participants.visit <- excluded.participants.visit %>%
  filter(!is.na(count_TE_vw_or)) #69 participants with eye tracking and trauma data

#calculating endorsement of the following less common events
table(excluded.participants.visit$ucla_abd_cg) #no endorsement - abduction caregiver
table(excluded.participants.visit$ucla_abd_ch) #no endorsement - abduction child
table(excluded.participants.visit$ucla_ter_cg) #no endorsement - terrorism caregiver
table(excluded.participants.visit$ucla_ter_ch) #no endorsement - terrorism child
table(excluded.participants.visit$ucla_war_cg) #no endorsement - war caregiver
table(excluded.participants.visit$ucla_war_ch) #no endorsement - war child
table(excluded.participants.visit$ucla_expl_cg) #no endorsement - exploitation caregiver
table(excluded.participants.visit$ucla_expl_ch) #no endorsement - exploitation child

#Creating a new composite variable for child OR adult report IPV in remaining events
pairs <- list(
  cv   = c("ucla_cv_cg", "ucla_cv_ch"),
  dv   = c("ucla_dv_cg", "ucla_dv_ch"),
  pas  = c("ucla_pas_cg", "ucla_pas_ch"),
  sab  = c("ucla_sab_cg", "ucla_sab_ch"),
  pab  = c("ucla_pab_cg", "ucla_pab_ch"),
  eab  = c("ucla_eab_cg", "ucla_eab_ch"),
  sas  = c("ucla_sas_cg", "ucla_sas_ch"),
  bully= c("ucla_bully_cg", "ucla_bully_ch"))

vars <- c("cv","dv","pas","sab","pab","eab","sas","bully")

for (v in vars) {
  cg <- paste0("ucla_", v, "_cg")
  ch <- paste0("ucla_", v, "_ch")
  new <- paste0("ucla_", v)
  
  excluded.participants.visit[[new]] <- pmax(excluded.participants.visit[[cg]], excluded.participants.visit[[ch]], na.rm = TRUE)
}

excluded.participants.visit$ipv.composite <- rowSums(excluded.participants.visit[paste0("ucla_", vars)], na.rm = TRUE)

excluded.participants.visit %>%
  count(ipv.composite) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) 
#exposures ranged from 0 to 5
#0 exposures: 60.9%, 1 exposure: 15.9%, 2 exposures: 15.9%, 3 exposures: 4.3%, 4 exposures: 1.4%, 5 exposures: 1.4%
#0 exposures: 60.9%, 1 exposure: 15.9%, 2 or more exposures: 23%

#merge prosocial measure
peq <- read_sav("datasets/PEQ_LV123_v01.sav") #pull in prosocial behaviors variable (PEQ)
peq.subset <- peq %>%
  select(id, peq_prosocgive_sum_lv1, peq_prosocgive_sum_lv3)
peq.subset$id <- as.integer(peq.subset$id) #change id to integer

#merge prosocial behavior with IPV dataset and visit dataset
excluded.participants <- excluded.participants %>% left_join(peq.subset, by = "id")
excluded.participants <- excluded.participants %>%
  left_join(visit, by = "id")

#create new peq by visit that corresponds to when each participant completed their eye tracking visit
excluded.participants <- excluded.participants %>%
  mutate(
    peq.by.visit = case_when(
      visit == 1 ~ peq_prosocgive_sum_lv1,
      visit == 3 ~ peq_prosocgive_sum_lv3,
      TRUE ~ NA_real_))

excluded.participants %>%
  summarise(
    mean_peq = mean(peq.by.visit, na.rm = TRUE),
    sd_peq = sd(peq.by.visit, na.rm = TRUE),
    min_peq   = min(peq.by.visit, na.rm = TRUE),
    max_peq   = max(peq.by.visit, na.rm = TRUE)) #peq: m = 15.44, sd = 5.50, range = 5-15

#merging externalizing variable
ext <- read_sav("datasets/LV123_BPC_y_cg.sav") #pulling in externalizing variable
ext$id <- as.integer(ext$id) 
ext.sub <- ext %>% select(id, bpc_sum_y_lv1, bpc_sum_y_LV3)

#merge with full dataset
excluded.participants <- excluded.participants %>% 
  left_join(ext.sub, by = "id")

#create new externalizing variable by visit that corresponds to ET session visit
excluded.participants <- excluded.participants %>%
  mutate(
    ext.by.visit = case_when(
      visit == 1 ~ bpc_sum_y_lv1,
      visit == 3 ~ bpc_sum_y_LV3,
      TRUE ~ NA_real_))

excluded.participants %>%
  summarise(
    mean_ext = mean(ext.by.visit, na.rm = TRUE),
    sd_ext = sd(ext.by.visit, na.rm = TRUE),
    min_ext   = min(ext.by.visit, na.rm = TRUE),
    max_ext   = max(ext.by.visit, na.rm = TRUE)) #ext: m = 1.87, sd = 1.97, range = 0-10

#merge anxiety variable
anx.lv1 <- read_sav("datasets/CHARM_LV1_PROMIS_PA_y_GP.sav") #pull in anxiety data from visit 1
anx.lv1 <- anx.lv1 %>% rename(id = Child_ID) 
anx.lv1$id <- as.integer(anx.lv1$id) 
anx.lv1.sub <- anx.lv1 %>% select(id, PA_T_1)

anx.lv3 <- read_sav("datasets/CHARM_LV3_PROMIS_PA_y_GP.sav") #pull in anxiety data from visit 3
anx.lv3 <- anx.lv3 %>% rename(id = Child_ID) 
anx.lv3$id <- as.integer(anx.lv3$id)
anx.lv3.sub <- anx.lv3 %>% select(id, PA_T_3)

#merge anxiety variables from visit 1 and visit 3 with IPV dataset
excluded.participants <- excluded.participants %>% 
  left_join(anx.lv1.sub, by = "id")  %>%
  left_join(anx.lv3.sub, by = "id")

#create new anxiety variable by visit that corresponds to when participant completed eye tracking visit
excluded.participants <- excluded.participants %>%
  mutate(
    anx.by.visit = case_when(
      visit == 1 ~ PA_T_1,
      visit == 3 ~ PA_T_3,
      TRUE ~ NA_real_))

excluded.participants %>%
  summarise(
    mean_anx = mean(anx.by.visit, na.rm = TRUE),
    sd_anx = sd(anx.by.visit, na.rm = TRUE),
    min_anx   = min(anx.by.visit, na.rm = TRUE),
    max_anx   = max(anx.by.visit, na.rm = TRUE)) #anx: m = 44.01, sd = 11.60, range = 33.5-83.3
