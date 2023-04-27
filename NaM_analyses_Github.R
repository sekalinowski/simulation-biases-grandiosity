# NaM Project
# Analysis code, including descriptive stats, final models, 
# Operates on the output of NaM_code_merged.R

#%%%%%%%%%%%%%%%%%%%
# Read packages ####
#%%%%%%%%%%%%%%%%%%%
if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(openxlsx)) {install.packages("openxlsx"); require(openxlsx)}
if (!require(Hmisc)) {install.packages("Hmisc"); require(Hmisc)}
if (!require(sjPlot)) {install.packages("sjPlot"); require(sjPlot)}
if (!require(car)) {install.packages("car"); require(car)}
if (!require(glmmTMB)) {install.packages("glmmTMB"); require(glmmTMB)}
if (!require(Rfast2)) {install.packages("Rfast2"); require(Rfast2)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(lattice)) {install.packages("lattice"); require(lattice)}

#%%%%%%%%%%%%%%%%%%%%%
# Load data files ####
#%%%%%%%%%%%%%%%%%%%%%

# Sets the working directory to the directory in which this script is stored.
# This should be in the same folder as the data files.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

S1_data_input <- "NaM_Study1_sharedData.csv"
Rep_data_input <- "NaM_Rep_sharedData.csv"

# Read in data from Study 1
S1_fulldf <- read.csv(S1_data_input) # 1344 x 23
str(S1_fulldf)

# Read in data from the Replication
Rep_fulldf <- read.csv(Rep_data_input) # 2028 x 23
str(Rep_fulldf)

# Data for future trials only
S1_futdf <- subset(S1_fulldf, S1_fulldf$taskName == "FUTURE")
Rep_futdf <- subset(Rep_fulldf, Rep_fulldf$taskName == "FUTURE")

#%%%%%%%%%%%%%%%%%%%%%%%%
# Descriptive Stats #####
#%%%%%%%%%%%%%%%%%%%%%%%%

##### Study 1
boxplot(S1_fulldf$eventDifficulty, na.rm=TRUE)
boxplot(S1_fulldf$eventVividness, na.rm = TRUE)
boxplot(S1_fulldf$eventVisPerspective, na.rm = TRUE)
boxplot(S1_fulldf$eventPlausibility, na.rm = TRUE)
boxplot(S1_fulldf$eventThoughtAboutBefore, na.rm = TRUE)
boxplot(S1_fulldf$numInternalDetails)
boxplot(S1_fulldf$numExternalDetails)

hist(S1_fulldf$FFNI_total)
hist(S1_fulldf$FFNI_Grandiose) # distribution of grandiose narcissism in Study 1
range(S1_fulldf$FFNI_Grandiose)
mean(S1_fulldf$FFNI_Grandiose)
sd(S1_fulldf$FFNI_Grandiose)

##### Replication
boxplot(Rep_fulldf$eventDifficulty, na.rm=TRUE)
boxplot(Rep_fulldf$eventVividness, na.rm = TRUE)
boxplot(Rep_fulldf$eventVisPerspective, na.rm = TRUE)
boxplot(Rep_fulldf$eventPlausibility, na.rm = TRUE)
boxplot(Rep_fulldf$eventThoughtAboutBefore, na.rm = TRUE)
boxplot(Rep_fulldf$numInternalDetails)
boxplot(Rep_fulldf$numExternalDetails)

hist(Rep_fulldf$FFNI_total)
hist(Rep_fulldf$FFNI_Grandiose) # distribution of grandiose narcissism in Replication
range(Rep_fulldf$FFNI_Grandiose)
mean(Rep_fulldf$FFNI_Grandiose)
sd(Rep_fulldf$FFNI_Grandiose)

########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cue Word & Manipulation Checks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Evaluating self-report ratings of the emotional tone of events resulting from 
#     positive or negative word cues on a scale of 1 (extremely negative) to 7 (extremely positive)

## Study 1

# Across all cue words
ggstatsplot::ggbetweenstats(
  data = S1_fulldf,
  x = stim,
  y = eventEmotionalTone,
  ylab = "Emotional Tone",
  xlab = "Cue Word",
  title = "Study 1: Emotional Tone of Cue Word",
  pairwise.comparisons = FALSE
)

# Binned across positive and negative cue words
boxplot(S1_fulldf$eventEmotionalTone ~ S1_fulldf$valence)

# Testing the relationship between cue word valence and emotional tone ratings
S1_emotionalToneCheck <- lmerTest::lmer(eventEmotionalTone ~ valence + (1|subjectID), data = S1_fulldf)
summary(S1_emotionalToneCheck)
sjPlot::tab_model(S1_emotionalToneCheck)

## Replication

# Across all cue words
ggstatsplot::ggbetweenstats(
  data = Rep_fulldf,
  x = stim,
  y = eventEmotionalTone,
  ylab = "Emotional Tone",
  xlab = "Cue Word",
  title = "Replication: Emotional Tone of Cue Word",
  pairwise.comparisons = FALSE
)

# Binned across positive and negative cue words
boxplot(Rep_fulldf$eventEmotionalTone ~ Rep_fulldf$valence)

# Testing the relationship between cue word valence and emotional tone ratings
Rep_emotionalToneCheck <- lmerTest::lmer(eventEmotionalTone ~ valence + (1|subjectID), data = Rep_fulldf)
summary(Rep_emotionalToneCheck)
sjPlot::tab_model(Rep_emotionalToneCheck)

# Getting SDs for the table 
sd(S1_fulldf[S1_fulldf$stim == "admired",]$eventEmotionalTone)
sd(S1_fulldf[S1_fulldf$stim == "interesting",]$eventEmotionalTone)
sd(S1_fulldf[S1_fulldf$stim == "kind",]$eventEmotionalTone)
sd(S1_fulldf[S1_fulldf$stim == "productive",]$eventEmotionalTone)
sd(S1_fulldf[S1_fulldf$stim == "skilled",]$eventEmotionalTone)
sd(S1_fulldf[S1_fulldf$stim == "smart",]$eventEmotionalTone)

sd(Rep_fulldf[Rep_fulldf$stim == "admired",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "interesting",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "kind",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "productive",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "skilled",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "smart",]$eventEmotionalTone)

sd(S1_fulldf[S1_fulldf$stim == "boring",]$eventEmotionalTone, na.rm = TRUE)
sd(S1_fulldf[S1_fulldf$stim == "criticized",]$eventEmotionalTone,na.rm = TRUE)
sd(S1_fulldf[S1_fulldf$stim == "inadequate",]$eventEmotionalTone,na.rm = TRUE)
sd(S1_fulldf[S1_fulldf$stim == "lazy",]$eventEmotionalTone,na.rm = TRUE)
sd(S1_fulldf[S1_fulldf$stim == "mean",]$eventEmotionalTone,na.rm = TRUE)
sd(S1_fulldf[S1_fulldf$stim == "stupid",]$eventEmotionalTone,na.rm = TRUE)

sd(Rep_fulldf[Rep_fulldf$stim == "boring",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "criticized",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "inadequate",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "lazy",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "mean",]$eventEmotionalTone)
sd(Rep_fulldf[Rep_fulldf$stim == "stupid",]$eventEmotionalTone)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Subjective Rating Analyses  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# In this section, we estimate the models predicting the subjective ratings of events (emotional tone, difficulty, vividness, visual perspective,
#   frequency, and plausibility), looking at the interaction of grandiosity, valence, and task, with random effects of stim (the cue word used
#   to prompt that trial) and subjectID.

#### Study 1 #

## Emotional tone
S1_toneModel <- lmerTest::lmer(eventEmotionalTone ~ FFNI_Grandiose*valence*taskName + (1|stim) + (1|subjectID), data = S1_fulldf)
plot(allEffects(S1_toneModel), main = "Emotional Tone of Event", xlab = "Grandiosity", ylab = "Emotional Tone
     (1 = extremely negative; 7 = extremely positive)")
summary(S1_toneModel)

## Difficulty
S1_difficultmodel <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName +(1|stim) + (1|subjectID), data = S1_fulldf)
plot(allEffects(S1_difficultmodel), main = "Difficulty Thinking of Event", xlab = "Grandiosity", ylab = "Difficulty thinking of and 
     writing about event (1 = Not at all difficult; 7 = Extremely difficult)")
summary(S1_difficultmodel)

## Vividness
S1_vividmodel <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName +(1|stim) + (1|subjectID), data = S1_fulldf)
plot(allEffects(S1_vividmodel), main = "Vividness of Event", xlab = "Grandiosity", ylab = "Vividness (1 = Not at all vivid; 7 = Extremely vivid)")
summary(S1_vividmodel)

## Visual perspective
S1_perspectmodel <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + (1|stim) + (1|subjectID), data = S1_fulldf)
plot(allEffects(S1_perspectmodel), main = "Visual Perspective of Event", xlab = "Grandiosity", ylab = "Visual Perspective 
     (1 = Entirely own eyes; 7 = Entirely outside point of view)")
summary(S1_perspectmodel)

## Frequency of thinking about before
S1_freqmodel <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence + (1|stim) + (1|subjectID), 
                                 data = S1_fulldf[S1_fulldf$taskName == "FUTURE",])
plot(allEffects(S1_freqmodel), main = "Frequency of Thinking About Event", xlab = "Grandiosity", ylab = "Thought About Event
     (1 = never; 7 = every day)")
summary(S1_freqmodel)

## Plausibility
S1_plausmodel <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + (1|stim) + (1|subjectID), 
                                data = S1_fulldf[S1_fulldf$taskName == "FUTURE",])
plot(allEffects(S1_plausmodel), main = "Plausibility of Future Event", xlab = "Grandiosity", ylab = "Plausibility
     (1 = very implausible; 7 = very plausible)")
summary(S1_plausmodel)

sjPlot::tab_model(S1_difficultmodel, S1_vividmodel, S1_perspectmodel, S1_plausmodel, S1_freqmodel)

## Study 1 Multiple Comparisons Correction (fdr) 
# Order is difficulty, vividness, visual perspective, plausibility, frequency

# Intercept
S1p_IntMain <- c(summary(S1_difficultmodel)$coefficients[1,5], summary(S1_vividmodel)$coefficients[1,5], summary(S1_perspectmodel)$coefficients[1,5],
                summary(S1_plausmodel)$coefficients[1,5], summary(S1_freqmodel)$coefficients[1,5])
S1p_IntMain
round(p.adjust(S1p_IntMain, method = "fdr", n = length(S1p_IntMain)), digits = 3)

# Main effect of grandiosity
S1p_GranMain <- c(summary(S1_difficultmodel)$coefficients[2,5], summary(S1_vividmodel)$coefficients[2,5], summary(S1_perspectmodel)$coefficients[2,5],
                  summary(S1_plausmodel)$coefficients[2,5], summary(S1_freqmodel)$coefficients[2,5])
S1p_GranMain
round(p.adjust(S1p_GranMain , method = "fdr", n = length(S1p_GranMain)),3)

# Main effect of valence
S1p_ValMain <- c(summary(S1_difficultmodel)$coefficients[3,5], summary(S1_vividmodel)$coefficients[3,5], summary(S1_perspectmodel)$coefficients[3,5],
                 summary(S1_plausmodel)$coefficients[3,5], summary(S1_freqmodel)$coefficients[3,5])
S1p_ValMain
round(p.adjust(S1p_ValMain, method = "fdr", n = length(S1p_ValMain)), 3)

# Main effect of task
S1p_TaskMain <- c(summary(S1_difficultmodel)$coefficients[4,5], summary(S1_vividmodel)$coefficients[4,5], summary(S1_perspectmodel)$coefficients[4,5])
S1p_TaskMain
round(p.adjust(S1p_TaskMain , method = "fdr", n = length(S1p_TaskMain)), 3)

# Interaction of valence and grandiosity
S1p_GranxVal <- c(summary(S1_difficultmodel)$coefficients[5,5], summary(S1_vividmodel)$coefficients[5,5], summary(S1_perspectmodel)$coefficients[5,5],
                  summary(S1_plausmodel)$coefficients[4,5], summary(S1_freqmodel)$coefficients[4,5])
S1p_GranxVal
round(p.adjust(S1p_GranxVal, method = "fdr", n = length(S1p_GranxVal)),3)

# Interaction of grandiosity and task
S1p_GranxTas <- c(summary(S1_difficultmodel)$coefficients[6,5], summary(S1_vividmodel)$coefficients[6,5], summary(S1_perspectmodel)$coefficients[6,5])
S1p_GranxTas
round(p.adjust(S1p_GranxTas, method = "fdr", n = length(S1p_GranxTas)),3)

# Interaction of valence and task
S1p_ValxTas <- c(summary(S1_difficultmodel)$coefficients[7,5], summary(S1_vividmodel)$coefficients[7,5], summary(S1_perspectmodel)$coefficients[7,5])
S1p_ValxTas
round(p.adjust(S1p_ValxTas , method = "fdr", n = length(S1p_ValxTas)),3)

# Interaction of grandiosity, valence, and task
S1p_GranxValxTas <- c(summary(S1_difficultmodel)$coefficients[8,5], summary(S1_vividmodel)$coefficients[8,5], summary(S1_perspectmodel)$coefficients[8,5])
S1p_GranxValxTas
round(p.adjust(S1p_GranxValxTas, method = "fdr", n = length(S1p_GranxValxTas)),3)

#### Replication #

## Emotional tone: there is a significant grandiosity*valence effect here, so we add emotional tone as a fixed effect in subsequent models
Rep_tonemodel <- lmerTest::lmer(eventEmotionalTone ~ FFNI_Grandiose*valence*taskName + (1|stim) + (1|subjectID), data = Rep_fulldf)
plot(allEffects(Rep_tonemodel), main = "Emotional Tone of Event", xlab = "Grandiosity", ylab = "Emotional Tone
     (1 = never; 7 = every day)")
summary(Rep_tonemodel)
sjPlot::tab_model(Rep_tonemodel)

## Difficulty
Rep_difficultmodel <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + (1|stim) + (1|subjectID), 
                                     data = Rep_fulldf)
plot(allEffects(Rep_difficultmodel), main = "Difficulty Thinking of Event", xlab = "Grandiosity", ylab = 
       "Difficulty thinking of and writing about event (1 = Not at all difficult; 7 = Extremely difficult)")
summary(Rep_difficultmodel)

## Vividness
Rep_vividmodel <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + (1|stim) + (1|subjectID), 
                                 data = Rep_fulldf)
plot(allEffects(Rep_vividmodel), main = "Vividness of Event", xlab = "Grandiosity", ylab = 
       "Vividness (1 = Not at all vivid; 7 = Extremely vivid)")
summary(Rep_vividmodel)

## Visual perspective
Rep_perspectmodel <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + (1|stim) + (1|subjectID), 
                                    data = Rep_fulldf)
plot(allEffects(Rep_perspectmodel), main = "Visual Perspective of Event", xlab = "Grandiosity", ylab = "Visual Perspective 
     (1 = Entirely own eyes; 7 = Entirely outside point of view)")
summary(Rep_perspectmodel)

## Frequency of having thought about before
Rep_freqmodel <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + (1|stim) + (1|subjectID), 
                                  data = Rep_fulldf)
plot(allEffects(Rep_freqmodel), main = "Frequency of Thinking About Event", xlab = "Grandiosity", ylab = "Thought About Event 
     (1 = never; 7 = every day)")
summary(Rep_freqmodel)

## Plausibility
Rep_plausmodel <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + eventEmotionalTone + (1|stim) + (1|subjectID), 
                                 data = Rep_fulldf[Rep_fulldf$taskName == "FUTURE",])
plot(allEffects(Rep_plausmodel), main = "Plausibility of Future Event", xlab = "Grandiosity", ylab = "Plausibility
     (1 = very implausible; 7 = very plausible)")
summary(Rep_plausmodel)

sjPlot::tab_model(Rep_difficultmodel, Rep_vividmodel, Rep_perspectmodel, Rep_freqmodel, Rep_plausmodel)

## Replication Multiple Comparisons Correction (fdr)
# Order is difficulty, vividness, visual perspective, plausibility, frequency

# Intercept
Repp_IntMain <- c(summary(Rep_difficultmodel)$coefficients[1,5], summary(Rep_vividmodel)$coefficients[1,5], summary(Rep_perspectmodel)$coefficients[1,5],
                 summary(Rep_plausmodel)$coefficients[1,5], summary(Rep_freqmodel)$coefficients[1,5])
Repp_IntMain
round(p.adjust(Repp_IntMain, method = "fdr", n = length (Repp_IntMain)),3)

# Main effect of grandiosity
Repp_GranMain <- c(summary(Rep_difficultmodel)$coefficients[2,5], summary(Rep_vividmodel)$coefficients[2,5], summary(Rep_perspectmodel)$coefficients[2,5],
                   summary(Rep_plausmodel)$coefficients[2,5], summary(Rep_freqmodel)$coefficients[2,5])
Repp_GranMain
round(p.adjust(Repp_GranMain , method = "fdr", n = length (Repp_GranMain)),3)

# Main effect of valence
Repp_ValMain <- c(summary(Rep_difficultmodel)$coefficients[3,5], summary(Rep_vividmodel)$coefficients[3,5], summary(Rep_perspectmodel)$coefficients[3,5],
                 summary(Rep_plausmodel)$coefficients[3,5], summary(Rep_freqmodel)$coefficients[3,5])
Repp_ValMain
round(p.adjust(Repp_ValMain, method = "fdr", n = length (Repp_ValMain )),3)

# Main effect of task
Repp_TaskMain <- c(summary(Rep_difficultmodel)$coefficients[4,5], summary(Rep_vividmodel)$coefficients[4,5], summary(Rep_perspectmodel)$coefficients[4,5],
                   summary(Rep_freqmodel)$coefficients[4,5])
Repp_TaskMain
round(p.adjust(Repp_TaskMain , method = "fdr", n = length (Repp_TaskMain)),3)

# Main effect of emotional tone
Repp_ToneMain <- c(summary(Rep_difficultmodel)$coefficients[5,5], summary(Rep_vividmodel)$coefficients[5,5], summary(Rep_perspectmodel)$coefficients[5,5],
                   summary(Rep_plausmodel)$coefficients[4,5], summary(Rep_freqmodel)$coefficients[5,5])
Repp_ToneMain
round(p.adjust(Repp_ToneMain , method = "fdr", n = length (Repp_ToneMain)),3)

# Interaction of grandiosity and valence
Repp_GranxVal <- c(summary(Rep_difficultmodel)$coefficients[6,5], summary(Rep_vividmodel)$coefficients[6,5], summary(Rep_perspectmodel)$coefficients[6,5],
                  summary(Rep_plausmodel)$coefficients[5,5], summary(Rep_freqmodel)$coefficients[6,5])
Repp_GranxVal
round(p.adjust(Repp_GranxVal, method = "fdr", n = length(Repp_GranxVal)),3)

# Interaction of grandiosity and task
Repp_GranxTas <- c(summary(Rep_difficultmodel)$coefficients[7,5], summary(Rep_vividmodel)$coefficients[7,5], summary(Rep_perspectmodel)$coefficients[7,5],
                    summary(Rep_freqmodel)$coefficients[7,5])
Repp_GranxTas
round(p.adjust(Repp_GranxTas, method = "fdr", n = length(Repp_GranxTas)),3)

# Interaction of valence and task
Repp_ValxTas <- c(summary(Rep_difficultmodel)$coefficients[8,5], summary(Rep_vividmodel)$coefficients[8,5], summary(Rep_perspectmodel)$coefficients[8,5],
                    summary(Rep_freqmodel)$coefficients[8,5])
Repp_ValxTas
round(p.adjust(Repp_ValxTas , method = "fdr", n = length(Repp_ValxTas)),3)

# Interaction of grandiosity, valence, and task
Repp_GranxValxTas <- c(summary(Rep_difficultmodel)$coefficients[9,5], summary(Rep_vividmodel)$coefficients[9,5], summary(Rep_perspectmodel)$coefficients[9,5],
                       summary(Rep_freqmodel)$coefficients[9,5])
Repp_GranxValxTas
round(p.adjust(Repp_GranxValxTas, method = "fdr", n = length(Repp_GranxValxTas)),3)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Objective Detail Analyses ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We here estimate a series of linear mixed effects models, predicting the internal and external detail from written narratives 
#     describing remembered past and imagined future events. Internal detail represents the amount of episodic detail
#     provided in the event, and external detail reflects non-episodic detail according to the Autobiographical Interview 
#     (Levine et al., 2002). Internal and external detail metrics were acquired with the automated Autobiographical Interview 
#     scoring tool (van Genugten & Schacter, 2022).

# Correlate manual and automated scores ###
S1_scoringDF <- S1_fulldf[!(is.na(S1_fulldf$manualInternalDetails)),]
dim(S1_scoringDF)
Rep_scoringDF <- Rep_fulldf[!(is.na(Rep_fulldf$manualInternalDetails)),]
dim(Rep_scoringDF)

### Study 1
# Internal
cor.test(S1_scoringDF$manualInternalDetails, S1_scoringDF$numInternalDetails) # 0.624

ggplot(data = S1_scoringDF) + 
  geom_point(mapping = aes(x = manualInternalDetails, y = numInternalDetails))

# External
cor.test(S1_scoringDF$manualExternalDetails, S1_scoringDF$numExternalDetails) # 0.324

ggplot(data = S1_scoringDF) + 
  geom_point(mapping = aes(x = manualExternalDetails, y = numExternalDetails))


##### Replication
# Internal
cor.test(Rep_scoringDF$manualInternalDetails, Rep_scoringDF$numInternalDetails) # 0.776

ggplot(data = Rep_scoringDF) + 
  geom_point(mapping = aes(x = manualInternalDetails, y = numInternalDetails))

# External
cor.test(Rep_scoringDF$manualExternalDetails, Rep_scoringDF$numExternalDetails) # 0.643

ggplot(data = Rep_scoringDF) + 
  geom_point(mapping = aes(x = manualExternalDetails, y = numExternalDetails))


##### Cross-correlations

### Study 1
cor.test(S1_scoringDF$manualInternalDetails, S1_scoringDF$numExternalDetails) # -0.079, n.s.

ggplot(data = S1_scoringDF) + 
  geom_point(mapping = aes(x = manualInternalDetails, y = numExternalDetails))

cor.test(S1_scoringDF$manualExternalDetails, S1_scoringDF$numInternalDetails) # 0.148, n.s.

ggplot(data = S1_scoringDF) + 
  geom_point(mapping = aes(x = manualExternalDetails, y = numInternalDetails))

### Rep
cor.test(Rep_scoringDF$manualInternalDetails, Rep_scoringDF$numExternalDetails) # -0.103, n.s.

ggplot(data = Rep_scoringDF) + 
  geom_point(mapping = aes(x = manualInternalDetails, y = numExternalDetails))

cor.test(Rep_scoringDF$manualExternalDetails, Rep_scoringDF$numInternalDetails) # -0.062, n.s.

ggplot(data = Rep_scoringDF) + 
  geom_point(mapping = aes(x = manualExternalDetails, y = numInternalDetails))


#### Create models: Number of details
## Study 1, Internal
S1_internalmodel <- lmerTest::lmer(numInternalDetails ~ FFNI_Grandiose*valence*taskName + (1|stim) + (1|subjectID), data = S1_fulldf)
plot(allEffects(S1_internalmodel), main = "Study 1 Internal Details", xlab = "Grandiosity", ylab = "# Internal Details")
summary(S1_internalmodel)

## Study 1, External
S1_externalmodel <- lmerTest::lmer(numExternalDetails ~ FFNI_Grandiose*valence*taskName + (1|stim) + (1|subjectID), data = S1_fulldf)
plot(allEffects(S1_externalmodel), main = "Study 1 External Details", xlab = "Grandiosity", ylab = "# External Details")
summary(S1_externalmodel)

## Replication, Internal
Rep_internalmodel <- lmerTest::lmer(numInternalDetails ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + (1|stim) + (1|subjectID), data = Rep_fulldf)
plot(allEffects(Rep_internalmodel), main = "Rep Internal Details", xlab = "Grandiosity", ylab = "# Internal Details")
summary(Rep_internalmodel)

## Replication, External
Rep_externalmodel <- lmerTest::lmer(numExternalDetails ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + (1|stim) + (1|subjectID), data = Rep_fulldf)
plot(allEffects(Rep_externalmodel), main = "Rep Internal Details", xlab = "Grandiosity", ylab = "# Internal Details")
summary(Rep_externalmodel)

# Make table for objective detail analyses - Study 1
sjPlot::tab_model(S1_internalmodel, S1_externalmodel)
sjPlot::tab_model(Rep_internalmodel, Rep_externalmodel)

####  Multiple Comparisons Correction (fdr) for Study 1 Objective Detail Analyses

# Intercept
S1_obj_IntMain <- c(summary(S1_internalmodel)$coefficients[1,5], summary(S1_externalmodel)$coefficients[1,5])
S1_obj_IntMain
round(p.adjust(S1_obj_IntMain, method = "fdr", n = length (S1_obj_IntMain)),3)

# Main effect of grandiosity
S1_obj_GranMain <- c(summary(S1_internalmodel)$coefficients[2,5], summary(S1_externalmodel)$coefficients[2,5])
S1_obj_GranMain
round(p.adjust(S1_obj_GranMain , method = "fdr", n = length (S1_obj_GranMain)),3)

# Main effect of valence
S1_obj_ValMain <- c(summary(S1_internalmodel)$coefficients[3,5], summary(S1_externalmodel)$coefficients[3,5])
S1_obj_ValMain
round(p.adjust(S1_obj_ValMain, method = "fdr", n = length (S1_obj_ValMain )),3)

# Main effect of task
S1_obj_TaskMain <- c(summary(S1_internalmodel)$coefficients[4,5], summary(S1_externalmodel)$coefficients[4,5])
S1_obj_TaskMain
round(p.adjust(S1_obj_TaskMain , method = "fdr", n = length (S1_obj_TaskMain)),3)

# Interaction of grandiosity and valence
S1_obj_GranxVal <- c(summary(S1_internalmodel)$coefficients[5,5], summary(S1_externalmodel)$coefficients[5,5])
S1_obj_GranxVal
round(p.adjust(S1_obj_GranxVal, method = "fdr", n = length(S1_obj_GranxVal)),3)

# Interaction of grandiosity and task
S1_obj_GranxTas <- c(summary(S1_internalmodel)$coefficients[6,5], summary(S1_externalmodel)$coefficients[6,5])
S1_obj_GranxTas
round(p.adjust(S1_obj_GranxTas, method = "fdr", n = length(S1_obj_GranxTas)),3)

# Interaction of valence and task
S1_obj_ValxTas <- c(summary(S1_internalmodel)$coefficients[7,5], summary(S1_externalmodel)$coefficients[7,5])
S1_obj_ValxTas
round(p.adjust(S1_obj_ValxTas , method = "fdr", n = length(S1_obj_ValxTas)),3)

# Interaction of grandiosity, valence, and task
S1_obj_GranxValxTas <- c(summary(S1_internalmodel)$coefficients[8,5], summary(S1_externalmodel)$coefficients[8,5])
S1_obj_GranxValxTas
round(p.adjust(S1_obj_GranxValxTas, method = "fdr", n = length(S1_obj_GranxValxTas)),3)

#### Multiple Comparisons Correction (fdr) for Replication Objective Detail Analyses

# Intercept
Rep_obj_IntMain <- c(summary(Rep_internalmodel)$coefficients[1,5], summary(Rep_externalmodel)$coefficients[1,5])
Rep_obj_IntMain
round(p.adjust(Rep_obj_IntMain, method = "fdr", n = length (Rep_obj_IntMain)),3)

# Main effect of grandiosity
Rep_obj_GranMain <- c(summary(Rep_internalmodel)$coefficients[2,5], summary(Rep_externalmodel)$coefficients[2,5])
Rep_obj_GranMain
p.adjust(Rep_obj_GranMain , method = "fdr", n = length (Rep_obj_GranMain))

# Main effect of valence
Rep_obj_ValMain <- c(summary(Rep_internalmodel)$coefficients[3,5], summary(Rep_externalmodel)$coefficients[3,5])
Rep_obj_ValMain
round(p.adjust(Rep_obj_ValMain, method = "fdr", n = length (Rep_obj_ValMain)),3)

# Main effect of task
Rep_obj_TaskMain <- c(summary(Rep_internalmodel)$coefficients[4,5], summary(Rep_externalmodel)$coefficients[4,5])
Rep_obj_TaskMain
round(p.adjust(Rep_obj_TaskMain , method = "fdr", n = length (Rep_obj_TaskMain)),3)

# Main effect of emotional tone
Rep_obj_ToneMain <- c(summary(Rep_internalmodel)$coefficients[5,5], summary(Rep_externalmodel)$coefficients[5,5])
Rep_obj_ToneMain
round(p.adjust(Rep_obj_ToneMain , method = "fdr", n = length (Rep_obj_ToneMain)),3)

# Interaction of grandiosity and valence
Rep_obj_GranxVal <- c(summary(Rep_internalmodel)$coefficients[6,5], summary(Rep_externalmodel)$coefficients[6,5])
Rep_obj_GranxVal
round(p.adjust(Rep_obj_GranxVal, method = "fdr", n = length(Rep_obj_GranxVal)),3)

# Interaction of grandiosity and task
Rep_obj_GranxTas <- c(summary(Rep_internalmodel)$coefficients[7,5], summary(Rep_externalmodel)$coefficients[7,5])
Rep_obj_GranxTas
round(p.adjust(Rep_obj_GranxTas, method = "fdr", n = length(Rep_obj_GranxTas)),3)

# Interaction of valence and task
Rep_obj_ValxTas <- c(summary(Rep_internalmodel)$coefficients[8,5], summary(Rep_externalmodel)$coefficients[8,5])
Rep_obj_ValxTas
round(p.adjust(Rep_obj_ValxTas , method = "fdr", n = length(Rep_obj_ValxTas)),3)

# Interaction of grandiosity, valence, and task
Rep_obj_GranxValxTas <- c(summary(Rep_internalmodel)$coefficients[9,5], summary(Rep_externalmodel)$coefficients[9,5])
Rep_obj_GranxValxTas
round(p.adjust(Rep_obj_GranxValxTas, method = "fdr", n = length(Rep_obj_GranxValxTas)),3)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model assumption checks ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We here test model assumptions of linearity, normality of residuals, and homoscedasticity for each of 
#     6 subjective detail models (emotional tone, difficulty, vividness, visual perspective, frequency,
#     and plausibility) and 2 objective detail models (internal and external detail) across Study 1 and the
#     replication.

### Emotional tone

## Study 1

# Linearity
plot(resid(S1_toneModel),#extract the residuals
     S1_fulldf$eventEmotionalTone[!is.na(S1_fulldf$eventEmotionalTone)])

# Normality of residuals
S1_tone_assumption <- plot_model(S1_toneModel, type='diag')
S1_tone_assumption[[1]]
S1_tone_assumption[[2]]$subjectID
S1_tone_assumption[[2]]$stim
S1_tone_assumption[[3]]

# homoscedasticity
S1_tone_assumption[[4]] 

## Replication

# Linearity
plot(resid(Rep_tonemodel),#extract the residuals
     Rep_fulldf$eventEmotionalTone) #specify original y variable

# Normality of residuals
Rep_tone_assumption <- plot_model(Rep_tonemodel, type='diag')
Rep_tone_assumption[[1]]
Rep_tone_assumption[[2]]$subjectID
Rep_tone_assumption[[2]]$stim
Rep_tone_assumption[[3]]

# homoscedasticity
Rep_tone_assumption[[4]] 

### Difficulty

## Study 1

# Linearity
plot(resid(S1_difficultmodel),#extract the residuals
     S1_fulldf$eventDifficulty) #specify original y variable

S1_diff_assumption <- plot_model(S1_difficultmodel, type='diag') 
S1_diff_assumption[[1]]
S1_diff_assumption[[2]]$subjectID
S1_diff_assumption[[2]]$stim
S1_diff_assumption[[3]]

# homoscedasticity
S1_diff_assumption[[4]] 

## Replication

# Linearity
plot(resid(Rep_difficultmodel),#extract the residuals
     Rep_fulldf$eventDifficulty) #specify original y variable

Rep_diff_assumption <- plot_model(Rep_difficultmodel, type='diag') 
Rep_diff_assumption[[1]]
Rep_diff_assumption[[2]]$subjectID
Rep_diff_assumption[[2]]$stim
Rep_diff_assumption[[3]]

# homoscedasticity
Rep_diff_assumption[[4]] 

### Vividness
## Study 1

# Linearity
plot(resid(S1_vividmodel),#extract the residuals
     S1_fulldf$eventVividness[!is.na(S1_fulldf$eventVividness)])

S1_vivid_assumption <- plot_model(S1_vividmodel, type='diag') 
S1_vivid_assumption[[1]]
S1_vivid_assumption[[2]]$subjectID
S1_vivid_assumption[[2]]$stim
S1_vivid_assumption[[3]]

# homoscedasticity
S1_vivid_assumption[[4]] 

## Replication

# Linearity
plot(resid(Rep_vividmodel),#extract the residuals
     Rep_fulldf$eventVividness[!is.na(Rep_fulldf$eventVividness)])

Rep_vivid_assumption <- plot_model(Rep_vividmodel, type='diag') 
Rep_vivid_assumption[[1]]
Rep_vivid_assumption[[2]]$subjectID
Rep_vivid_assumption[[2]]$stim
Rep_vivid_assumption[[3]]

# homoscedasticity
Rep_vivid_assumption[[4]] 

### Visual perspective
## Study 1

# Linearity
plot(resid(S1_perspectmodel),#extract the residuals
     S1_fulldf$eventVisPerspective[!is.na(S1_fulldf$eventVisPerspective)])

S1_visPerspect_assumption <- plot_model(S1_perspectmodel, type='diag') 
S1_visPerspect_assumption[[1]]
S1_visPerspect_assumption[[2]]$subjectID
S1_visPerspect_assumption[[2]]$stim
S1_visPerspect_assumption[[3]]

# homoscedasticity
S1_visPerspect_assumption[[4]] 

## Replication

# Linearity
plot(resid(Rep_perspectmodel),
     Rep_fulldf$eventVisPerspective) 

Rep_visPerspect_assumption <- plot_model(Rep_perspectmodel, type='diag') 
Rep_visPerspect_assumption[[1]]
Rep_visPerspect_assumption[[2]]$subjectID
Rep_visPerspect_assumption[[2]]$stim
Rep_visPerspect_assumption[[3]]

# homoscedasticity
Rep_visPerspect_assumption[[4]] 

### Frequency of thinking about about before

# Linearity
plot(resid(S1_freqmodel),#extract the residuals
     S1_fulldf$eventThoughtAboutBefore[!is.na(S1_fulldf$eventThoughtAboutBefore)])

S1_freq_assumption <- plot_model(S1_freqmodel, type='diag') 
S1_freq_assumption[[1]]
S1_freq_assumption[[2]]$subjectID
S1_freq_assumption[[2]]$stim
S1_freq_assumption[[3]]

# homoscedasticity
S1_freq_assumption[[4]] 

## Replication

# Linearity
plot(resid(Rep_freqmodel),#extract the residuals
     Rep_fulldf$eventThoughtAboutBefore) #specify original y variable

Rep_freq_assumption <- plot_model(Rep_freqmodel, type='diag') 
Rep_freq_assumption[[1]]
Rep_freq_assumption[[2]]$subjectID
Rep_freq_assumption[[2]]$stim
Rep_freq_assumption[[3]]

# homoscedasticity
Rep_freq_assumption[[4]] 

### Plausibility

## Study 1

# Linearity
plot(resid(S1_plausmodel),
     S1_futdf$eventPlausibility[!is.na(S1_futdf$eventPlausibility)], ylab = "Plausibility Rating")

S1_plaus_assumption <- plot_model(S1_plausmodel, type='diag')
S1_plaus_assumption[[1]]
S1_plaus_assumption[[2]]$subjectID
S1_plaus_assumption[[2]]$stim
S1_plaus_assumption[[3]]

# homoscedasticity
S1_plaus_assumption[[4]] 

## Replication

# Linearity
plot(resid(Rep_plausmodel),#extract the residuals
     Rep_futdf$eventPlausibility)

Rep_plaus_assumption <- plot_model(Rep_plausmodel, type='diag')
Rep_plaus_assumption[[1]]
Rep_plaus_assumption[[2]]$subjectID
Rep_plaus_assumption[[2]]$stim
Rep_plaus_assumption[[3]]

# homoscedasticity
Rep_plaus_assumption[[4]]

#### Objective detail

## Study 1
# Internals
# Linearity
plot(resid(S1_internalmodel),#extract the residuals
     S1_fulldf$numInternalDetails) #specify original y variable

S1_int_assumption <- plot_model(S1_internalmodel, type='diag') 
S1_int_assumption[[1]]
S1_int_assumption[[2]]$subjectID
S1_int_assumption[[2]]$stim
S1_int_assumption[[3]]

# homoscedasticity
S1_int_assumption[[4]] 

# Externals
# Linearity
plot(resid(S1_externalmodel),#extract the residuals
     S1_fulldf$numExternalDetails) #specify original y variable

S1_ext_assumption <- plot_model(S1_externalmodel, type='diag') 
S1_ext_assumption[[1]]
S1_ext_assumption[[2]]$subjectID
S1_ext_assumption[[2]]$stim
S1_ext_assumption[[3]]

# homoscedasticity
S1_ext_assumption[[4]] 

## Replication
# Internals

# Linearity
plot(resid(Rep_internalmodel),
     Rep_fulldf$numInternalDetails) 

Rep_int_assumption <- plot_model(Rep_internalmodel, type='diag') 
Rep_int_assumption[[1]]
Rep_int_assumption[[2]]$subjectID
Rep_int_assumption[[2]]$stim
Rep_int_assumption[[3]]

# homoscedasticity
Rep_int_assumption[[4]] 

# Externals
# Linearity
plot(resid(Rep_externalmodel),#extract the residuals
     Rep_fulldf$numExternalDetails) #specify original y variable

Rep_ext_assumption <- plot_model(S1_externalmodel, type='diag') 
Rep_ext_assumption[[1]]
Rep_ext_assumption[[2]]$subjectID
Rep_ext_assumption[[2]]$stim
Rep_ext_assumption[[3]]

# homoscedasticity
Rep_ext_assumption[[4]] 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Agentic vs. Communal Post-Hoc Analyses ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# In this section, we assess whether the agency of the cue word impacts our interaction effects.

# STUDY 1
kinddata <- S1_fulldf[S1_fulldf$stim == "kind",]
meandata <- S1_fulldf[S1_fulldf$stim == "mean",]
smartdata <- S1_fulldf[S1_fulldf$stim == "smart",]
stupiddata <- S1_fulldf[S1_fulldf$stim == "stupid",]

S1exploratorydata <- rbind(kinddata, meandata, smartdata, stupiddata)
table(S1exploratorydata$stim)

S1exploratorydata$stimtype <- "agentic"
S1exploratorydata$stimtype[S1exploratorydata$stim == "kind"] <- "communal"
S1exploratorydata$stimtype[S1exploratorydata$stim == "mean"] <- "communal"
table(S1exploratorydata$stimtype)

### Difficulty
S1_difficultmodel_stimtype <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*stimtype +(1|subjectID), data = S1exploratorydata)
summary(S1_difficultmodel_stimtype)

# Vividness
S1_vividmodel_stimtype <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*stimtype +(1|subjectID), data = S1exploratorydata)
summary(S1_vividmodel_stimtype)

# Visual perspective
S1_perspectmodel_stimtype <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*stimtype + (1|subjectID), data = S1exploratorydata)
summary(S1_perspectmodel_stimtype)
# sig at the 0.020

# Plausibility
S1_plausmodel_stimtype <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence*stimtype +(1|subjectID), data = S1exploratorydata)
summary(S1_plausmodel_stimtype)

# Thought about before
S1_freqmodel_stimtype <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*stimtype + (1|subjectID), data = S1exploratorydata)
summary(S1_freqmodel_stimtype)

tab_model(S1_difficultmodel_stimtype, S1_vividmodel_stimtype, S1_perspectmodel_stimtype, 
          S1_plausmodel_stimtype, S1_freqmodel_stimtype)

## Study 1 Multiple Comparisons Correction (fdr) for S1 Agentic/Communal Exploration
# Order is difficulty, vividness, visual perspective, frequency, plausibility.

# Intercept
S1p_IntMain_st <- c(summary(S1_difficultmodel_stimtype)$coefficients[1,5], summary(S1_vividmodel_stimtype)$coefficients[1,5], summary(S1_perspectmodel_stimtype)$coefficients[1,5],
                   summary(S1_freqmodel_stimtype)$coefficients[1,5], summary(S1_plausmodel_stimtype)$coefficients[1,5])
S1p_IntMain_st
round(p.adjust(S1p_IntMain_st, method = "fdr", n = length (S1p_IntMain_st)),3)

# Main effect of grandiosity
S1p_GranMain_st <- c(summary(S1_difficultmodel_stimtype)$coefficients[2,5], summary(S1_vividmodel_stimtype)$coefficients[2,5], summary(S1_perspectmodel_stimtype)$coefficients[2,5],
                   summary(S1_freqmodel_stimtype)$coefficients[2,5], summary(S1_plausmodel_stimtype)$coefficients[2,5])
S1p_GranMain_st
round(p.adjust(S1p_GranMain_st, method = "fdr", n = length (S1p_GranMain_st)),3)

# Main effect of valence
S1p_ValMain_st <- c(summary(S1_difficultmodel_stimtype)$coefficients[3,5], summary(S1_vividmodel_stimtype)$coefficients[3,5], summary(S1_perspectmodel_stimtype)$coefficients[3,5],
             summary(S1_freqmodel_stimtype)$coefficients[3,5],  summary(S1_plausmodel_stimtype)$coefficients[3,5])
S1p_ValMain_st
round(p.adjust(S1p_ValMain_st, method = "fdr", n = length (S1p_ValMain_st)),3)

# Main effect of stim type
S1p_StimTypeMain_st <- c(summary(S1_difficultmodel_stimtype)$coefficients[4,5], summary(S1_vividmodel_stimtype)$coefficients[4,5], summary(S1_perspectmodel_stimtype)$coefficients[4,5],
                   summary(S1_freqmodel_stimtype)$coefficients[4,5],summary(S1_plausmodel_stimtype)$coefficients[4,5])
S1p_StimTypeMain_st
round(p.adjust(S1p_StimTypeMain_st, method = "fdr", n = length (S1p_StimTypeMain_st)),3)

# Interaction of grandiosity and valence
S1p_GranxVal_st<- c(summary(S1_difficultmodel_stimtype)$coefficients[5,5], summary(S1_vividmodel_stimtype)$coefficients[5,5], summary(S1_perspectmodel_stimtype)$coefficients[5,5],
                  summary(S1_freqmodel_stimtype)$coefficients[5,5],  summary(S1_plausmodel_stimtype)$coefficients[5,5])
S1p_GranxVal_st
round(p.adjust(S1p_GranxVal_st, method = "fdr", n = length (S1p_GranxVal_st)),3)

# Interaction of grandiosity and stim type
S1p_GranxStimType_st  <- c(summary(S1_difficultmodel_stimtype)$coefficients[6,5], summary(S1_vividmodel_stimtype)$coefficients[6,5], summary(S1_perspectmodel_stimtype)$coefficients[6,5],
                   summary(S1_plausmodel_stimtype)$coefficients[5,5], summary(S1_freqmodel_stimtype)$coefficients[6,5])
S1p_GranxStimType_st 
round(p.adjust(S1p_GranxStimType_st , method = "fdr", n = length(S1p_GranxStimType_st )),3)

# Interaction of valence and stim type
S1p_ValxStimType_st <- c(summary(S1_difficultmodel_stimtype)$coefficients[7,5], summary(S1_vividmodel_stimtype)$coefficients[7,5], summary(S1_perspectmodel_stimtype)$coefficients[7,5],
                         summary(S1_plausmodel_stimtype)$coefficients[7,5], summary(S1_freqmodel_stimtype)$coefficients[7,5])
S1p_ValxStimType_st
round(p.adjust(S1p_ValxStimType_st, method = "fdr", n = length(S1p_ValxStimType_st)),3)

# Interaction of grandiosity, valence, and stim type
S1p_GranxValxStimType_st <- c(summary(S1_difficultmodel_stimtype)$coefficients[8,5], summary(S1_vividmodel_stimtype)$coefficients[8,5], summary(S1_perspectmodel_stimtype)$coefficients[8,5],
                              summary(S1_plausmodel_stimtype)$coefficients[7,5], summary(S1_freqmodel_stimtype)$coefficients[8,5])
S1p_GranxValxStimType_st
round(p.adjust(S1p_GranxValxStimType_st, method = "fdr", n = length(S1p_GranxValxStimType_st)),3)

# REPLICATION 

Rep_kinddata <- Rep_fulldf[Rep_fulldf$stim == "kind",]
Rep_meandata <- Rep_fulldf[Rep_fulldf$stim == "mean",]
Rep_smartdata <- Rep_fulldf[Rep_fulldf$stim == "smart",]
Rep_stupiddata <- Rep_fulldf[Rep_fulldf$stim == "stupid",]

Repexploratorydata <- rbind(Rep_kinddata, Rep_meandata, Rep_smartdata,Rep_stupiddata)
table(Repexploratorydata$stim)

Repexploratorydata$stimtype <- "agentic"
Repexploratorydata$stimtype[Repexploratorydata$stim == "kind"] <- "communal"
Repexploratorydata$stimtype[Repexploratorydata$stim == "mean"] <- "communal"
table(Repexploratorydata$stimtype)

### Difficulty
Rep_difficultmodel_stimtype <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*stimtype + eventEmotionalTone + (1|subjectID), data = Repexploratorydata)
summary(Rep_difficultmodel_stimtype)

# Vividness
Rep_vividmodel_stimtype <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*stimtype + eventEmotionalTone + (1|subjectID), data = Repexploratorydata)
summary(Rep_vividmodel_stimtype)

# Visual perspective
Rep_perspectmodel_stimtype <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*stimtype + eventEmotionalTone + (1|subjectID), data = Repexploratorydata)
summary(Rep_perspectmodel_stimtype)

# Plausibility
Rep_plausmodel_stimtype <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence*stimtype + eventEmotionalTone + (1|subjectID), data = Repexploratorydata)
summary(Rep_plausmodel_stimtype)

# Thought about before
Rep_freqmodel_stimtype <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*stimtype + eventEmotionalTone + (1|subjectID), data = Repexploratorydata)
summary(Rep_freqmodel_stimtype)

tab_model(Rep_difficultmodel_stimtype, Rep_vividmodel_stimtype, Rep_perspectmodel_stimtype, 
          Rep_freqmodel_stimtype, Rep_plausmodel_stimtype)

## Replication Multiple Comparisons Correction (fdr) for S1 Agentic/Communal Exploration
# Order is difficulty, vividness, visual perspective, frequency, plausibility.

# Intercept
Repp_IntMain_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[1,5], summary(Rep_vividmodel_stimtype)$coefficients[1,5], summary(Rep_perspectmodel_stimtype)$coefficients[1,5],
                    summary(Rep_freqmodel_stimtype)$coefficients[1,5], summary(Rep_plausmodel_stimtype)$coefficients[1,5])
Repp_IntMain_st
round(p.adjust(Repp_IntMain_st, method = "fdr", n = length (Repp_IntMain_st)),3)

# Main effect of grandiosity
Repp_GranMain_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[2,5], summary(Rep_vividmodel_stimtype)$coefficients[2,5], summary(Rep_perspectmodel_stimtype)$coefficients[2,5],
                     summary(Rep_freqmodel_stimtype)$coefficients[2,5], summary(Rep_plausmodel_stimtype)$coefficients[2,5])
Repp_GranMain_st
round(p.adjust(Repp_GranMain_st, method = "fdr", n = length (Repp_GranMain_st)),3)

# Main effect of valence
Repp_ValMain_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[3,5], summary(Rep_vividmodel_stimtype)$coefficients[3,5], summary(Rep_perspectmodel_stimtype)$coefficients[3,5],
                    summary(Rep_freqmodel_stimtype)$coefficients[3,5],  summary(Rep_plausmodel_stimtype)$coefficients[3,5])
Repp_ValMain_st
round(p.adjust(Repp_ValMain_st, method = "fdr", n = length (Repp_ValMain_st)),3)

# Main effect of stim type
Repp_StimTypeMain_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[4,5], summary(Rep_vividmodel_stimtype)$coefficients[4,5], summary(Rep_perspectmodel_stimtype)$coefficients[4,5],
                         summary(Rep_freqmodel_stimtype)$coefficients[4,5],summary(Rep_plausmodel_stimtype)$coefficients[4,5])
Repp_StimTypeMain_st
round(p.adjust(Repp_StimTypeMain_st, method = "fdr", n = length (Repp_StimTypeMain_st)),3)

# Main Effect of Emo Tone
Repp_ToneMain_st<- c(summary(Rep_difficultmodel_stimtype)$coefficients[5,5], summary(Rep_vividmodel_stimtype)$coefficients[5,5], summary(Rep_perspectmodel_stimtype)$coefficients[5,5],
                    summary(Rep_freqmodel_stimtype)$coefficients[5,5],  summary(Rep_plausmodel_stimtype)$coefficients[5,5])
Repp_ToneMain_st
round(p.adjust(Repp_ToneMain_st, method = "fdr", n = length (Repp_ToneMain_st)),3)

# Interaction of grandiosity and valence
Repp_GranxVal_st  <- c(summary(Rep_difficultmodel_stimtype)$coefficients[6,5], summary(Rep_vividmodel_stimtype)$coefficients[6,5], summary(Rep_perspectmodel_stimtype)$coefficients[6,5],
                           summary(Rep_plausmodel_stimtype)$coefficients[5,5], summary(Rep_freqmodel_stimtype)$coefficients[6,5])
Repp_GranxVal_st
round(p.adjust(Repp_GranxVal_st, method = "fdr", n = length(Repp_GranxVal_st)),3)

# Interaction of grandiosity and stim type
Repp_GranxStimType_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[7,5], summary(Rep_vividmodel_stimtype)$coefficients[7,5], summary(Rep_perspectmodel_stimtype)$coefficients[7,5],
                         summary(Rep_plausmodel_stimtype)$coefficients[7,5], summary(Rep_freqmodel_stimtype)$coefficients[7,5])
Repp_GranxStimType_st
round(p.adjust(Repp_GranxStimType_st, method = "fdr", n = length(Repp_GranxStimType_st)),3)

# Interaction of valence and stim type
Repp_ValxStimType_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[8,5], summary(Rep_vividmodel_stimtype)$coefficients[8,5], summary(Rep_perspectmodel_stimtype)$coefficients[8,5],
                              summary(Rep_plausmodel_stimtype)$coefficients[7,5], summary(Rep_freqmodel_stimtype)$coefficients[8,5])
Repp_ValxStimType_st
round(p.adjust(Repp_ValxStimType_st, method = "fdr", n = length(Repp_ValxStimType_st)),3)

# Interaction of grandiosity, valence, and stim type
Repp_GranxValxStimType_st <- c(summary(Rep_difficultmodel_stimtype)$coefficients[9,5], summary(Rep_vividmodel_stimtype)$coefficients[9,5], summary(Rep_perspectmodel_stimtype)$coefficients[9,5],
                            summary(Rep_plausmodel_stimtype)$coefficients[9,5], summary(Rep_freqmodel_stimtype)$coefficients[9,5])
Repp_GranxValxStimType_st
round(p.adjust(Repp_GranxValxStimType_st, method = "fdr", n = length(Repp_GranxValxStimType_st)),3)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Checking for Influence of Covariates ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We here systematically check whether including any of the below covariates (depression, anxiety, self-esteem, 
#     individual differences in visual imagery, or fantasy-proneness) impacts our results.

#STUDY1

#DEPRESSION
# Difficulty
S1_difficultmodel_DEPRESSION <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + DASS_depression + (1|stim) + (1|subjectID), data = S1_fulldf)

# Vividness
S1_vividmodel_DEPRESSION <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + DASS_depression + (1|stim) + (1|subjectID), data = S1_fulldf)

# Visual perspective
S1_perspectmodel_DEPRESSION <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + DASS_depression +(1|stim) + (1|subjectID), data = S1_fulldf)

# Plausibility
S1_plausmodel_DEPRESSION <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + DASS_depression +(1|stim) + (1|subjectID), data = S1_futdf)

# Thought about before
S1_beforemodel_DEPRESSION <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence + DASS_depression +(1|stim) + (1|subjectID), data = S1_futdf)

sjPlot::tab_model(S1_difficultmodel_DEPRESSION, S1_vividmodel_DEPRESSION, S1_perspectmodel_DEPRESSION, S1_plausmodel_DEPRESSION, S1_beforemodel_DEPRESSION)
# fully replicates

#ANXIETY
# Difficulty
S1_difficultmodel_ANXIETY <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + DASS_anxiety + (1|stim) + (1|subjectID), data = S1_fulldf)

# Vividness
S1_vividmodel_ANXIETY <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + DASS_anxiety + (1|stim) + (1|subjectID), data = S1_fulldf)

# Visual perspective
S1_perspectmodel_ANXIETY <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + DASS_anxiety +(1|stim) + (1|subjectID), data = S1_fulldf)

# Plausibility
S1_plausmodel_ANXIETY <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + DASS_anxiety +(1|stim) + (1|subjectID), data = S1_futdf)

# Thought about before
S1_beforemodel_ANXIETY <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence + DASS_anxiety +(1|stim) + (1|subjectID), data = S1_futdf)

sjPlot::tab_model(S1_difficultmodel_ANXIETY, S1_vividmodel_ANXIETY, S1_perspectmodel_ANXIETY, S1_plausmodel_ANXIETY, S1_beforemodel_ANXIETY)
# fully replicates

#SELF-ESTEEM
# Difficulty
S1_difficultmodel_ESTEEM <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + RSES_total + (1|stim) + (1|subjectID), data = S1_fulldf)

# Vividness
S1_vividmodel_ESTEEM <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + RSES_total + (1|stim) + (1|subjectID), data = S1_fulldf)

# Visual perspective
S1_perspectmodel_ESTEEM <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + RSES_total +(1|stim) + (1|subjectID), data = S1_fulldf)

# Plausibility
S1_plausmodel_ESTEEM <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + RSES_total +(1|stim) + (1|subjectID), data = S1_futdf)

# Thought about before
S1_beforemodel_ESTEEM <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence + RSES_total +(1|stim) + (1|subjectID), data = S1_futdf)

sjPlot::tab_model(S1_difficultmodel_ESTEEM, S1_vividmodel_ESTEEM, S1_perspectmodel_ESTEEM, S1_plausmodel_ESTEEM, S1_beforemodel_ESTEEM)
# fully replicates

#VVIQ
S1_fulldf$CEQ_total
# Difficulty
S1_difficultmodel_VVIQ <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + VVIQ_total + (1|stim) + (1|subjectID), data = S1_fulldf)

# Vividness
S1_vividmodel_VVIQ <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + VVIQ_total + (1|stim) + (1|subjectID), data = S1_fulldf)

# Visual perspective
S1_perspectmodel_VVIQ <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + VVIQ_total +(1|stim) + (1|subjectID), data = S1_fulldf)

# Plausibility
S1_plausmodel_VVIQ <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + VVIQ_total +(1|stim) + (1|subjectID), data = S1_futdf)

# Thought about before
S1_beforemodel_VVIQ <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence + VVIQ_total +(1|stim) + (1|subjectID), data = S1_futdf)

sjPlot::tab_model(S1_difficultmodel_VVIQ, S1_vividmodel_VVIQ, S1_perspectmodel_VVIQ, S1_plausmodel_VVIQ, S1_beforemodel_VVIQ)
# fully replicates

#CEQ
# Difficulty
S1_difficultmodel_CEQ <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + CEQ_total + (1|stim) + (1|subjectID), data = S1_fulldf)

# Vividness
S1_vividmodel_CEQ <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + CEQ_total + (1|stim) + (1|subjectID), data = S1_fulldf)

# Visual perspective
S1_perspectmodel_CEQ <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + CEQ_total +(1|stim) + (1|subjectID), data = S1_fulldf)

# Plausibility
S1_plausmodel_CEQ <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + CEQ_total +(1|stim) + (1|subjectID), data = S1_futdf)

# Thought about before
S1_beforemodel_CEQ <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence + CEQ_total +(1|stim) + (1|subjectID), data = S1_futdf)

sjPlot::tab_model(S1_difficultmodel_CEQ, S1_vividmodel_CEQ, S1_perspectmodel_CEQ, S1_plausmodel_CEQ, S1_beforemodel_CEQ)
# fully replicates


# REPLICATION 

#DEPRESSION
# Difficulty
Rep_difficultmodel_DEPRESSION <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + DASS_depression + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Vividness
Rep_vividmodel_DEPRESSION <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ DASS_depression + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Visual perspective
Rep_perspectmodel_DEPRESSION <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ DASS_depression +(1|stim) + (1|subjectID), data = Rep_fulldf)

# Plausibility
Rep_plausmodel_DEPRESSION <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + eventEmotionalTone+ DASS_depression +(1|stim) + (1|subjectID), data = Rep_futdf)

# Thought about before
Rep_beforemodel_DEPRESSION <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone + DASS_depression +(1|stim) + (1|subjectID), data = Rep_fulldf)

sjPlot::tab_model(Rep_difficultmodel_DEPRESSION, Rep_vividmodel_DEPRESSION, Rep_perspectmodel_DEPRESSION, Rep_plausmodel_DEPRESSION, Rep_beforemodel_DEPRESSION)
# fully replicates

#ANXIETY
# Difficulty
Rep_difficultmodel_ANXIETY <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ DASS_anxiety + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Vividness
Rep_vividmodel_ANXIETY <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ DASS_anxiety + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Visual perspective
Rep_perspectmodel_ANXIETY <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ DASS_anxiety +(1|stim) + (1|subjectID), data = Rep_fulldf)

# Plausibility
Rep_plausmodel_ANXIETY <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + eventEmotionalTone+ DASS_anxiety +(1|stim) + (1|subjectID), data = Rep_futdf)

# Thought about before
Rep_beforemodel_ANXIETY <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*taskName+ eventEmotionalTone+ DASS_anxiety +(1|stim) + (1|subjectID), data = Rep_fulldf)

sjPlot::tab_model(Rep_difficultmodel_ANXIETY, Rep_vividmodel_ANXIETY, Rep_perspectmodel_ANXIETY, Rep_plausmodel_ANXIETY, Rep_beforemodel_ANXIETY)
# fully replicates

#SELF-ESTEEM
# Difficulty
Rep_difficultmodel_ESTEEM <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ RSES_total + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Vividness
Rep_vividmodel_ESTEEM <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ RSES_total + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Visual perspective
Rep_perspectmodel_ESTEEM <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ RSES_total +(1|stim) + (1|subjectID), data = Rep_fulldf)

# Plausibility
Rep_plausmodel_ESTEEM <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + eventEmotionalTone+ RSES_total +(1|stim) + (1|subjectID), data = Rep_futdf)

# Thought about before
Rep_beforemodel_ESTEEM <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ RSES_total +(1|stim) + (1|subjectID), data = Rep_fulldf)

sjPlot::tab_model(Rep_difficultmodel_ESTEEM, Rep_vividmodel_ESTEEM, Rep_perspectmodel_ESTEEM, Rep_plausmodel_ESTEEM, Rep_beforemodel_ESTEEM)
# fully replicates

#VVIQ
# Difficulty
Rep_difficultmodel_VVIQ <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ VVIQ_total + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Vividness
Rep_vividmodel_VVIQ <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName+ eventEmotionalTone + VVIQ_total + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Visual perspective
Rep_perspectmodel_VVIQ <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ VVIQ_total +(1|stim) + (1|subjectID), data = Rep_fulldf)

# Plausibility
Rep_plausmodel_VVIQ <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + eventEmotionalTone+ VVIQ_total +(1|stim) + (1|subjectID), data = Rep_futdf)

# Thought about before
Rep_beforemodel_VVIQ <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ VVIQ_total +(1|stim) + (1|subjectID), data = Rep_fulldf)

sjPlot::tab_model(Rep_difficultmodel_VVIQ, Rep_vividmodel_VVIQ, Rep_perspectmodel_VVIQ, Rep_plausmodel_VVIQ, Rep_beforemodel_VVIQ)
# fully replicates

#CEQ
# Difficulty
Rep_difficultmodel_CEQ <- lmerTest::lmer(eventDifficulty ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ CEQ_total + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Vividness
Rep_vividmodel_CEQ <- lmerTest::lmer(eventVividness ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ CEQ_total + (1|stim) + (1|subjectID), data = Rep_fulldf)

# Visual perspective
Rep_perspectmodel_CEQ <- lmerTest::lmer(eventVisPerspective ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ CEQ_total +(1|stim) + (1|subjectID), data = Rep_fulldf)

# Plausibility
Rep_plausmodel_CEQ <- lmerTest::lmer(eventPlausibility ~ FFNI_Grandiose*valence + eventEmotionalTone+ CEQ_total +(1|stim) + (1|subjectID), data = Rep_futdf)

# Thought about before
Rep_beforemodel_CEQ <- lmerTest::lmer(eventThoughtAboutBefore ~ FFNI_Grandiose*valence*taskName + eventEmotionalTone+ CEQ_total +(1|stim) + (1|subjectID), data = Rep_fulldf)

sjPlot::tab_model(Rep_difficultmodel_CEQ, Rep_vividmodel_CEQ, Rep_perspectmodel_CEQ, Rep_plausmodel_CEQ, Rep_beforemodel_CEQ)
# fully replicates

