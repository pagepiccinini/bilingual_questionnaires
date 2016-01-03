## LOAD PACKAGES ####
library(pcaMethods)


## READ IN DATA ####
bds = read.table("data/bds.txt", header=T, sep="\t")


## ORGANIZE DATA BY SUBTYPE ####
# Demographic information (not include sex and age because not actually a "choice")
bds_demo = subset(bds, select=c(black, white))

# Ages
bds_ages = subset(bds, select=c(age_english, school_english, comfort_english, comfort_spanish, age_spanish, school_spanish))

	# English
bds_ages_e = subset(bds, select=c(age_english, comfort_english, school_english))

	# Spanish
bds_ages_s = subset(bds, select=c(age_spanish, comfort_spanish, school_spanish))

# Preference
#bds_pref = subset(bds, select=c(use_predominantly, use_math, accent, use_life, lost_fluency))
bds_pref = subset(bds, select=c(use_predominantly, use_math, use_life, lost_fluency))


## RUN PPCAS ####
# Demographic information
bds_demo_pca <- prcomp(bds_demo, center=T, scale.=T)
bds_demo_summary <- summary(bds_demo_pca)
bds_demo_rotated <- as.data.frame(bds_demo_pca$x)

# Ages
bds_ages_pca <- prcomp(bds_ages, center=T, scale.=T)
bds_ages_summary <- summary(bds_ages_pca)
bds_ages_rotated <- as.data.frame(bds_ages_pca$x)

	# English
bds_ages_e_pca <- prcomp(bds_ages_e, center=T, scale.=T)
bds_ages_e_summary <- summary(bds_ages_e_pca)
bds_ages_e_rotated <- as.data.frame(bds_ages_e_pca$x)

	# Spanish
bds_ages_s_pca <- prcomp(bds_ages_s, center=T, scale.=T)
bds_ages_s_summary <- summary(bds_ages_s_pca)
bds_ages_s_rotated <- as.data.frame(bds_ages_s_pca$x)

# Preference
bds_pref_pca <- prcomp(bds_pref, center=T, scale.=T)
bds_pref_summary <- summary(bds_pref_pca)
bds_pref_rotated <- as.data.frame(bds_pref_pca$x)


## RUN REGRESSIONS

# Variables collapsed by language
bds_small.glm = glm(bds$form_lg ~ bds_ages_pca$x[,1:1] * bds_prof_sub_pca$x[,1:1])

# Variables separated by language
bds.glm = glm(bds$form_lg ~ bds_demo_pca$x[,1:1] + bds_ages_e_pca$x[,1:1] + bds_ages_s_pca$x[,1:1] + bds_exp_e_pca$x[,1:1] + bds_exp_s_pca$x[,1:1] + bds_cf_e_pca$x[,1:1] + bds_cf_s_pca$x[,1:1] + bds_prof_e_pca$x[,1:1] + bds_prof_s_pca$x[,1:1])

# MANOVA collapsed by language
bds_demo.manova = manova(bds_demo_pca$x[,1:2] ~ bds$form_lg)
summary(bds_demo.manova)

bds_ages.manova = manova(bds_ages_pca$x[,1:6] ~ bds$form_lg)
summary(bds_ages.manova)
	bds_ages_e.manova = manova(bds_ages_e_pca$x[,1:3] ~ bds$form_lg)
	summary(bds_ages_e.manova)
	
	bds_ages_s.manova = manova(bds_ages_s_pca$x[,1:3] ~ bds$form_lg)
	summary(bds_ages_s.manova)

bds_pref.manova = manova(bds_pref_pca$x[,1:4] ~ bds$form_lg)
summary(bds_pref.manova)


















