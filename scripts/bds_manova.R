## LOAD PACKAGES ####
library(pcaMethods)


## READ IN DATA ####
bds = read.table("data/bds.txt", header=T, sep="\t")


## ORGANIZE DATA BY SUBTYPE ####
# Ages
bds_ages = data.matrix(subset(bds, select=c(age_english, school_english, comfort_english, comfort_spanish, age_spanish, school_spanish)))

	# English
bds_ages_e = data.matrix(subset(bds, select=c(age_english, comfort_english, school_english)))

	# Spanish
bds_ages_s = data.matrix(subset(bds, select=c(age_spanish, comfort_spanish, school_spanish)))

# Preference
bds_pref = data.matrix(subset(bds, select=c(use_predominantly, use_math, accent, use_life, lost_fluency)))


## RUN MANOVAS ####
bds_ages.manova = manova(bds_ages ~ bds$form_lg)
summary(bds_ages.manova)
	bds_ages_e.manova = manova(bds_ages_e ~ bds$form_lg)
	summary(bds_ages_e.manova)
	
	bds_ages_s.manova = manova(bds_ages_s ~ bds$form_lg)
	summary(bds_ages_s.manova)

bds_pref.manova = manova(bds_pref ~ bds$form_lg)
summary(bds_pref.manova)


















