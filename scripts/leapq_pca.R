## LOAD PACKAGES ####
library(pcaMethods)


## READ IN DATA ####
leap = read.table("data/leapq.txt", header=T, sep="\t")


## ORGANIZE DATA BY SUBTYPE ####
# Demographic information (not include sex and age because not actually a "choice")
leap_demo = subset(leap, select=c(black, white))

# Ages
leap_ages = subset(leap, select=c(english, e_acquire_age, e_fluent_age, e_beganread_age, e_fluentread_age, e_country_spoken_y_m, e_family_spoken_y_m, e_work_spoken_y_m, spanish, s_acquire_age, s_fluent_age, s_beganread_age, s_fluentread_age, s_country_spoken_y_m, s_family_spoken_y_m, s_work_spoken_y_m))

	# English
leap_ages_e = subset(leap, select=c(english, e_acquire_age, e_fluent_age, e_beganread_age, e_fluentread_age, e_country_spoken_y_m, e_family_spoken_y_m, e_work_spoken_y_m))

	# Spanish
leap_ages_s = subset(leap, select=c(spanish, s_acquire_age, s_fluent_age, s_beganread_age, s_fluentread_age, s_country_spoken_y_m, s_family_spoken_y_m, s_work_spoken_y_m))

# Exposure
leap_exp = subset(leap, select=c(percent_exp_eng, percent_read_eng, percent_speak_eng, e_friends_exp, e_family_exp, e_reading_exp, e_instruction_exp, e_tv_exp, e_music_exp, percent_exp_sp, percent_read_sp, percent_speak_sp, s_friends_exp, s_family_exp, s_reading_exp, s_instruction_exp, s_tv_exp, s_music_exp))

	# English
leap_exp_e = subset(leap, select=c(percent_exp_eng, percent_read_eng, percent_speak_eng, e_friends_exp, e_family_exp, e_reading_exp, e_instruction_exp, e_tv_exp, e_music_exp))

	# Spanish
leap_exp_s = subset(leap, select=c(percent_exp_sp, percent_read_sp, percent_speak_sp, s_friends_exp, s_family_exp, s_reading_exp, s_instruction_exp, s_tv_exp, s_music_exp))

# Contributing Factors
leap_cf = subset(leap, select=c(e_friends_con, e_family_con, e_reading_con, e_instruction_con, e_tv_con, e_music_con, s_friends_con, s_family_con, s_reading_con, s_instruction_con, s_tv_con, s_music_con))

	# English
leap_cf_e = subset(leap, select=c(e_friends_con, e_family_con, e_reading_con, e_instruction_con, e_tv_con, e_music_con))

	# Spanish
leap_cf_s = subset(leap, select=c(s_friends_con, s_family_con, s_reading_con, s_instruction_con, s_tv_con, s_music_con))

# Proficiency
leap_prof = subset(leap, select=c(e_speaking_prof, e_understanding_prof, e_reading_prof, e_selfrate_accent, e_otherrate_accent, s_speaking_prof, s_understanding_prof, s_reading_prof, s_selfrate_accent, s_otherrate_accent))

	# English
leap_prof_e = subset(leap, select=c(e_speaking_prof, e_understanding_prof, e_reading_prof, e_selfrate_accent, e_otherrate_accent))

	# Spanish
leap_prof_s = subset(leap, select=c(s_speaking_prof, s_understanding_prof, s_reading_prof, s_selfrate_accent, s_otherrate_accent))


## RUN PPCAS ####
# Demographic information
leap_demo_pca <- prcomp(leap_demo, center=T, scale.=T)
leap_demo_summary <- summary(leap_demo_pca)
leap_demo_rotated <- as.data.frame(leap_demo_pca$x)

# Ages
leap_ages_pca <- prcomp(leap_ages, center=T, scale.=T)
leap_ages_summary <- summary(leap_ages_pca)
leap_ages_rotated <- as.data.frame(leap_ages_pca$x)

	# English
leap_ages_e_pca <- prcomp(leap_ages_e, center=T, scale.=T)
leap_ages_e_summary <- summary(leap_ages_e_pca)
leap_ages_e_rotated <- as.data.frame(leap_ages_e_pca$x)

	# Spanish
leap_ages_s_pca <- prcomp(leap_ages_s, center=T, scale.=T)
leap_ages_s_summary <- summary(leap_ages_s_pca)
leap_ages_s_rotated <- as.data.frame(leap_ages_s_pca$x)

# Exposure
leap_exp_pca <- prcomp(leap_exp, center=T, scale.=T)
leap_exp_summary <- summary(leap_exp_pca)
leap_exp_rotated <- as.data.frame(leap_exp_pca$x)

	# English
leap_exp_e_pca <- prcomp(leap_exp_e, center=T, scale.=T)
leap_exp_e_summary <- summary(leap_exp_e_pca)
leap_exp_e_rotated <- as.data.frame(leap_exp_e_pca$x)

	# Spanish
leap_exp_s_pca <- prcomp(leap_exp_s, center=T, scale.=T)
leap_exp_s_summary <- summary(leap_exp_s_pca)
leap_exp_s_rotated <- as.data.frame(leap_exp_s_pca$x)

# Contributing factors
leap_cf_pca <- prcomp(leap_cf, center=T, scale.=T)
leap_cf_summary <- summary(leap_cf_pca)
leap_cf_rotated <- as.data.frame(leap_cf_pca$x)

	# English
leap_cf_e_pca <- prcomp(leap_cf_e, center=T, scale.=T)
leap_cf_e_summary <- summary(leap_cf_e_pca)
leap_cf_e_rotated <- as.data.frame(leap_cf_e_pca$x)

	# Spanish
leap_cf_s_pca <- prcomp(leap_cf_s, center=T, scale.=T)
leap_cf_s_summary <- summary(leap_cf_s_pca)
leap_cf_s_rotated <- as.data.frame(leap_cf_s_pca$x)

# Proficiency
leap_prof_pca <- prcomp(leap_prof, center=T, scale.=T)
leap_prof_summary <- summary(leap_prof_pca)
leap_prof_rotated <- as.data.frame(leap_prof_pca$x)

	# English
leap_prof_e_pca <- prcomp(leap_prof_e, center=T, scale.=T)
leap_prof_e_summary <- summary(leap_prof_e_pca)
leap_prof_e_rotated <- as.data.frame(leap_prof_e_pca$x)

	# Spanish
leap_prof_s_pca <- prcomp(leap_prof_s, center=T, scale.=T)
leap_prof_s_summary <- summary(leap_prof_s_pca)
leap_prof_s_rotated <- as.data.frame(leap_prof_s_pca$x)


## RUN REGRESSIONS ####
# Variables collapsed by language
leap_small.glm = glm(leap$form_lg ~ leap_demo_pca$x[,1:1] + leap_ages_pca$x[,1:1] + leap_exp_pca$x[,1:1] + leap_cf_pca$x[,1:1] + leap_prof_pca$x[,1:1])

# Variables separated by language
leap.glm = glm(leap$form_lg ~ leap_demo_pca$x[,1:1] + leap_ages_e_pca$x[,1:1] + leap_ages_s_pca$x[,1:1] + leap_exp_e_pca$x[,1:1] + leap_exp_s_pca$x[,1:1] + leap_cf_e_pca$x[,1:1] + leap_cf_s_pca$x[,1:1] + leap_prof_e_pca$x[,1:1] + leap_prof_s_pca$x[,1:1])

# MANOVA collapsed by language
leap_demo.manova = manova(leap_demo_pca$x[,1:2] ~ leap$form_lg)
summary(leap_demo.manova)

leap_ages.manova = manova(leap_ages_pca$x[,1:16] ~ leap$form_lg)
summary(leap_ages.manova)
	leap_ages_e.manova = manova(leap_ages_e_pca$x[,1:8] ~ leap$form_lg)
	summary(leap_ages_e.manova)
	
	leap_ages_s.manova = manova(leap_ages_s_pca$x[,1:8] ~ leap$form_lg)
	summary(leap_ages_s.manova)

leap_exp.manova = manova(leap_exp_pca$x[,1:18] ~ leap$form_lg)
summary(leap_exp.manova)
	leap_exp_e.manova = manova(leap_exp_e_pca$x[,1:9] ~ leap$form_lg)
	summary(leap_exp_e.manova)
	
	leap_exp_s.manova = manova(leap_exp_s_pca$x[,1:9] ~ leap$form_lg)
	summary(leap_exp_s.manova)

leap_cf.manova = manova(leap_cf_pca$x[,1:12] ~ leap$form_lg)
summary(leap_cf.manova)
	leap_cf_e.manova = manova(leap_cf_e_pca$x[,1:6] ~ leap$form_lg)
	summary(leap_cf_e.manova)
	
	leap_cf_s.manova = manova(leap_cf_s_pca$x[,1:6] ~ leap$form_lg)
	summary(leap_cf_s.manova)

leap_prof.manova = manova(leap_prof_pca$x[,1:10] ~ leap$form_lg)
summary(leap_prof.manova)
	leap_prof_e.manova = manova(leap_prof_e_pca$x[,1:5] ~ leap$form_lg)
	summary(leap_prof_e.manova)
	
	leap_prof_s.manova = manova(leap_prof_s_pca$x[,1:5] ~ leap$form_lg)
	summary(leap_prof_s.manova)


















