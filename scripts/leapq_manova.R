## LOAD PACKAGES ####
library(pcaMethods)


## READ IN DATA ####
leap = read.table("data/leapq.txt", header=T, sep="\t")


## ORGANIZE DATA BY SUBTYPE ####
# Ages
leap_ages = data.matrix(subset(leap, select=c(english, e_acquire_age, e_fluent_age, e_beganread_age, e_fluentread_age, e_country_spoken_y_m, e_family_spoken_y_m, e_work_spoken_y_m, spanish, s_acquire_age, s_fluent_age, s_beganread_age, s_fluentread_age, s_country_spoken_y_m, s_family_spoken_y_m, s_work_spoken_y_m)))

	# English
leap_ages_e = data.matrix(subset(leap, select=c(english, e_acquire_age, e_fluent_age, e_beganread_age, e_fluentread_age, e_country_spoken_y_m, e_family_spoken_y_m, e_work_spoken_y_m)))

	# Spanish
leap_ages_s = data.matrix(subset(leap, select=c(spanish, s_acquire_age, s_fluent_age, s_beganread_age, s_fluentread_age, s_country_spoken_y_m, s_family_spoken_y_m, s_work_spoken_y_m)))

# Exposure
leap_exp = data.matrix(subset(leap, select=c(percent_exp_eng, percent_read_eng, percent_speak_eng, e_friends_exp, e_family_exp, e_reading_exp, e_instruction_exp, e_tv_exp, e_music_exp, percent_exp_sp, percent_read_sp, percent_speak_sp, s_friends_exp, s_family_exp, s_reading_exp, s_instruction_exp, s_tv_exp, s_music_exp)))

	# English
leap_exp_e = data.matrix(subset(leap, select=c(percent_exp_eng, percent_read_eng, percent_speak_eng, e_friends_exp, e_family_exp, e_reading_exp, e_instruction_exp, e_tv_exp, e_music_exp)))

	# Spanish
leap_exp_s = data.matrix(subset(leap, select=c(percent_exp_sp, percent_read_sp, percent_speak_sp, s_friends_exp, s_family_exp, s_reading_exp, s_instruction_exp, s_tv_exp, s_music_exp)))

# Contributing Factors
leap_cf = data.matrix(subset(leap, select=c(e_friends_con, e_family_con, e_reading_con, e_instruction_con, e_tv_con, e_music_con, s_friends_con, s_family_con, s_reading_con, s_instruction_con, s_tv_con, s_music_con)))

	# English
leap_cf_e = data.matrix(subset(leap, select=c(e_friends_con, e_family_con, e_reading_con, e_instruction_con, e_tv_con, e_music_con)))

	# Spanish
leap_cf_s = data.matrix(subset(leap, select=c(s_friends_con, s_family_con, s_reading_con, s_instruction_con, s_tv_con, s_music_con)))

# Proficiency
leap_prof = data.matrix(subset(leap, select=c(e_speaking_prof, e_understanding_prof, e_reading_prof, e_selfrate_accent, e_otherrate_accent, s_speaking_prof, s_understanding_prof, s_reading_prof, s_selfrate_accent, s_otherrate_accent)))

	# English
leap_prof_e = data.matrix(subset(leap, select=c(e_speaking_prof, e_understanding_prof, e_reading_prof, e_selfrate_accent, e_otherrate_accent)))

	# Spanish
leap_prof_s = data.matrix(subset(leap, select=c(s_speaking_prof, s_understanding_prof, s_reading_prof, s_selfrate_accent, s_otherrate_accent)))


## RUN MANOVAS ####
leap_ages.manova = manova(leap_ages ~ leap$form_lg)
summary(leap_ages.manova)
	leap_ages_e.manova = manova(leap_ages_e ~ leap$form_lg)
	summary(leap_ages_e.manova)
	
	leap_ages_s.manova = manova(leap_ages_s ~ leap$form_lg)
	summary(leap_ages_s.manova)

leap_exp.manova = manova(leap_exp ~ leap$form_lg)
summary(leap_exp.manova)
	leap_exp_e.manova = manova(leap_exp_e ~ leap$form_lg)
	summary(leap_exp_e.manova)
	
	leap_exp_s.manova = manova(leap_exp_s ~ leap$form_lg)
	summary(leap_exp_s.manova)

leap_cf.manova = manova(leap_cf ~ leap$form_lg)
summary(leap_cf.manova)
	leap_cf_e.manova = manova(leap_cf_e ~ leap$form_lg)
	summary(leap_cf_e.manova)
	
	leap_cf_s.manova = manova(leap_cf_s ~ leap$form_lg)
	summary(leap_cf_s.manova)

leap_prof.manova = manova(leap_prof ~ leap$form_lg)
summary(leap_prof.manova)
	leap_prof_e.manova = manova(leap_prof_e ~ leap$form_lg)
	summary(leap_prof_e.manova)
	
	leap_prof_s.manova = manova(leap_prof_s ~ leap$form_lg)
	summary(leap_prof_s.manova)
	

## FOLLOW UP REGRESSIONS
# Ages - Spanish
leap_ages_s_follow = lm(leap_ages_s ~ leap$form_lg)
summary(leap_ages_s_follow)
p.adjust(c(1, 0.0057, 0.172, 0.415, 0.239, 0.851369, 0.617, 0.263), method="BH")

# Current exposure - Spanish
leap_exp_s_follow = lm(leap_exp_s ~ leap$form_lg)
summary(leap_exp_s_follow)
p.adjust(c(0.0777, 0.246287, 0.347, 0.845, 0.761, 0.37, 0.29745, 0.592, 0.0654), method="BH")
leap_exp_s_follow$coefficients

# Contributing factors - English
leap_cf_e_follow = lm(leap_cf_e ~ leap$form_lg)
summary(leap_cf_e_follow)
p.adjust(c(0.163, 0.254, 0.124, 0.24875, 0.423, 0.478), method="BH")
leap_cf_e_follow$coefficients
















