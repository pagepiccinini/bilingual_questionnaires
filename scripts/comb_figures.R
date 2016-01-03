## LOAD PACKAGES ####
library(ggplot2)


## READ IN DATA AND ORGANIZE ####
leap = read.table("data/leapq.txt", header=T, sep="\t")
bds = read.table("data/bds.txt", header=T, sep="\t")

leap_age_s_acquire = subset(leap, select=c(form_lg, s_acquire_age))
	leap_age_s_acquire$form = "LEAP-Q"
bds_age_s_acquire = subset(bds, select=c(form_lg, age_spanish))
	names(bds_age_s_acquire)[names(bds_age_s_acquire)=="age_spanish"] <- "s_acquire_age"
	bds_age_s_acquire$form = "BDS"
	
age_s_acquire = rbind(leap_age_s_acquire, bds_age_s_acquire)

age_s_acquire$form_lg = factor(age_s_acquire$form_lg, levels=c(0, 1), labels=c("English", "Spanish"))

age_s_acquire$form = factor(age_s_acquire$form, levels=c("LEAP-Q", "BDS"))


## MAKE FIGURES ####
# Age acquire Spanish
comb_s_age_plot <- ggplot(data = age_s_acquire, aes(x=form, y=s_acquire_age)) +
					geom_boxplot(aes(fill=form_lg)) +
					theme_bw() +
             		scale_fill_manual(values=c("white", "grey")) +
					xlab("Questionnaire") +
					ylab("Age acquire Spanish in years") +
					#ggtitle("LEAP-Q and BDS") +
					guides(fill=guide_legend(title="Language of\nQuestionnaire")) +
					theme(text=element_text(size=18), title=element_text(size=18), axis.line = element_line(colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_blank(), panel.background = element_blank(), legend.position=c(0.2,0.8), legend.key=element_blank()) +
					scale_y_continuous(limits=c(0, 7))
	pdf("../Figures/s_age.pdf")
comb_s_age_plot
	dev.off()
