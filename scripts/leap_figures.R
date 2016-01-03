## LOAD PACKAGES ####
library(ggplot2)


## READ IN DATA AND ORGANIZE ####
leap = read.table("data/leapq.txt", header=T, sep="\t")

leap$form_lg = factor(leap$form_lg, levels=c(0, 1), labels=c("English", "Spanish"))


## MAKE FIGURES ####
# Age acquire Spanish
s_age_plot <- ggplot(data = leap, aes(x=form_lg, y=s_acquire_age)) +
					geom_boxplot(aes(fill=form_lg)) +
					theme_bw() +
             		scale_fill_manual(values=c("white", "grey")) +
					xlab("Language of Questionnaire") +
					ylab("Age acquire Spanish in years") +
					#ggtitle("LEAP-Q") +
					guides(fill=FALSE) +
					theme(text=element_text(size=18), title=element_text(size=18), axis.line = element_line(colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_blank(), panel.background = element_blank()) +
					scale_y_continuous(limits=c(0, 3.5))
	pdf("../Figures/leap_s_age.pdf")
s_age_plot
	dev.off()
