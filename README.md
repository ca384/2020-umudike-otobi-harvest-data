# 2020-umudike-otobi-harvest-data
umu.oto<-read.csv("/Users/chinedoziamaefula/OneDrive - Cornell University/2020 Data/2020.umu.otobi.combine.harvestdata.csv")

umu.oto$rep_number<-as.factor(umu.oto$rep_number)
umu.oto$block_number <-as.factor(umu.oto$block_number )
umu.oto$location<-as.factor(umu.oto$location)
umu.oto$plot_number<-as.factor(umu.oto$plot_number)
umu.oto$accession_name<-as.factor(umu.oto$accession_name)
str(umu.oto)
umu.oto$root.number.ha <- (umu.oto$root.number.counting*10 / 8)
umu.oto$log.root.number.ha <-log(umu.oto$root.number.ha)

# adding new column for prop harvested to the complete data
umu.oto$prophav <- ((umu.oto$plant.stands.harvested/8)*100)
umu.oto$log.fry<-log(umu.oto$fry)
umu.oto$log.root.number.counting<-log(umu.oto$root.number.counting)

hist(umu.oto$root.number.counting)
hist(umu.oto$dmc.spg)
hist(umu.oto$DMc.oven)
hist(umu.oto$log.fry)
hist(umu.oto$starch)
hist(umu.oto$log.root.number.counting)


boxplot( dmc.spg~location, data=umu.oto,  ylab = "DMC (%)", main="Dry matter by specific gravity method")

boxplot( starch~location, data=umu.oto,  ylab = "starch (%)", main="starch by specific gravity method")
boxplot( DMc.oven~location, data=umu.oto, ylab = "DMC (%)", main="Dry matter by oven dry method")
boxplot( umu.oto$log.fry~location, data=umu.oto, ylab = "log fRY", main=" log of Yield (tonnes/ha")
boxplot( umu.oto$log.root.number.counting~location, data=umu.oto, ylab = "number of root", main="log of Yield (tonnes/ha")

#using lmer to fix the mixed model for dmc oven dry and by specific gravity method using studentized residual to remove the outliers

library(lme4)
fit.dmc.ov <- lmer(DMc.oven ~  (1 | accession_name)+location + rep_number:location  + (1 | block_number:location) +  (1|accession_name:location), data=umu.oto)
summary(fit.dmc.ov)
anova(fit.dmc.ov)

fit.dmc.ov_an <- lmer(DMc.oven ~ rep_number:location+ (1 | block_number), data=umu.oto)
anova(fit.dmc.ov, fit.dmc.ov_an, test="Chisq")

errorval=3.0
mod.refit.ov <- lmer(DMc.oven ~ (1 | accession_name) +location + (1|accession_name:location)+ rep_number:location +  (1 | block_number:location), data=umu.oto, subset=abs(resid(fit.dmc.ov)/sigma(fit.dmc.ov)) <= errorval)

summary(mod.refit.ov)
residual.studentized<-rstudent(mod.refit.ov)
hist(residual.studentized)

#for dmc spg
#The variance is almost similar to that with the outlier

# dmc specific gravity method
fit.dmc.spg <- lmer(dmc.spg~  (1 | accession_name)+location + rep_number:location  + (1 | block_number:location) +  (1|accession_name:location), data=umu.oto)
summary(fit.dmc.spg)
anova(fit.dmc.spg)

fit.dmc.spg_an <- lmer(dmc.spg ~ rep_number:location+ (1 | block_number), data=umu.oto)
anova(fit.dmc.spg, fit.dmc.spg_an, test="Chisq")

errorval=3.0
mod.refit.dmc.spg <- lmer(dmc.spg ~ (1 | accession_name) +location + (1|accession_name:location)+ rep_number:location +  (1 | block_number:location), data=umu.oto, subset=abs(resid(fit.dmc.spg)/sigma(fit.dmc.spg)) <= errorval)

summary(mod.refit.dmc.spg)
residual.studentized<-rstudent(mod.refit.dmc.spg)
hist(residual.studentized)

