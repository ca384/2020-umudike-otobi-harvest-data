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
```

```{r, adding new column for prop harvested to the complete data}
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
