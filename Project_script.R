ehr = read.csv('Project/Avaandmed_ehitised.csv',header = TRUE, check.names=FALSE, sep=";", dec = ",")

labels(ehr)
head(ehr)
head(ehr$Date.Created)
tail(ehr$Date.Created)

nrow(subset(ehr, ehr$"Omandi liigi nimii" == "kinnisasi"))

kinnis = subset(ehr, ehr$"Omandi liigi nimii" == "kinnisasi")


levels(kinnis$"Seisundi nimi")
nrow(subset(kinnis, kinnis$"Seisundi nimi" == "ehitamisel"))

plot(kinnis$"Seisundi nimi")

kasutusel = subset(kinnis, kinnis$"Seisundi nimi" == "kasutusel")

levels(kasutusel$"Peamine KAOS")
plot(kasutusel$"Peamine KAOS")

sort(table(kasutusel$"Peamine KAOS"),decreasing=TRUE)[1:6]

library(ggplot2)
##install.packages("ggplot2")

##unique(kasutusel$Aadress.tekstina.kehtiv)

##nrow(subset(ehr, ehr$"Peamine KAOS" == "Tuumaelektrijaama rajatis"))

## Filter housing
elamu = subset(kasutusel, grepl("(E|e)lamu", kasutusel$"Peamine KAOS"), drop = TRUE)

## Filter irrelevant assisting buildings like sheds and wells
unique(elamu$"Peamine KAOS")
elamu = subset(elamu, !grepl("bihoone", elamu$"Peamine KAOS"), drop = TRUE)


## Filter to only after year 2000
nrow(subset(elamu, complete.cases(elamu$"Esmane Kasutus")))

nrow(subset(elamu, elamu$"Esmane Kasutus" >= 2000 | elamu$"Eh Alust Kp" >= 2000))
plot(factor(subset(elamu, elamu$"Esmane Kasutus" >= 2000)$"Esmane Kasutus"))


sum(is.na(ehr$"Esmane Kasutus"))

nrow(subset(elamu, Eh.Alust.Kp != "" | elamu$"Esmane Kasutus" >= 2000))

library(data.table)
year(as.Date(elamu$Eh.Alust.Kp, "%Y/%m/%d"))
elamu$"Eh Alust Kp" = year(as.Date(elamu$"Eh Alust Kp", "%Y/%m/%d"))

nrow(subset(elamu, Eh.Alust.Kp != "" | elamu$"Esmane Kasutus" >= 2000))

nrow(subset(elamu, complete.cases(elamu$"Eh Alust Kp")))

## Store filtered data
uued = subset(elamu, Esmane.Kasutus >= 2000 | Eh.Alust.Kp >= 2000)

#write.csv(uued, file = 'Project/Elamud_post2000.csv', sep = ',')



#uued = read.csv('Project/Elamud_post2000.csv',header = TRUE, check.names=FALSE, sep=",")

length(unique(ehr$Omavalitsuse.kood))
ggplot(uued) +
  barplot(uued$Peamine.KAOS)


nrow(subset(uued, grepl("(T|t)allinn", uued$Aadress.tekstina.kehtiv), drop = TRUE))
nrow(subset(uued, Omavalitsuse.kood == 784))

plot(factor(uued$Esmane.Kasutus))
plot(factor(uued$Eh.Alust.Kp))

uued = subset(elamu, elamu$"Esmane Kasutus" >= 2000 | elamu$"Eh Alust Kp" >= 2000)

uued$"Esmane Kasutus"[is.na(uued$"Esmane Kasutus")] = uued$"Eh Alust Kp"[is.na(uued$"Esmane Kasutus")]

nrow(uued)
uued = subset(uued, uued$"Esmane Kasutus" >= 2000)

plot(factor(uued$"Esmane Kasutus"))
plot(factor(uued$"Eh Alust Kp"))



## Done filtering by year
write.csv(uued, file = 'Project/Elamud_post2000.csv', na = "", row.names = FALSE)










## New checkpoint
uued = read.csv('Project/shared/Algandmed/Ehitised/Elamud_post2000.csv',header = TRUE, check.names=FALSE, sep=",", dec = ",")


## Jaotus omavalitsuste järgi
plot(factor(uued$"Omavalitsuse kood"))
## Suurimad x omavalitsust koos arvuga
sort(table(uued$"Omavalitsuse kood"),decreasing=TRUE)[1:10]


## Format to numbers
##uued$`Ehitisalune Pind` = as.numeric(uued$"Ehitisalune Pind")
uued$`Aadress tekstina kehtiv`[which.max(uued$`Ehitisalune Pind`)]
uued$`Ehitisalune Pind`[which.max(uued$`Ehitisalune Pind`)]

## OUTLIERS

outliers2 = function(data, sdCount)
{
  sd(data)
  return(c(which( data > mean(data, na.rm = TRUE) + sd(data, na.rm = TRUE) * sdCount)))
}

uued$`Korgus`[outliers2(uued$`Korgus`,3)]
uued = uued[-outliers2(uued$`Korgus`,3)[c(7, 15, 16, 27)],]


uued[outliers2(uued$`Ehitisalune Pind`, 5)[74],]
uued$`Ehitisalune Pind`[outliers2(uued$`Ehitisalune Pind`, 5)]
## Too many actual large buildings to remove any outliers, just cutting everything above 2000
uued = uued[-which(uued$`Ehitisalune Pind` > 2000),]


## ggplot(uued$`Ehitisalune Pind`) + geom_histogram()

plot(table(uued$`Esmane Kasutus`))

## one year data. TODO loop it?
e2001 = subset(uued, uued$`Esmane Kasutus` == 2001)
hist(e2001$`Ehitisalune Pind`)
ggplot(e2001) + geom_histogram(aes(x = e2001$`Ehitisalune Pind`), binwidth = 40)


## Histogram for building areas
ggplot(uued, aes(x = uued$`Ehitisalune Pind`)) + 
  geom_histogram(binwidth = 40, fill = "#AA2222") +
  labs(x="Area covered by building",y="") + 
  theme_minimal()
  

ggplot(uued, aes(x = uued$`Koetav Pind`)) + 
  geom_histogram(binwidth = 40, fill = "#AA2222") +
  labs(x="Area covered by the building",y="") + 
  theme_minimal()


ggplot(uued) + geom_histogram(aes(x = uued$Korgus), binwidth = 1)

## Without Tallinn
#ggplot(subset(uued, uued$`Omavalitsuse kood` != 784)) + 
#  geom_histogram(aes(x = subset(uued, uued$`Omavalitsuse kood` != 784)$`Ehitisalune Pind`), binwidth = 40)


## Boxplots
ggplot(uued) + geom_boxplot(aes(y = uued$`Ehitisalune Pind`, x = uued$`Esmane Kasutus`, group = uued$`Esmane Kasutus`) )
+ facet_wrap(~uued$`Esmane Kasutus`)

##
ggplot(uued) + geom_boxplot(aes(y = uued$Korgus, x = uued$`Esmane Kasutus`, group = uued$`Esmane Kasutus`) )
+ facet_wrap(~uued$`Esmane Kasutus`)


uued = subset(uued, uued$`Esmane Kasutus` < 2016)

## Total construction amounts various graphs
ggplot(uued) + 
  geom_bar(aes(x = uued$`Esmane Kasutus`), fill="#AA2222") + 
  labs(x="Year of first usage",y="Amount constructed") + 
  theme_minimal() +
  theme(text = element_text(size=20)) 

ggplot(uued, aes(x = uued$`Esmane Kasutus`)) + 
  geom_bar(aes(fill=..count..)) + 
  labs(x="",y="Amount constructed") + 
  theme_minimal() +
  theme(text = element_text(size=20)) +
  theme(legend.position="none")

ggplot(uued, aes(x = uued$`Esmane Kasutus`)) + 
  geom_line(stat="bin", binwidth = 1) + 
  labs(x="Year of first usage",y="Amount constructed") + 
  theme_minimal() +
  theme(text = element_text(size=20)) +
  ylim(0, 1600)
  
as.numeric(format(as.Date(uued$`Date Created`[1]), "%Y"))

ggplot(uued, aes(x = as.numeric(format(as.Date(uued$`Date Created`), "%Y")))) + 
  geom_bar(aes(fill=..count..)) + 
  labs(x="Year of first usage",y="Amount constructed") + 
  theme_minimal() +
  theme(text = element_text(size=20)) +
  theme(legend.position="none")

ggplot(uued) + geom_bar(aes(x = uued$`Esmane Kasutus`, y = uued$`Ehitisalune Pind`), stat = "identity")
## Just checking if the amount matched
sum(e2001$"Ehitisalune Pind")


## Kõrgus
ggplot(uued) + geom_histogram(aes(x = as.numeric(uued$Korgus)), binwidth = 1)
ggplot(uued) + geom_point(aes(x = uued$Korgus, y = uued$`Ehitisalune Pind`, color = uued$`Esmane Kasutus`))

ggplot(uued, aes(x = uued$Korgus, y = uued$`Esmane Kasutus`)) +
  geom_bar()



plot(table((uued$Korgus)))

## Kõrgus ja pind
ggplot(uued) + geom_point(aes(x = uued$`Ehitisalune Pind`, y = uued$Korgus))

## 
ggplot(uued) + geom_point(aes(x = uued$`Ehitisalune Pind`, y = uued$`Koetav Pind`))

ggplot(uued) + geom_point(aes(x = uued$`Ehitisalune Pind`, y = uued$`Koetav Pind`))


## Factoriks suuremad maakonnad ja ülejäänu, siis saaks asukoha järgi samasuguseid
NROW(unique((uued$"Omavalitsuse kood")))
sort(table(uued$"Omavalitsuse kood"),decreasing=TRUE)[1:30]

ggplot(uued) + geom_bar(aes(x = uued$`Omavalitsuse kood`))

barplot(sort(table(uued$"Omavalitsuse kood"), decreasing = TRUE)[1:30])
## Still in progress

summarySE(data = uued, measurevar="Ehitisalune Pind", groupvars=c("Esmane Kasutus"), na.rm = TRUE)





## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_%28ggplot2%29/
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}





## Composite size by multiplying the area and height, doesn't seem to add anything visually :(
uued$comp_size = uued$`Ehitisalune Pind` * uued$Korgus

ggplot(subset(uued, uued$comp_size < 10000), aes(x = subset(uued, uued$comp_size < 10000)$comp_size)) + 
  geom_histogram(binwidth = 40, fill = "#AA2222") +
  labs(x="Build volume",y="") + 
  theme_minimal()



uued$first_use_date = uued$`Esmane Kasutus`



mean_area_by_year = summarySE(data = uued, measurevar="Ehitisalune Pind", groupvars=c("first_use_date"), na.rm = TRUE)

pd <- position_dodge(0.1)

ggplot(mean_area_by_year, aes(x=first_use_date, y=mean_area_by_year$`Ehitisalune Pind`)) + 
  geom_errorbar(aes(ymin=mean_area_by_year$`Ehitisalune Pind`-se, ymax=mean_area_by_year$`Ehitisalune Pind`+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("") +
  ylab("Average construction area") +
#  scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
#                   breaks=c("OJ", "VC"),
#                   labels=c("Orange juice", "Ascorbic acid"),
#                   l=40) +                    # Use darker colors, lightness=40
#  ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
#  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=seq(160,250,10)) +         # Set tick every 4
  scale_x_continuous(breaks=seq(2000,2015,2)) + 
  theme_bw() +
  theme(text = element_text(size=20))

#  theme(legend.justification=c(1,0),
#        legend.position=c(1,0))               # Position legend in bottom right
  
  




uued$om_kood = uued$`Omavalitsuse kood`
temp$om_kood = as.factor(temp$om_kood)
NROW(subset(uued$om_kood, complete.cases(uued$om_kood)))
NROW(subset(uued$`Ehitisalune Pind`, !complete.cases(uued$`Ehitisalune Pind`)))
temp = subset(uued, complete.cases(uued$`Ehitisalune Pind`))
NROW(subset(temp$`Ehitisalune Pind`, !complete.cases(temp$`Ehitisalune Pind`)))
mean_area_by_year_and_ov = summarySE(data = uued, measurevar="Ehitisalune Pind", groupvars=c("first_use_date", "om_kood"), na.rm = TRUE)






## Rows after filtering year
ehr = subset(ehr, ehr$"Esmane Kasutus" >= 2000 | ehr$"Eh Alust Kp" >= 2000)

ehr$"Esmane Kasutus"[is.na(ehr$"Esmane Kasutus")] = ehr$"Eh Alust Kp"[is.na(ehr$"Esmane Kasutus")]

nrow(ehr)
temp = subset(ehr, ehr$"Esmane Kasutus" >= 2000)

nrow(ehr)
