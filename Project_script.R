ehr = read.csv('Project/Avaandmed_ehitised.csv',header = TRUE, sep=";")

labels(ehr)
head(ehr)
head(ehr$Date.Created)
tail(ehr$Date.Created)

nrow(subset(ehr, Omandi.liigi.nimii == "kinnisasi"))

kinnis = subset(ehr, Omandi.liigi.nimii == "kinnisasi")


levels(kinnis$Seisundi.nimi)
nrow(subset(kinnis, Seisundi.nimi == "ehitamisel"))

plot(kinnis$Seisundi.nimi)

kasutusel = subset(kinnis, Seisundi.nimi == "kasutusel")

levels(kasutusel$Peamine.KAOS)
plot(kasutusel$Peamine.KAOS)
library(ggplot2)
install.packages("ggplot2")
??ggplot

unique(kasutusel$Aadress.tekstina.kehtiv)

nrow(subset(ehr, Peamine.KAOS == "Tuumaelektrijaama rajatis"))

elamu = subset(kasutusel, grepl("(E|e)lamu", kasutusel$Peamine.KAOS), drop = TRUE)

nrow(subset(elamu, complete.cases(elamu$Esmane.Kasutus)))

nrow(subset(elamu, Esmane.Kasutus >= 2004))

sum(is.na(ehr$Esmane.Kasutus))

plot(elamu$Esmane.Kasutus)


nrow(subset(elamu, Eh.Alust.Kp != "" | Esmane.Kasutus >= 2004))
