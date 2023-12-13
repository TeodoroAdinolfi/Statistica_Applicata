## Progetto di Statistica Applicata - Analisi di regressione di un dataset

## Gruppo 07
## Adinolfi Teodoro
## Amato Emilio
## Bove Antonio
## Ferrara Grazia

# Imposto cartella di lavoro

setwd('G:/Drive condivisi/Progetto_statistica')

# Caricamento dataset

data = read.csv("Dataset_AH_gruppo7.csv", header = TRUE)
View(data)

prestazSWcalc = data$y_prestazSWcalc
CPU = data$x1_CPU
HD = data$x2_HD
proc = data$x3_proc
aging  = data$x4_aging
audio = data$x5_audio
RAM = data$x6_RAM

# Analisi preliminare dei dati, in forma grafica o tabellare, tRAMite gli strumenti di statistica descrittiva, 
# quali scatter plot, istogRAMmi, box-plot, ecc., e analisi di correlazione delle variabili presenti nel dataset, 
# utile alla definizione dei modelli di regressione.

########################################################## Freq. tables ####################################################### 
mycol <- rgb(25, 134, 220, max = 255, alpha = 150, names = "ourBlue")

library(gridExtra)
library(gridtext)
library(grid)
library(zoo)

tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:9], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="navyblue", fontface=3L, hjust=0, x=0)),
  padding=unit(c(6, 6), "mm"))

dev.off()
limitiCPU = seq(from=min(CPU), to=max(CPU), length.out=9) #ottengo le classi di modalit?
pop <-cut(CPU, breaks=limitiCPU, right=FALSE, include.lowest = TRUE)
title <- cbind(round(rollmean(limitiCPU,2),2),table(pop), table(pop)/length(pop), cumsum(table(pop)), cumsum(table(pop))/length(pop))
colnames(title) <- c("Centro di classe","Assolute", "Relative", "Cumulate", "Relative Cumulate")
title
table <- grid.table(title, theme=tt3)
table <- grid.text("Frequenze CPU", vjust = -15, hjust=-0.05)


dev.off()
limitiHD = seq(from=min(HD), to=max(HD), length.out=9) #ottengo le classi di modalit?
pop <-cut(HD, breaks=limitiHD, right=FALSE, include.lowest = TRUE)
title <- cbind(round(rollmean(limitiHD,2),2),table(pop), table(pop)/length(pop), cumsum(table(pop)), cumsum(table(pop))/length(pop))
colnames(title) <- c("Centro di classe","Assolute", "Relative", "Cumulate", "Relative Cumulate")
title
table <- grid.table(title, theme=tt3)
table <- grid.text("Frequenze Hard Disk", vjust = -15, hjust=-0.05)


dev.off()
limitiProc = seq(from=min(proc), to=max(proc), length.out=9)#ottengo le classi di modalit?
pop <-cut(proc, breaks=limitiProc, right=FALSE, include.lowest = TRUE)
title <- cbind(round(rollmean(limitiProc,2),2),table(pop), table(pop)/length(pop), cumsum(table(pop)), cumsum(table(pop))/length(pop))
colnames(title) <- c("Centro di classe","Assolute", "Relative", "Cumulate", "Relative Cumulate")
title
table <- grid.table(title, theme=tt3)
table <- grid.text("Frequenze Processi", vjust = -15, hjust=-0.05)


dev.off()
limitiAg = seq(from=min(aging), to=max(aging), length.out=9) #ottengo le classi di modalit?
pop <-cut(aging, breaks=limitiAg, right=FALSE, include.lowest = TRUE)
title <- cbind(round(rollmean(limitiAg,2),2),table(pop), table(pop)/length(pop), cumsum(table(pop)), cumsum(table(pop))/length(pop))
colnames(title) <- c("Centro di classe","Assolute", "Relative", "Cumulate", "Relative Cumulate")
title
table <- grid.table(title, theme=tt3)
table <- grid.text("Frequenze Aging", vjust = -15, hjust=-0.05)


dev.off()
limitiAu <- seq(from=min(audio), to=max(audio), length.out=9) #ottengo le classi di modalità
pop <-cut(audio, breaks=limitiAu, right=FALSE, include.lowest = TRUE) 
title <- cbind(round(rollmean(limitiAu,2),2), table(pop), table(pop)/length(pop), cumsum(table(pop)), cumsum(table(pop))/length(pop))
colnames(title) <- c("Centro di classe","Assolute", "Relative", "Cumulate", "Relative Cumulate")
title
table <- grid.table(title, theme=tt3)
table <- grid.text("Frequenze Audio", vjust = -15, hjust=-0.05)


dev.off()
limitiRAM <- seq(from=min(RAM), to=max(RAM), length.out=9) #ottengo le classi di modalit?
pop <-cut(RAM, breaks=limitiRAM, right=FALSE, include.lowest = TRUE)
title <- cbind(round(rollmean(limitiRAM,2),2), table(pop), table(pop)/length(pop), cumsum(table(pop)), cumsum(table(pop))/length(pop))
colnames(title) <- c("Centro di classe","Assolute", "Relative", "Cumulate", "Relative Cumulate")
title
table <- grid.table(title, theme=tt3)
table <- grid.text("Frequenze RAM", vjust = -15, hjust=-0.05)

######################################################### IstogRAMmi #########################################################

hist(CPU, freq = TRUE, breaks=limitiCPU, xlim=c(-2,2), ylab="Frequenze Assolute",main="Distribuzione di frequenza CPU",xlab="Velocità CPU",col=mycol)
axis(4, at = c(0,5,10,15), labels = c(0.00,0.05,0.10,0.15), line=0.05)
mtext("Frequenze Relative", side = 4, line=-1.5)
rug(CPU,lwd=1.0,side=1,col="dimgrey")

hist(HD, freq = TRUE,breaks=limitiHD, xlim=c(-2,2), ylab="Frequenze Assolute",main="Distribuzione di frequenza Hard Disk",xlab="Dimensioni Hard Disk",col=mycol)
axis(4, at = c(0,5,10,15), labels = c(0.00,0.05,0.10,0.15), line=0.05)
mtext("Frequenze Relative", side = 4, line=-1.5)
rug(HD,lwd=1.0,side=1,col="dimgrey")

hist(proc, freq = TRUE,breaks=limitiProc, xlim=c(-2,2), ylab = "Frequenze Assolute", main = "Distribuzione di frequenza Processi", xlab = "Numero di Processi Software",col=mycol)
axis(4, at = c(0,5,10,15), labels = c(0.00,0.05,0.10,0.15), line=0.05)
mtext("Frequenze Relative", side = 4, line=-1.5)
rug(proc,lwd=1.0,side=1,col="dimgrey")

hist(aging, freq = TRUE, breaks=limitiAg, xlim=c(-2,2), ylab = "Frequenze Assolute", main = "Distribuzione di frequenza Aging", xlab = "Aging Software",col=mycol)
axis(4, at = c(0,5,10,15), labels = c(0.00,0.05,0.10,0.15), line=0.05)
mtext("Frequenze Relative", side = 4, line=-1.5)
rug(aging,lwd=1.0,side=1,col="dimgrey")

hist(audio, freq = TRUE, breaks=limitiAu, xlim=c(-2,2),ylab = "Frequenze Assolute", main = "Distribuzione di frequenza Audio", xlab = "Prestazioni Scheda Audio",col=mycol)
axis(4, at = c(0,5,10,15), labels = c(0.00,0.05,0.10,0.15), line=0.05)
mtext("Frequenze Relative", side = 4, line=-1.5)
rug(audio,lwd=1.0,side=1,col="dimgrey")

hist(RAM, freq = TRUE, breaks=limitiRAM, xlim=c(-2,2), ylab = "Frequenze Assolute", main = "Distribuzione di frequenza RAM", xlab = "Prestazioni RAM",col=mycol)
axis(4, at = c(0,5,10,15), labels = c(0.00,0.05,0.10,0.15), line=0.05)
mtext("Frequenze Relative", side = 4, line=-1.5)
rug(RAM,lwd=1.0,side=1,col="dimgrey")

#####################################################Density Plot################################################

hist(CPU,freq=FALSE,breaks=limitiCPU, xlim=c(-2,2), ylab="Densità",main="Density Plot CPU",xlab="Velocità CPU",col="lightblue")
lines(density(CPU),lwd=2,col="darkblue")   
rug(CPU,lwd=1.0,side=1,col="dimgrey")
polygon(density(CPU), col = mycol)

hist(HD, freq = FALSE,breaks=limitiHD, xlim=c(-2,2), ylab="Densità",main="Density Plot Hard Disk",xlab="Dimensioni Hard Disk",col="lightblue")
lines(density(HD),lwd=2,col="darkblue")   
rug(HD,lwd=1.0,side=1,col="dimgrey")
polygon(density(HD), col = mycol)

hist(proc, freq = FALSE,breaks=limitiProc, xlim=c(-2,2), ylab = "Densità", main = "Density Plot Processi", xlab = "Numero di Processi Software",col="lightblue")
lines(density(proc),lwd=2,col="darkblue")   
rug(proc,lwd=1.0,side=1,col="dimgrey")
polygon(density(proc), col = mycol)

hist(aging, freq = FALSE, breaks=limitiAg, xlim=c(-2,2), ylab = "Densità", main = "Density Plot Aging", xlab = "Aging Software",col="lightblue")
lines(density(aging),lwd=2,col="darkblue")   
rug(aging,lwd=1.0,side=1,col="dimgrey")
polygon(density(aging), col = mycol)

hist(audio, freq = FALSE, breaks=limitiAu, xlim=c(-2,2), ylab = "Densità", main = "Density Plot Audio", xlab = "Prestazioni Scheda Audio",col="lightblue")
lines(density(audio),lwd=2,col="darkblue")   
rug(audio,lwd=1.0,side=1,col="dimgrey")
polygon(density(audio), col = mycol)

hist(RAM, freq = FALSE, breaks=limitiRAM, xlim=c(-2,2), ylab = "Densità", main = "Density Plot RAM", xlab = "Prestazioni RAM",col="lightblue")
lines(density(RAM),lwd=2,col="darkblue")   
rug(RAM,lwd=1.0,side=1,col="dimgrey")
polygon(density(RAM), col = mycol)

###################################################### Grafici a gradini ######################################################

plot(ecdf(CPU),ylab="Frequenze Relative Cumulate",xlab="Velocità CPU",main="Distribuzione cumulata di frequenza CPU",col="dodgerblue2")
lines(ecdf(CPU),lwd=5,col="dodgerblue2")
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c(0,20,40,60,80,100), line=0.01)
mtext("Frequenze Cumulate", side = 4, line=-1.5)

plot(ecdf(HD), ylab="Frequenze Relative Cumulate",xlab="Dimensione Hard Disk",main="Distribuzione cumulata di frequenza Hard Disk",col="dodgerblue2")
lines(ecdf(HD),lwd=5,col="dodgerblue2")
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c(0,20,40,60,80,100), line=0.01)
mtext("Frequenze Cumulate", side = 4, line=-1.5)

plot(ecdf(proc), ylab="Frequenze Relative Cumulate",xlab="Numero di Processi Software",main="Distribuzione cumulata di frequenza Processi",col="dodgerblue2")
lines(ecdf(proc),lwd=5,col="dodgerblue2")
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c(0,20,40,60,80,100), line=0.01)
mtext("Frequenze Cumulate", side = 4, line=-1.5)

plot(ecdf(aging), ylab="Frequenze Relative Cumulate",xlab="Aging Software", main="Distribuzione cumulata di frequenza Aging",col="dodgerblue2")
lines(ecdf(aging),lwd=5,col="dodgerblue2")
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c(0,20,40,60,80,100), line=0.01)
mtext("Frequenze Cumulate", side = 4, line=-1.5)

plot(ecdf(audio), ylab="Frequenze Relative Cumulate",xlab="Prestazioni Scheda Audio", main="Distribuzione cumulata di frequenza Audio",col="dodgerblue2")
lines(ecdf(audio),lwd=5,col="dodgerblue2")
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c(0,20,40,60,80,100), line=0.01)
mtext("Frequenze Cumulate", side = 4, line=-1.5)

plot(ecdf(RAM), ylab="Frequenze Relative Cumulate",xlab="Prestazioni RAM", main="Distribuzione cumulata di frequenza RAM",col="dodgerblue2")
lines(ecdf(RAM),lwd=5,col="dodgerblue2")
axis(4, at = c(0.0,0.2,0.4,0.6,0.8,1.0), labels = c(0,20,40,60,80,100), line=0.01)
mtext("Frequenze Cumulate", side = 4, line=-1.5)

################################################# Indici di tendenza centrale #################################################

dev.off()
d <- do.call(cbind, lapply(data, summary))
colnames(d) <- c("Prestazioni\nSW Calcolatore","Velocità\nCPU","Dimensioni\nHD","Numero\nprocessi SW","Aging SW","Prestazioni\nscheda audio","Prestazioni\nRAM")
grid.table(round(d,2),theme=tt3)

# Non esiste un comando per calcolare la moda, che per? si pu? ricavare da una tabella di frequenze o da un grafico delle 
# frequenze di una variabile (? il valore con la frequenza pi? elevata, assoluta o relativa)

freq = table(CPU)
maxCPU=which.max(freq)
print(maxCPU)

freq = table(HD)
maxHD=which.max(freq)
print(maxHD)

freq = table(proc)
maxProc=which.max(freq)
print(maxProc)

freq = table(aging)
maxAg=which.max(freq)
print(maxAg)

freq = table(audio)
maxAu=which.max(freq)
print(maxAu)

freq = table(RAM)
maxRAM=which.max(freq)
print(maxRAM)

freq = table(data$y_prestazSWcalc)
maxPre=which.max(freq)
print(maxPre)

########################################################### Box-Plot ##########################################################

# Complessivo
boxplot(CPU,HD,proc,aging,audio,RAM, names = c("Velocit? CPU","Dimensione HD", "Numero proc","Aging Software","Prestazioni Scheda Audio","Prestazioni RAM"), notch=TRUE, col = c("blue","red","green","yellow","pink","orange"))

# Singoli
boxplot(CPU, horizontal=F, notch=TRUE, main="Box Plot CPU", col="dodgerblue2", border = "darkblue", ylab="Velocit? CPU")
testo=c("minimo","cardine inferiore","1 ?? quartile",
        "mediana","3 ?? quartile","cardine superiore", "valori anomali","massimo")
ascissetesto=rep(1.275,length(testo))
ordinatetesto=c(min(CPU),min(CPU)+0.2,
                quantile(CPU,c(0.25,0.50,0.75),names=FALSE),
                max(CPU)-0.2,max(CPU)-10,max(CPU))
text(ascissetesto,ordinatetesto,testo,cex=0.5,font=4)

boxplot(HD, horizontal=F, notch=TRUE, main="Box Plot Hard Disk", col = "dodgerblue2", border = "darkblue", ylab="Dimensioni Hard Disk")
testo=c("minimo","cardine inferiore","1 ?? quartile",
        "mediana","3 ?? quartile","cardine superiore", "valori anomali","massimo")
ascissetesto=rep(1.275,length(testo))
ordinatetesto=c(min(HD),min(HD)+0.2,
                quantile(HD,c(0.25,0.50,0.75),names=FALSE),
                max(HD) - 0.2,max(HD)-10,max(HD))
text(ascissetesto,ordinatetesto,testo,cex=0.5,font=4)

boxplot(proc, horizontal=F, notch=TRUE, main="Box Plot proc", col = "dodgerblue2", border = "darkblue", ylab="Numero di proc SW")
testo=c("minimo","cardine inferiore","1 ?? quartile",
        "mediana","3 ?? quartile","cardine superiore", "valori anomali","massimo")
ascissetesto=rep(1.275,length(testo))
ordinatetesto=c(min(proc),min(proc)+0.2,
                quantile(proc,c(0.25,0.50,0.75),names=FALSE),
                max(proc)-0.2,max(proc)-10,max(proc))
text(ascissetesto,ordinatetesto,testo,cex=0.5,font=4)

boxplot(aging, horizontal=F, notch=TRUE, main="Box Plot Aging", border = "darkblue", col = "dodgerblue2", ylab="Aging SW")
testo=c("minimo","cardine inferiore","1 ?? quartile",
        "mediana","3 ?? quartile","cardine superiore", "valori anomali","massimo")
ascissetesto=rep(1.275,length(testo))
ordinatetesto=c(min(aging),min(aging)+0.2,
                quantile(aging,c(0.25,0.50,0.75),names=FALSE),
                max(aging)-0.2,max(aging)-10,max(aging))
text(ascissetesto,ordinatetesto,testo,cex=0.5,font=4)

boxplot(audio, horizontal=F, notch=TRUE, main="Box Plot Audio", col = "dodgerblue2", border = "darkblue", ylab="Prestazioni Scheda Audio")
testo=c("minimo","cardine inferiore","1 ?? quartile",
        "mediana","3 ?? quartile","cardine superiore", "valori anomali","massimo")
ascissetesto=rep(1.275,length(testo))
ordinatetesto=c(min(audio),min(audio)+0.2,
                quantile(audio,c(0.25,0.50,0.75),names=FALSE),
                max(audio)-0.2,max(audio)-10,max(audio))
text(ascissetesto,ordinatetesto,testo,cex=0.5,font=4)

boxplot(RAM, horizontal=F, notch=TRUE, main="Box Plot RAM", border = "darkblue", col = "dodgerblue2", ylab="Preastazioni RAM")
testo=c("minimo","cardine inferiore","1 ?? quartile",
        "mediana","3 ?? quartile","cardine superiore", "valori anomali","massimo")
ascissetesto=rep(1.275,length(testo))
ordinatetesto=c(min(RAM),min(RAM)+0.2,
                quantile(RAM,c(0.25,0.50,0.75),names=FALSE),
                max(RAM)-0.2,max(RAM)-10,max(RAM))
text(ascissetesto,ordinatetesto,testo,cex=0.5,font=4)

############################################# Indici di dispersione ###########################################################

# Escursione campionaria
Rcalc = max(data$y_prestazSWcalc) - min(data$y_prestazSWcalc)
RCPU = max(CPU) - min(CPU)
RHD = max(HD) - min(HD)
Rproc = max(proc) - min(proc)
Raging = max(aging) - min(aging)
Raudio = max(audio) - min(audio)
RRAM = max(RAM) - min(RAM)
range = c(Rcalc,RCPU,RHD,Rproc,Raging,Raudio,RRAM)

# Deviazione standard
sdcalc = sd(data$y_prestazSWcalc, na.rm = TRUE) # se TRUE i casi mancanti non vengono considerati
sdCPU = sd(CPU, na.rm = TRUE) 
sdHD = sd(HD, na.rm = TRUE)
sdproc = sd(proc, na.rm = TRUE)
sdaging = sd(aging, na.rm = TRUE)
sdaudio = sd(audio, na.rm = TRUE)
sdRAM = sd(RAM, na.rm = TRUE)
sd = c(sdcalc, sdCPU, sdHD, sdproc, sdaging, sdaudio, sdRAM)

# Funzione da utilizzare se la varianza non ? campionaria (con n al denominatore invece di n-1)
varpop<-function(variabile){
  var(variabile)*(1-1/length(variabile))
}

# Varianza
varcalc = var(data$y_prestazSWcalc, na.rm = TRUE) #se TRUE i casi mancanti non vengono considerati
varCPU = var(CPU, na.rm = TRUE) 
varHD = var(HD, na.rm = TRUE)
varproc = var(proc, na.rm = TRUE)
varaging = var(aging, na.rm = TRUE)
varaudio = var(audio, na.rm = TRUE)
varRAM = var(RAM, na.rm = TRUE)
var = c(varcalc, varCPU, varHD, varproc, varaging, varaudio, varRAM)

dev.off()
tab2 <- rbind(range, var, sd)
colnames(tab2) <- c("Prestazioni\nSW Calcolatore","Velocità\nCPU","Dimensioni\nHD","Numero\nprocessi SW","Aging SW","Prestazioni\nscheda audio","Prestazioni\nRAM")
rownames(tab2) <- c("Range","Varianza","Deviazione\nStandard")
grid.table(round(tab2,2), theme=tt3)

# Coefficiente di variazione
CVcalc = sdcalc/mean(data$y_prestazSWcalc)

########################################## Analisi di correlazione ############################################################

# Scatter Plot 

# Totale
pairs(cbind(prestazSWcalc,CPU,HD,proc, aging, audio, RAM),panel=panel.smooth, main = "Correlazione per variabili multivariate", col="dodgerblue2")

# Singoli
library(ggplot2)
ggplot(data = data,aes(y=prestazSWcalc,x=CPU))+geom_point(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="lm",colour = 'darkgoldenrod1') + labs(y="Prestazioni Calcolatore",x="CPU",title = "Correlazione tra la velocit? della CPU e le prestazioni SW del Calcolatore") + theme_minimal()
ggsave("CPUPrestaz.jpeg")
ggplot(data = data,aes(y=CPU,x=prestazSWcalc))+geom_point(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="lm",colour = 'darkgoldenrod1') + labs(y="CPU",x="Prestazioni Calcolatore",title = "Correlazione tra le prestazioni SW del Calcolatore e la CPU") + theme_minimal()
ggsave("prestazCPU.jpeg")

ggplot(data = data,aes(y=prestazSWcalc,x=aging))+geom_point(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="lm",colour = 'darkgoldenrod1') + labs(y="Prestazioni Calcolatore",x="aging",title = "Correlazione tra l'aging SW e le prestazioni SW del calcolatore") + theme_minimal()
ggsave("agingPrestaz.jpeg")
ggplot(data = data,aes(y=aging,x=prestazSWcalc))+geom_point(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="lm",colour = 'darkgoldenrod1') + labs(y="aging",x="Prestazioni Calcolatore",title = "Correlazione tra le prestazioni SW del Calcolatore e l'aging SW") + theme_minimal()
ggsave("prestazaAGING.jpeg")

ggplot(data = data,aes(y=prestazSWcalc,x=proc))+geom_point(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="lm",colour = 'darkgoldenrod1') + labs(y="Prestazioni Calcolatore",x="proc",title = "Correlazione tra il numero di proc e le prestazioni SW del calcolatore") + theme_minimal()
ggsave("procPrestaz.jpeg")
ggplot(data = data,aes(y=proc,x=prestazSWcalc))+geom_point(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="lm",colour = 'darkgoldenrod1') + labs(y="proc",x="Prestazioni Calcolatore",title = "Correlazione tra le prestazioni SW del Calcolatore e il numero di proc SW in esecuzione") + theme_minimal()
ggsave("prestazproc.jpeg")

ggplot(data = data,aes(y=prestazSWcalc,x=proc))+geom_jitter(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="gam",colour = 'darkgoldenrod1') + labs(y="Prestazioni Calcolatore",x="proc") + theme_minimal()
ggsave("procPrestazQuadro.jpeg")
ggplot(data = data,aes(y=proc,x=prestazSWcalc))+geom_jitter(shape = 21, colour = 'darkblue', fill = 'dodgerblue2', stroke = 1)+geom_smooth(method="gam",colour = 'darkgoldenrod1') + labs(y="proc",x="Prestazioni Calcolatore") + theme_minimal()
ggsave("prestazprocquadro.jpeg")

# Coefficienti di correlazione
dev.off()
corr = cor(data)
colnames(corr)=rownames(corr)=c("Prestazioni\nSW Calcolatore","Velocità\nCPU","Dimensioni\nHD","Numero\nprocessi SW","Aging SW","Prestazioni\nscheda audio","Prestazioni\nRAM")
grid.table(round(corr,2), theme=tt3)
print(corr)

# Correlation plot
library(corrplot)
corrplot(corr, method="ellipse",tl.srt=45)

######################################################### Interazione #########################################################

library("interactions")

# Nostra Interazione
int_a = lm(prestazSWcalc ~ CPU*RAM)
interact_plot(int_a, pred = CPU, modx = RAM, plot.points = TRUE, interval = TRUE, int.width = 0.95)

# altre
int_b = lm(prestazSWcalc ~ CPU*audio)
interact_plot(int_b, pred = CPU, modx = audio, plot.points = TRUE)

int_c = lm(prestazSWcalc ~ CPU*aging)
interact_plot(int_c, pred = CPU, modx = aging, plot.points = TRUE)

int_d = lm(prestazSWcalc ~ CPU*HD)
interact_plot(int_d, pred = CPU, modx = HD, plot.points = TRUE)

int_e = lm(prestazSWcalc ~ CPU*proc)
interact_plot(int_e, pred = CPU, modx = proc, plot.points = TRUE)

int_f = lm(prestazSWcalc ~ RAM*audio)
interact_plot(int_f, pred = RAM, modx = audio, plot.points = TRUE)

int_g = lm(prestazSWcalc ~ RAM*aging)
interact_plot(int_g, pred = RAM, modx = aging, plot.points = TRUE)

int_h = lm(prestazSWcalc ~ RAM*HD)
interact_plot(int_h, pred = RAM, modx = HD, plot.points = TRUE)

int_i = lm(prestazSWcalc ~ RAM*proc)
interact_plot(int_i, pred = RAM, modx = proc, plot.points = TRUE)

int_j = lm(prestazSWcalc ~ audio*aging)
interact_plot(int_j, pred = audio, modx = aging, plot.points = TRUE)

int_k = lm(prestazSWcalc ~ audio*HD)
interact_plot(int_k, pred = audio, modx = HD, plot.points = TRUE)

int_l = lm(prestazSWcalc ~ audio*proc)
interact_plot(int_l, pred = audio, modx = proc, plot.points = TRUE)

int_m = lm(prestazSWcalc ~ aging*HD)
interact_plot(int_m, pred = aging, modx = HD, plot.points = TRUE)

int_n = lm(prestazSWcalc ~ aging*proc)
interact_plot(int_n, pred = aging, modx = proc, plot.points = TRUE)

int_o = lm(prestazSWcalc ~ HD*proc)
interact_plot(int_o, pred = HD, modx = proc, plot.points = TRUE)

############################################### Regressione polinomiale multipla ############################################### 

fit1 = lm(prestazSWcalc ~ CPU)
summary(fit1)

fit2 = lm(prestazSWcalc ~ CPU + HD)
summary(fit2)

fit3 = lm(prestazSWcalc ~ CPU + HD + proc)
summary(fit3)

fit4 = lm(prestazSWcalc ~ CPU + HD + proc + aging)
summary(fit4)

fit5 = lm(prestazSWcalc ~ CPU + HD + proc + aging + audio)
summary(fit5)

fit6 = lm(prestazSWcalc ~ CPU + HD + proc + aging + audio + RAM)
summary(fit6)

fit7 = lm(prestazSWcalc ~ CPU + proc + aging + RAM)
summary(fit7)

fit7_bis = lm(prestazSWcalc ~ CPU + proc + I(proc^2) + aging + RAM)
summary(fit7_bis)

################################################## Possibile modello stimato ################################################## 
fit8 = lm(prestazSWcalc ~ CPU + RAM + CPU*RAM + aging + proc + I(proc^2))
summary(fit8)
plot(fit8)

RSS = anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit7_bis, fit8)$RSS
vett = c(summary(fit1)$r.squared, summary(fit2)$r.squared, summary(fit3)$r.squared, summary(fit4)$r.squared, summary(fit5)$r.squared, summary(fit6)$r.squared, summary(fit7)$r.squared, summary(fit7_bis)$r.squared, summary(fit8)$r.squared)
dfR_2 = data.frame(modelli=c(1:9), vett)
dfRSS = data.frame(modelli=c(1:9), RSS)
library(ggplot2)
ggplot(data=dfRSS, aes(x=modelli, y=RSS, group = 1)) + geom_line(color="dodgerblue", size=1.5) + geom_point(size=3) + scale_x_discrete("Modelli", limit = c("1","2", "3","4","5","6","7","7_bis","8")) + ggtitle("Evoluzione RSS") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
ggsave("evoluzione_RSS.png")
ggplot(data=dfR_2, aes(x=modelli, y=vett, group = 1)) + geom_line(color="dodgerblue", size=1.5) + geom_point(size=3) + scale_x_discrete("Modelli", limit = c("1","2", "3","4","5","6","7","7_bis","8")) + ggtitle("Evoluzione R^2") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + ylab("R^2")
ggsave("evoluzione_R^2.png")

################################################### Test di specificazione ####################################################

residui = residuals(fit8)

# Test t Student: Media e deviazione standard incognite --> uso della distribuzione t-student e calcolo dell'intervallo di 
# confidenza al 95% per il valore atteso mu
# Calcolo della deviazione standard S = sqrt(sum(x-sample_mean)^2)/sqrt(n-1) 

n_sample = length(residui)
sample_mean = mean(residui)
temp=0
for (i in 1:n_sample)
{
  temp = temp + (residui[i]-sample_mean)^2
}

sample_stdev = sqrt(temp/(n_sample - 1))

# Visita della tabella t-student con (1 - alpha) = 0.95 --> alpha = 0.05 e (numero campioni - 1) = 99 = DF 
# o alternativamente ...

alpha = 0.05
t_score = qt(p=alpha, df=(n_sample - 1), lower.tail = F)
print(t_score)

# Intervallo di confidenza

lower_conf_int=sample_mean-t_score*sample_stdev/sqrt(n_sample)
upper_conf_int=sample_mean+t_score*sample_stdev/sqrt(n_sample)
print(lower_conf_int)
print(upper_conf_int)
plot(sample_mean, ylim = c(min(residui),max(residui)))
abline(h=lower_conf_int, col='blue')
abline(h=upper_conf_int, col='blue')

# Test Shapiro

shapiro = shapiro.test(residui)

# qq-plot
qqnorm(scale(residui),col="dodgerblue2")
abline(0,1, col="darkblue")

##################################################### Grafici diagnostici #####################################################

# Analisi grafica dei residui (fatta sul modello (probabilmente) scelto)

# Diagnostica sull'omoschedasticit? --> verificata con una dispersione verticale costante

plot(fit8, which=3,col=mycol)

# Verifica della normalit? (la distribuzione dei residui deve essere di tipo gaussiano)

qqnorm(scale(residui),col="dodgerblue2")
abline(0,1, col="darkblue")

# Linearit?: occorre tracciaare il grafico dei residui (ordinata) verso i valori previsti (ascissa).
# I punti dovrebbero essere distribuiti in modo simmetrico intorno ad una linea orizzontale con intercetta uguale a zero. 
# Andamenti di tipo diverso indicano la presenza di non linearit?.

plot(fit8, which=1,col=mycol)
res=fit8$residuals

# Indipendenza: si pu? tracciare il grafico dei residui (ordinata) verso i residui precedenti? (ascissa) che non dovrebbe 
# rivelare alcun pattern evidente 

n<-length(residui)
plot(residui[-n], residui[-1], col="darkblue")

################################################ Stima ai minimi quadrati paRAMetri ############################################

vettore_unitario = rep(1, 100)
CPU_RAM = CPU*RAM
X = cbind(vettore_unitario, CPU, RAM, CPU_RAM, aging, proc, I(proc^2))
b = solve((t(X)%*%X))%*%t(X)%*%prestazSWcalc
rownames(b) = c("intercetta", "CPU", "RAM", "CPU_RAM", "aging", "proc", "proc_2")

#################################### Stima della varianza della variabile aleatoria epsilon ####################################
SQE = sum((prestazSWcalc - fit8$fitted.values)^2)
var_espilon = SQE/93

########################################### Intervalli di confidenza paRAMetri v.1 ############################################

alpha = 0.05
gdl = length(prestazSWcalc) - 7
proc_2 = proc^2
CPU_RAM = CPU*RAM
n_sample = length(prestazSWcalc)
sample_mean = mean(CPU)
temp=0
for (i in 1:n_sample)
{
  temp = temp + (prestazSWcalc[i]-(43.6273+5.1754*CPU[i]+2.4386*RAM[i]-4.2156*aging[i]+1.8369*proc[i]-5.0523*proc_2[i]-3.5206*CPU_RAM[i]))^2
}
S=sqrt(temp/gdl)

temp=0
for(i in 1:n_sample)
{
  temp = temp + (CPU[i]^2)
}
temp = temp - n_sample*(sample_mean)^2

L = 5.1754 - (1.985802*S*sqrt(1/(temp)))
U = 5.1754 + (1.985802*S*sqrt(1/(temp)))

############################################## Intervalli di confidenza paRAMetri v.2 ######################################### 

library(gridExtra)
library(gridtext)
library(grid)
library(zoo)


tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:9], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="navyblue", fontface=3L, hjust=0, x=0)),
  padding=unit(c(12, 6), "mm"))

conf_int_parametri = function(x, estimate, flag){
  
  # Confidence intervals
  
  y.fitted=fit8$fitted.values
  y=prestazSWcalc
  
  # Find SQE(SSE) and MSQE(MSE)
  n=length(y)
  sqe=sum((y - y.fitted)^2)
  msqe=sqe/(n-7)
  S=sqrt(msqe)
  
  # t-value at 95% 
  t.val = qt(0.975, n - 7) # Critical value of t
  
  if(flag){
    # Conf int b0
    conf_int_b=t.val*S*sqrt(1/n+((mean(x))^2/(sum(x^2)-n*(mean(x))^2)))
    low_confint_b=estimate-conf_int_b
    up_confint_b=estimate+conf_int_b
  }else{
    # Conf int expected value
    conf_int_b=t.val*S*sqrt((1/(sum(x^2)-n*(mean(x))^2)))
    low_confint_b=estimate-conf_int_b
    up_confint_b=estimate+conf_int_b
  }
  confV <- list(low_confint_b,up_confint_b)
  return (confV)
}

c1 <- conf_int_parametri(CPU, fit8$coefficients[1],TRUE)
c2 <- conf_int_parametri(CPU, fit8$coefficients[2],FALSE)
c3 <- conf_int_parametri(RAM, fit8$coefficients[3],FALSE)
c4 <- conf_int_parametri(CPU*RAM, fit8$coefficients[7],FALSE)
c5 <- conf_int_parametri(aging, fit8$coefficients[4],FALSE)
c6 <- conf_int_parametri(proc, fit8$coefficients[5],FALSE)
c7 <- conf_int_parametri(proc^2, fit8$coefficients[6],FALSE)

conf <- rbind (c1,c2,c3,c5,c6,c7,c4)
m <- matrix(unlist(conf), ncol = 2, nrow = 7 )
colnames(m) <- c("2.5%","97.5%")
rownames(m) <- c("Intercept","CPU","RAM","Aging","Processi","I(Processi^2)","CPU:RAM")
dev.off()
grid.table(round(m,2),theme=tt3)

dev.off()
conf2 <- round(confint(lm(fit8)),2)
colnames(conf2) <- c("2.5%","97.5%")
rownames(conf2) <- c("Intercept","CPU","RAM","Aging","Processi","I(Processi^2)","CPU:RAM")
grid.table(conf2,theme=tt3)


######################################### Calcolo del coefficiente di determinazione ##########################################

SQR = sum((fit8$fitted.values - mean(prestazSWcalc))^2)
SQE = sum((prestazSWcalc - fit8$fitted.values)^2)

SQTOT = SQR + SQE
R_2 = SQR/SQTOT

###################################################### Test Durbin Watson #####################################################

install.packages('lmtest')
library(lmtest)
modello = formula(fit8) # Memorizziamo la formula del modello in un oggetto
dw = dwtest(modello,data=data) # Il valore della statistica di Durbin-Watson ? sempre compreso tra 0 e 4.

# Un valore di 2 indica che non appare presente alcuna autocorrelazione. Valori piccoli di d indicano che i residui successivi sono, in media, vicini in valore l'uno all'altro, o correlati positivamente. 
# Valori grandi di d indicano che i residui successivi sono, in media, molto differenti in valore l'uno dall'altro, o correlati negativamente


##################################################### Stepwise Regression #####################################################

# Il comando step() consente di effettuare una stepwise regression basata sul criterio AIC, nell'argomento
# direction si pu? indicare se una procedura backward (direction ="backward"), forward 
# direction = "forward"), oppure entRAMbe (direction = "both").

# Procedura Backward

f0 = lm(prestazSWcalc ~ CPU + HD + proc + aging + audio + RAM)
backsel = step(f0, direction = "backward")

# Procedura Forward

f1 = lm(prestazSWcalc ~ 1, data=data)
forwsel = step(f1, direction='forward', scope=formula(f0))

# Stepwise regression

f2 = lm(prestazSWcalc ~ 1, data=data)
forwsel = step(f2, direction='both', scope=formula(f0))

fit9 = lm(prestazSWcalc ~ aging + CPU + proc + RAM + audio)
summary(fit9)

fit10 = lm(prestazSWcalc ~ CPU*RAM + aging + proc + I(proc^2) + audio  + CPU + RAM)
summary(fit10)

# Stepwise regression on the most fitted model

f3 = step(fit8)

#################################################### Confronto tra modelli #################################################### 

# Confronto col fit completo dei regressori consigliati dall'analisi, rispetto al modello  trovato da noi
anova (fit9, fit8)

# Confronto con/senza audio
anova (fit10, fit8)