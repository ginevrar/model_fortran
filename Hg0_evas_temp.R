setwd('C:/Users/gi/Dropbox/volat')

vis<-read.table("fort.97", header=F)
names(vis)<-c('vis','rho','D','temp')
out<-read.table("fort.77", header=F); 
names(out)<-c('wind','hg0w','hg0atm','flux1','flux2')
str(out)

setwd('C:/Users/gi/Dropbox/')
flussi_ven<-read.table('flussi_vento.txt', header=T)
str(flussi_ven)
names(flussi_ven)<-c('Hg0', 'Wind', 'Flusso',
                     'TGM','basin', 'Temp')

png('Hg0_evasion_ngm2h_temp.png', height = 15, width = 21, res=300,
    units = 'cm')
par(mfrow=c(1,2), mar=c(2.5,2.5,2,1), oma=c(2,2,2,0.4))
plot(vis$temp, out$flux1, xlim=c(0,40),ylim=c(0,40),
     main='Wannikhof method',col='#69696988', 
     ylab='', cex.lab=1.3)
mtext(expression(paste('Hg'^0*'evasion (ng m'^-2*' h'^-1*')')),
      side=2,line=2.5, cex=1.2)
mtext(expression(paste('Temperature (°C)')),
      side=1,line=2.6, cex=1.2)
par(new=T)
plot(flussi_ven$Temp,flussi_ven$Flusso,
     xlim=c(0,40),pch='', ylim=c(0,40),
      ylab='', xlab='')
with(subset(flussi_ven, basin=="Tirreno"), 
     points(Temp, Flusso, col="#33a02c",
            bg="#33a02c55", cex=1.7,lwd=1.4,type="p",pch=21))
with(subset(flussi_ven, basin=="Wmed"), 
     points(Temp, Flusso, col="#b2df8a",bg="#b2df8a55", type="p",lwd=1.4, cex=1.4,pch=22))
with(subset(flussi_ven, basin=="Ionio"), 
     points(Temp, Flusso, col="#a6cee3",bg="#a6cee355", type="p",lwd=1.4, cex=1.4,pch=23))
with(subset(flussi_ven, basin=="Sicily"), 
     points(Temp, Flusso, col="#1f78b4", bg="#1f78b455",cex=1.8,lwd=2,
            type="p",pch=3))
with(subset(flussi_ven, basin=="Emed"), 
     points(Temp, Flusso, col="#e31a1c",bg="#e31a1c55", lwd=1.4,cex=1.4,type="p",pch=25))
with(subset(flussi_ven, basin=="Ad"), cex=1.4,
     points(Temp, Flusso, col="#ff7f00",bg="#ff7f0055", lwd=1.4,
            type="p",pch=24))
with(subset(flussi_ven, basin=="NAd"), 
     points(Temp, Flusso, col="#d86b00", cex=1.8,lwd=2,
            type="p",pch=4))
with(subset(flussi_ven, basin=="Otr"), 
     points(Temp, Flusso, col="#6a3d9a", bg="#6a3d9a55", 
            type="p", lwd=2,cex=1.8,pch=8))
legend(0.01,41,bty='n',cex=.9,
       col=c("#33a02c", "#b2df8a","#a6cee3","#1f78b4",'#6a3d9a',"#e31a1c",
             "#ff7f00","#d86b00"),lwd=1.5,
       pt.bg=c("#33a02c55", "#b2df8a55","#a6cee355","#1f78b455",'#6a3d9a',"#e31a1c55",
               "#ff7f0055","#fb9a99"), pch=c(21,22,23,3,8,25,24,4),
       legend=c('Tirrenian','West - Med','Ionian',
                'Sicily','Otranto','Est - Med',
                'Adriatic', 'Northern Adriatic'))

plot(vis$temp, out$flux2,  main='Borges method',
     col='#a1a1a188',ylab='', ylim=c(0,40), xlim=c(0,40),cex.lab=1.3)
mtext(expression(paste('Temperature (°C)')),
      side=1,line=2.5, cex=1.3)
par(new=T)
plot(flussi_ven$Temp,flussi_ven$Flusso, xlim=c(0,40),pch='', 
     col='white', ylim=c(0,40), ylab='', xlab='')
with(subset(flussi_ven, basin=="Tirreno"), 
     points(Temp, Flusso, col="#33a02c", bg="#33a02c55", cex=1.7,lwd=1.4,
            type="p",pch=21))
with(subset(flussi_ven, basin=="Wmed"), 
     points(Temp, Flusso, col="#b2df8a",bg="#b2df8a55", type="p",lwd=1.4, cex=1.4,pch=22))
with(subset(flussi_ven, basin=="Ionio"), 
     points(Temp, Flusso, col="#a6cee3",bg="#a6cee355", type="p",lwd=1.4, cex=1.4,pch=23))
with(subset(flussi_ven, basin=="Sicily"), 
     points(Temp, Flusso, col="#1f78b4", bg="#1f78b455",cex=1.8,lwd=2,
            type="p",pch=3))
with(subset(flussi_ven, basin=="Emed"), 
     points(Temp, Flusso, col="#e31a1c",bg="#e31a1c55", lwd=1.4,cex=1.4,type="p",pch=25))
with(subset(flussi_ven, basin=="Ad"), cex=1.4,
     points(Temp, Flusso, col="#ff7f00",bg="#ff7f0055", lwd=1.4,
            type="p",pch=24))
with(subset(flussi_ven, basin=="NAd"), 
     points(Temp, Flusso, col="#d86b00", cex=1.8,lwd=2,
            type="p",pch=4))
with(subset(flussi_ven, basin=="Otr"), 
     points(Temp, Flusso, col="#6a3d9a", bg="#6a3d9a55", 
            type="p", lwd=2,cex=1.8,pch=8))
dev.off()





f<-k$kvw/100*(out$hg0w-out$hg0atm/sc$H)
summary(f)


F1<-((out$flux1*1000)*24)/200.59
plot(out$Temp, F1, main='Flux Wannikhof',
     col='red',ylab='Hg0 evasion (pmol/m2d)',  cex.lab=1.3)


plot(out$hg0atm,out$hg0w, col='blue',ylab='Hg0 water (pg/L)', xlab='Temperature (C)', cex.lab=1.3)

