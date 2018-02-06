setwd('C:/Users/Ginevra/Google Drive/MERCURIO/Hg_Literature_observtions')
setwd('C:/Users/gi/Dropbox/')
med_dati<-read.table('Hg0_flux_ng_m2_h.txt', header=T)
flussi_ven<-read.table('flussi_vento.txt', header=T)

setwd('C:/Users/Ginevra/Google Drive/MERCURIO/fortran_model/volat')
setwd('C:/Users/gi/Dropbox/fortran/volat')
# dimensionless H stimata a 0.3 a 25^ (Loux et al 2004) 
out<-read.table("fort.77", header=F); 
names(out)<-c('wind','hg0w','hg0atm','flux1','flux2')

k<-read.table("fort.82", header=F); 
names(k)<-c('kvw','kvb')
str(k)

sc<-read.table("fort.98", header=F); 
names(sc)<-c('co2','hg','H')
str(sc)
summary(sc$H)

vis<-read.table("fort.97", header=F)
names(vis)<-c('vis','rho','D','temp')

str(vis)
plot(vis$temp)
summary(vis$temp)

png('Temp_parameters.png', height = 15, 
    width = 21, res=300,units = 'cm')
par(mfrow=c(2,2), mar=c(4,5,2,1),mgp=c(2.2,1,0))
plot(vis$temp,vis$vis*1000,  col='red',
     ylab=expression(paste('cP')), 
     main='viscosity',
     xlab='Temperature (*C)', cex.lab=1.2)
plot(vis$temp,vis$D,      col='purple',
     ylab=expression(paste('cm '^2*' s'^-1)),main='Hg diffusivity', 
     xlab='Temperature (*C)', cex.lab=1.2)

plot(vis$temp,sc$H,  main='Henry coefficient',  col='royalblue',
     ylab=expression(paste('dimensionless')), 
     xlab='Temperature (*C)', cex.lab=1)
plot(vis$temp,sc$hg,  col='chartreuse2', 
     main='Hg Schmidt number (ScHg)',
     ylab=expression(paste('dimensionless')), 
     xlab='Temperature (*C)', cex.lab=1)
dev.off()

hg0_ion<-flussi_ven[(flussi_ven$basin=='Ionio'),]
hg0_tir<-flussi_ven[(flussi_ven$basin=='Tirreno'),]
hg0_Wm<-flussi_ven[(flussi_ven$basin=='Wmed'),]
hg0_Em<-flussi_ven[(flussi_ven$basin=='Emed'),]
hg0_S<-flussi_ven[(flussi_ven$basin=='Sicily'),]
hg0_Ad<-flussi_ven[(flussi_ven$basin=='Ad'),]
hg0_NAd<-flussi_ven[(flussi_ven$basin=='NAd'),]

par('usr') # return default x1 x2 y1 y2 
           # of the last plot executed
png('Hg0_mod_conc_test_.png', height = 15, 
    width = 21, res=300,units = 'cm')
par(mfrow=c(1,2), mar=c(1,2,2,2), oma=c(0,2,2,0.4))
boxplot(out$hg0w, ylim=c(0,120),
        col='white',ylab='', main='water', 
        cex.lab=1.3)
mtext(expression(paste('pg L'^-1) ),side=2,
      line=1.95, cex=1.2)
par(new=T)
stripchart(hg0_tir$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=21,
           col='#33a02c',bg='#33a02c44', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_Wm$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=22,
           col='#b2df8a',bg='#b2df8a44', cex=1.3, vertical =T)
par(new=T)
stripchart(hg0_ion$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=23,
           col='#a6cee3',bg='#A6CEE344', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_S$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=3,lwd=2,
           col='#1f78b4',bg='#1f78b444', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_Em$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=25,
           col='#e31a1c',bg='#e31a1c44', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_Ad$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=24,
           col='#ff7f00',bg='#ff7f0044', cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_NAd$Hg0, 1, method = 'jitter', 
           ylim=c(0,120),pch=4,lwd=2,
           col='#d86b00', cex=1.5, 
           vertical =T)

boxplot(out$hg0atm, col='white',ylab='',width = .91,
        ylim=c(0,4),xlim=c(0,2),
        main='atmosphere',cex.lab=1.3)
mtext(expression(paste('ng m'^-3)), 
      side=2,line=1.95, cex=1.2)
mtext(expression(paste('Hg'^0*' concentrations')),
      side=3,line=0.0, cex=1.6, outer=T, font = 2)
legend(.024,4.2,cex=.9, bty = 'n',
col=c("#33a02c", "#b2df8a",
"#a6cee3","#1f78b4",'#6a3d9a',"#e31a1c", "#ff7f00","#d86b00"),lwd=1.5,
pt.bg=c("#33a02c55", "#b2df8a55","#a6cee355","#1f78b455",'#6a3d9a',"#e31a1c55",
               "#ff7f0055","#fb9a99"), pch=c(21,22,23,3,8,25,24,4),
       legend=c('Tirrenian','West - Med','Ionian',
                'Sicily','Otranto','Est - Med',
                'Adriatic', 'Northern Adr'))

par(new=T)
stripchart(hg0_tir$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=21,
           col='#33a02c',bg='#33a02c44', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_Wm$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=22,
           col='#b2df8a',bg='#b2df8a44', cex=1.3, vertical =T)
par(new=T)
stripchart(hg0_ion$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=23,
           col='#a6cee3',bg='#A6CEE344', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_S$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=3,lwd=2,
           col='#1f78b4',bg='#1f78b444', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_Em$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=25,
           col='#e31a1c',bg='#e31a1c44', 
           cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_Ad$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=24,
           col='#ff7f00',bg='#ff7f0044', cex=1.5, vertical =T)
par(new=T)
stripchart(hg0_NAd$TGM, 1, method = 'jitter', 
           ylim=c(0,4),pch=4,lwd=2,
           col='#d86b00', cex=1.5, 
           vertical =T)
dev.off()
