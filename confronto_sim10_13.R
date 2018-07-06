
s10<-read.table("fort.88", header=F); 
names(s10)<-  c('silt','pom','ppom','psilt')

s13<-read.table("fort.89", header=F); 
names(s13)<-  c('silt','pom','ppom','psilt')

png('Silt_pom_Sediment.png')
par(mfrow=c(2,1))
plot(s10$silt, type='l',ylim=c(513754,513756), col='blue')
par(new=T)
plot(s13$silt, type='l',ylim=c(513754,513756), main ='Silt in Sediment')
legend(0,513755, legend=c('sim10','sim13 - tolte Vb negative'), 
       col=c('blue','black'), pch='-')
plot(s10$pom, type='l',ylim=c(27609.55,27609.65),col='blue')
par(new=T)
plot(s13$pom, type='l',ylim=c(27609.55,27609.65), main ='POM in Sediment')
dev.off()


plot(s10$ppom, type='l',ylim=c(5.099994,5.11),col='blue')
par(new=T)
plot(s13$ppom, type='l',ylim=c(5.099994,5.11), main ='POM in Sediment')

plot(s10$psilt, type='l',ylim=c(94,96),col='blue')
par(new=T)
plot(s13$psilt, type='l',ylim=c(94,96), main ='POM in Sediment')
legend(0,513754, legend=c('sim10','sim13 - no Vb negative'), 
       col=c('blue','black'), pch='-')
