#
# kv calculated on Soerensen et al., 2010 -..
#
# ScCO2 = 0.11 Tc^2 -6.16 Tc + 644.7 
# ScHg0 = v/D    Schmidt number for Hg(0)
# v = N/p(rho) = 0.017*e^0.025*T   kinematic viscosity (cm2 s-1)
# N = Viscosity of water (cP - centipoise)   1 P= 1Pa*s , 
# log(N)= (1301/(998.333+8.1855(Tc-20) + 0.00585(Tc-20)^2))- 3.30233
# Rho = density
# D = diffusivivty  (7.4*10^8*(saf*Mw)^1/2*Tk)/N*Vb^0.6

#---set parameters
Sal<-18

Tc<-c(5.069480339, 5.457643648, 6.115080574,8.460433969,14.51500356,18.30181401,
      20.6778546, 21.35133381, 18.46984289, 12.69752051, 11.23118764, 5.679422088)

Tk<-Tc+273.15

saf<-2.26  # Solvent association factor introduced to define the effective molecular 
Mw <-18    # g/mol molecular weight of water
Vb<-12.74  # (cm3 mol-1) Molal volume of mercury at its normal boiling

A<-0.25  # (unitless) Constant based on the 
          # Weibull distribution of wind speeds over oceans

#monthly mean of data U10 and V10 from WFR chem mod 
#m/s
u<-c(1.347741947,.183236717,1.061082668,0.578819799,0.632989111,0.7980219,
     2.283728059,2.183822636,1.944001452,0.869728034,0.666685241,1.100868196)

#----------- calc density(T, sal) ------
a<-0.824493 - 0.0040899*Tc + 0.000076438*Tc^2 -
  0.00000082467*Tc^3 + 0.0000000053675*Tc^4
b<--0.005724 + 0.00010227*Tc - 0.0000016546*Tc^2 
Rho<- 1000*(1-(Tc+288.9414)/(508929.2*(Tc+68.12963))*(Tc-3.9863)^2) 	# density T rr
Rho2<- Rho + a*Sal + b*Sal^(3/2) + 0.00048314*Sal^2 					# density T and sal corrected
                                                              # mg/cm3
#----------- calc viscosity (T, sal), diffusivity and kin viscosity  ------
library(marelac) # Function viscosity calculates the shear 
                  # viscosity of water, in centipoise
 N<-viscosity(S = 18, t = Tc, P = 1)    #formula is valid for 0 < t < 30 and 0 < S < 36

 D<-(7.4*10^-8*((saf*Mw)^1/2)*Tk)/(N*(Vb^0.6) )    # DIFFUSIVITY cm2/s
 v<-N/Rho2  # kinematic viscosity cm2/s

 #----------- Schmidt number
 ScCO2 <-0.11*(Tc^2)-(6.16*Tc) + 644.7    # ok --- Burke, J.; Hoyer, M.; Keeler, G.; Scherbatskoy, T. 
                                          # Water, Air, Soil
                                          # Pollut. 1995, 80, 353
 ScHg0 <- v/D 
 
 #  u wind vel [m/s]
 fr     <-(ScHg0/ScCO2)^-.5  # [m/hr-1]
 kvol  <-A*(u^2)*fr     # range 0 -8 
 plot( kvol, type='l')
mean(kvol)  #2.7   # comparable to k (m/day) in Rolfhus and fitz 2001
plot( kvol, Tc)


  
  setwd("C:/Users/Ginevra/Dropbox/BlackSea2/implementazione/Deposizione_atm")
write.table(kvol, file='kvol_soerensen_2010.txt')


salinity<-0.0018066*Cl_mg_L    # Cl (mg/L) 


#bouchet et al. 2011   kHg comprese tra 0 e 200 cm/h  -> 0.2 m/h
#  2.4  m/day 

k_CO2=(0.1+2.26)*u    #bouchet et al. 2011
k_Hg<-k_CO2*fr           #[m/s]
mean(k_Hg)              #
k_Hg_cm_h<-(k_Hg*100)/60*60 

#from rolfhus et fitzg 2001
w<-5
u<-sqrt(w)
kvol_wannikof<-0.31*w^2*((ScHg0/600)^-1/2)  #cm/h
kvol_wannikof*24 #m/day


BS_surf<-2.961E+11
evasion<-5     #kmol/y
a<-evasion/BS_surf #kmol/m2y 
b<-a*10^3 #mol/m2y 
evasion_pmol_m2_d<-b*10^12/365 #pmol/m2d
evasion_pmol_m2_d


# Hg0 evasion compiled b ysoerensen et al 2013
ev_ng_m2_h<-c(1.9,0.1,1.67,5.57,0.8,2.86)

ev_pmol_m2_d<-ev_ng_m2_d*24/200.59*1000
mean(ev_pmol_m2_d)
range(ev_pmol_m2_d)
