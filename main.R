#Main proceeding code
setwd("XXX")
q_e=1#rate of infectious outliers of particular 7day(0.309)14days(0.105)21days(0.045)28days(0.023)
source("functions.R")
source("framework.R")
source("settings.R")
this_C <- C/scale_50
ptm <- proc.time()  
for (i in seq(0, 100, by =1)){
  j <- i/100
  list_health_workers[[paste0(i)]] <- my_sim(this_C, j, "health_workers", num_perday, v_e_type, this_v_e)
  list_normal[[paste0(i)]] <- my_sim(this_C, j, "normal", num_perday, v_e_type, this_v_e)
  list_all[[paste0(i)]] <- my_sim(this_C, j, "all", num_perday, v_e_type, this_v_e)
}
time<-0:365
options(scipen=200)

h10I1<-list_health_workers[["10"]][["I1"]]
h10I2<-list_health_workers[["10"]][["I2"]]
h10I3<-list_health_workers[["10"]][["I3"]]
h10I4<-list_health_workers[["10"]][["I4"]]
h10I5<-list_health_workers[["10"]][["I5"]]
h10I6<-list_health_workers[["10"]][["I6"]]
h10I7<-list_health_workers[["10"]][["I7"]]
h10I8<-list_health_workers[["10"]][["I8"]]
h10I9<-list_health_workers[["10"]][["I9"]]
h10I10<-list_health_workers[["10"]][["I10"]]

hI_total_10<-h10I1+h10I2+h10I3+h10I4+h10I5+h10I6+h10I7+h10I8+h10I9+h10I10
hI_total_10_rate<-(hI_total_10/pop_total)*100
percent<-as.numeric(hI_total_10)
strat<-"health_workers"
df_health_workers<-data.frame(time,percent,strat)
n10I1<-list_normal[["10"]][["I1"]]
n10I2<-list_normal[["10"]][["I2"]]
n10I3<-list_normal[["10"]][["I3"]]
n10I4<-list_normal[["10"]][["I4"]]
n10I5<-list_normal[["10"]][["I5"]]
n10I6<-list_normal[["10"]][["I6"]]
n10I7<-list_normal[["10"]][["I7"]]
n10I8<-list_normal[["10"]][["I8"]]
n10I9<-list_normal[["10"]][["I9"]]
n10I10<-list_normal[["10"]][["I10"]]

nI_total_10<-n10I1+n10I2+n10I3+n10I4+n10I5+n10I6+n10I7+n10I8+n10I9+n10I10
nI_total_10_rate<-(nI_total_10/pop_total)*100
percent<-as.numeric(nI_total_10)
strat<-"normal"
df_normal<-data.frame(time,percent,strat)
proc.time() - ptm
b1I1<-list_all[["1"]][["I1"]]
b1I2<-list_all[["1"]][["I2"]]
b1I3<-list_all[["1"]][["I3"]]
b1I4<-list_all[["1"]][["I4"]]
b1I5<-list_all[["1"]][["I5"]]
b1I6<-list_all[["1"]][["I6"]]
b1I7<-list_all[["1"]][["I7"]]
b1I8<-list_all[["1"]][["I8"]]
b1I9<-list_all[["1"]][["I9"]]
b1I10<-list_all[["1"]][["I10"]]

bI_total_1<-b1I1+b1I2+b1I3+b1I4+b1I5+b1I6+b1I7+b1I8+b1I9+b1I10
bI_total_1_rate<-(bI_total_1/pop_total)*100
percent<-as.numeric(bI_total_1)
strat<-"all"
df_baseline<-data.frame(time,percent,strat)


df <- rbind(df_health_workers,df_normal)

infect_10 <- plot_strat_overtime("I",df_health_workers,df_normal,df_baseline, 0.1/num_perday) +
  alllabels_theme + 
  ggtitle("10% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))

h30I1<-list_health_workers[["30"]][["I1"]]
h30I2<-list_health_workers[["30"]][["I2"]]
h30I3<-list_health_workers[["30"]][["I3"]]
h30I4<-list_health_workers[["30"]][["I4"]]
h30I5<-list_health_workers[["30"]][["I5"]]
h30I6<-list_health_workers[["30"]][["I6"]]
h30I7<-list_health_workers[["30"]][["I7"]]
h30I8<-list_health_workers[["30"]][["I8"]]
h30I9<-list_health_workers[["30"]][["I9"]]
h30I10<-list_health_workers[["30"]][["I10"]]

hI_total_30<-h30I1+h30I2+h30I3+h30I4+h30I5+h30I6+h30I7+h30I8+h30I9+h30I10
hI_total_30_rate<-(hI_total_30/pop_total)*100
percent<-as.numeric(hI_total_30)
strat<-"health_workers"
df_health_workers<-data.frame(time,percent,strat)
n30I1<-list_normal[["30"]][["I1"]]
n30I2<-list_normal[["30"]][["I2"]]
n30I3<-list_normal[["30"]][["I3"]]
n30I4<-list_normal[["30"]][["I4"]]
n30I5<-list_normal[["30"]][["I5"]]
n30I6<-list_normal[["30"]][["I6"]]
n30I7<-list_normal[["30"]][["I7"]]
n30I8<-list_normal[["30"]][["I8"]]
n30I9<-list_normal[["30"]][["I9"]]
n30I10<-list_normal[["30"]][["I10"]]

nI_total_30<-n30I1+n30I2+n30I3+n30I4+n30I5+n30I6+n30I7+n30I8+n30I9+n30I10
nI_total_30_rate<-(nI_total_30/pop_total)*100
percent<-as.numeric(nI_total_30)
strat<-"normal"
df_normal<-data.frame(time,percent,strat)
df <- rbind(df_health_workers,df_normal)
infect_30 <- plot_strat_overtime("I",df_health_workers ,df_normal,df_baseline, 0.3/num_perday) +
  onlyx_theme  + 
  ggtitle("30% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))

h50I1<-list_health_workers[["50"]][["I1"]]
h50I2<-list_health_workers[["50"]][["I2"]]
h50I3<-list_health_workers[["50"]][["I3"]]
h50I4<-list_health_workers[["50"]][["I4"]]
h50I5<-list_health_workers[["50"]][["I5"]]
h50I6<-list_health_workers[["50"]][["I6"]]
h50I7<-list_health_workers[["50"]][["I7"]]
h50I8<-list_health_workers[["50"]][["I8"]]
h50I9<-list_health_workers[["50"]][["I9"]]
h50I10<-list_health_workers[["50"]][["I10"]]

hI_total_50<-h50I1+h50I2+h50I3+h50I4+h50I5+h50I6+h50I7+h50I8+h10I9+h50I10
hI_total_50_rate<-(hI_total_50/pop_total)*100
percent<-as.numeric(hI_total_50)
strat<-"health_workers"
df_health_workers<-data.frame(time,percent,strat)
n50I1<-list_normal[["50"]][["I1"]]
n50I2<-list_normal[["50"]][["I2"]]
n50I3<-list_normal[["50"]][["I3"]]
n50I4<-list_normal[["50"]][["I4"]]
n50I5<-list_normal[["50"]][["I5"]]
n50I6<-list_normal[["50"]][["I6"]]
n50I7<-list_normal[["50"]][["I7"]]
n50I8<-list_normal[["50"]][["I8"]]
n50I9<-list_normal[["50"]][["I9"]]
n50I10<-list_normal[["50"]][["I10"]]
nI_total_50<-n50I1+n50I2+n50I3+n50I4+n50I5+n50I6+n50I7+n50I8+n50I9+n50I10
nI_total_50_rate<-(nI_total_50/pop_total)*100
percent<-as.numeric(nI_total_50)
strat<-"normal"
df_normal<-data.frame(time,percent,strat)
df <- rbind(df_health_workers,df_normal)
infect_50 <- plot_strat_overtime("I",df_health_workers ,df_normal,df_baseline, 0.5/num_perday) +
  onlyx_theme  + 
  ggtitle("50% vaccine supply") + 
  theme(plot.title = element_text(color = "black"))


b1D1<-list_all[["1"]][["D1"]]
b1D2<-list_all[["1"]][["D2"]]
b1D3<-list_all[["1"]][["D3"]]
b1D4<-list_all[["1"]][["D4"]]
b1D5<-list_all[["1"]][["D5"]]
b1D6<-list_all[["1"]][["D6"]]
b1D7<-list_all[["1"]][["D7"]]
b1D8<-list_all[["1"]][["D8"]]
b1D9<-list_all[["1"]][["D9"]]
b1D10<-list_all[["1"]][["D10"]]

bD_total_1<-b1D1+b1D2+b1D3+b1D4+b1D5+b1D6+b1D7+b1D8+b1D9+b1D10
bD_total_1_rate<-(bD_total_1/pop_total)*100
percent<-as.numeric(bD_total_1)
strat<-"all"
df_baseline<-data.frame(time,percent,strat)
h10D1<-list_health_workers[["10"]][["D1"]]
h10D2<-list_health_workers[["10"]][["D2"]]
h10D3<-list_health_workers[["10"]][["D3"]]
h10D4<-list_health_workers[["10"]][["D4"]]
h10D5<-list_health_workers[["10"]][["D5"]]
h10D6<-list_health_workers[["10"]][["D6"]]
h10D7<-list_health_workers[["10"]][["D7"]]
h10D8<-list_health_workers[["10"]][["D8"]]
h10D9<-list_health_workers[["10"]][["D9"]]
h10D10<-list_health_workers[["10"]][["D10"]]

hD_total_10<-h10D1+h10D2+h10D3+h10D4+h10D5+h10D6+h10D7+h10D8+h10D9+h10D10
hD_total_10_rate<-(hD_total_10/pop_total)*100
percent<-as.numeric(hD_total_10)
strat<-"health_workers"
df_health_workers<-data.frame(time,percent,strat)
n10D1<-list_normal[["10"]][["D1"]]
n10D2<-list_normal[["10"]][["D2"]]
n10D3<-list_normal[["10"]][["D3"]]
n10D4<-list_normal[["10"]][["D4"]]
n10D5<-list_normal[["10"]][["D5"]]
n10D6<-list_normal[["10"]][["D6"]]
n10D7<-list_normal[["10"]][["D7"]]
n10D8<-list_normal[["10"]][["D8"]]
n10D9<-list_normal[["10"]][["D9"]]
n10D10<-list_normal[["10"]][["D10"]]

nD_total_10<-n10D1+n10D2+n10D3+n10D4+n10D5+n10D6+n10D7+n10D8+n10D9+n10D10
nD_total_10_rate<-(nD_total_10/pop_total)*100
percent<-as.numeric(nD_total_10)
strat<-"normal"
df_normal<-data.frame(time,percent,strat)
df <- rbind(df_health_workers,df_normal)
mort_10 <- plot_strat_overtime("D", df_health_workers, df_normal,df_baseline, 0.1/num_perday) 


h30D1<-list_health_workers[["30"]][["D1"]]
h30D2<-list_health_workers[["30"]][["D2"]]
h30D3<-list_health_workers[["30"]][["D3"]]
h30D4<-list_health_workers[["30"]][["D4"]]
h30D5<-list_health_workers[["30"]][["D5"]]
h30D6<-list_health_workers[["30"]][["D6"]]
h30D7<-list_health_workers[["30"]][["D7"]]
h30D8<-list_health_workers[["30"]][["D8"]]
h30D9<-list_health_workers[["30"]][["D9"]]
h30D10<-list_health_workers[["30"]][["D10"]]

hI_total_30<-h30D1+h30D2+h30D3+h30D4+h30D5+h30D6+h30D7+h30D8+h30D9+h30D10
hI_total_30_rate<-(hI_total_30/pop_total)*100
percent<-as.numeric(hI_total_30)
strat<-"health_workers"
df_health_workers<-data.frame(time,percent,strat)
n30D1<-list_normal[["30"]][["D1"]]
n30D2<-list_normal[["30"]][["D2"]]
n30D3<-list_normal[["30"]][["D3"]]
n30D4<-list_normal[["30"]][["D4"]]
n30D5<-list_normal[["30"]][["D5"]]
n30D6<-list_normal[["30"]][["D6"]]
n30D7<-list_normal[["30"]][["D7"]]
n30D8<-list_normal[["30"]][["D8"]]
n30D9<-list_normal[["30"]][["D9"]]
n30D10<-list_normal[["30"]][["D10"]]

nI_total_30<-n30D1+n30D2+n30D3+n30D4+n30D5+n30D6+n30D7+n30D8+n30D9+n30D10
nI_total_30_rate<-(nI_total_30/pop_total)*100
percent<-as.numeric(nI_total_30)
strat<-"normal"
df_normal<-data.frame(time,percent,strat)
df <- rbind(df_health_workers,df_normal)

mort_30 <- plot_strat_overtime("D", df_health_workers,df_normal,df_baseline, 0.3/num_perday) + 
  onlyx_theme

h50D1<-list_health_workers[["50"]][["D1"]]
h50D2<-list_health_workers[["50"]][["D2"]]
h50D3<-list_health_workers[["50"]][["D3"]]
h50D4<-list_health_workers[["50"]][["D4"]]
h50D5<-list_health_workers[["50"]][["D5"]]
h50D6<-list_health_workers[["50"]][["D6"]]
h50D7<-list_health_workers[["50"]][["D7"]]
h50D8<-list_health_workers[["50"]][["D8"]]
h50D9<-list_health_workers[["50"]][["D9"]]
h50D10<-list_health_workers[["50"]][["D10"]]

hI_total_50<-h50D1+h50D2+h50D3+h50D4+h50D5+h50D6+h50D7+h50D8+h50D9+h50D10
hI_total_50_rate<-(hI_total_50/pop_total)*100
percent<-as.numeric(hI_total_50)
strat<-"health_workers"
df_health_workers<-data.frame(time,percent,strat)
n50D1<-list_normal[["50"]][["D1"]]
n50D2<-list_normal[["50"]][["D2"]]
n50D3<-list_normal[["50"]][["D3"]]
n50D4<-list_normal[["50"]][["D4"]]
n50D5<-list_normal[["50"]][["D5"]]
n50D6<-list_normal[["50"]][["D6"]]
n50D7<-list_normal[["50"]][["D7"]]
n50D8<-list_normal[["50"]][["D8"]]
n50D9<-list_normal[["50"]][["D9"]]
n50D10<-list_normal[["50"]][["D10"]]

nI_total_50<-n50D1+n50D2+n50D3+n50D4+n50D5+n50D6+n50D7+n50D8+n50D9+n50D10
nI_total_50_rate<-(nI_total_50/pop_total)*100
percent<-as.numeric(nI_total_50)
strat<-"normal"
df_normal<-data.frame(time,percent,strat)
df <- rbind(df_health_workers,df_normal)

mort_50 <- plot_strat_overtime("D", df_health_workers,df_normal,df_baseline, 0.5/num_perday) + 
  onlyx_theme


sub_panel2 <- ggarrange(infect_10,infect_30,infect_50,nrow=1)#PLOT: Fig. 2A and 2B
#PLOT:Fig.4B and 4C
RE <- read.csv("XXX.csv",header=T)#Upload the estimated 'Re' matrix under different vaccine coverage and efficiencies settings.
Re <- as.matrix(RE[1:13,1:21])
library(MASS)
ve<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
vc<-c(40,45,50,55,60,65,70,75,80,85,90,95,100)
persp(x=y1,y=x1,C,theta =120, phi =25
      ,ltheta =80,col =rainbow(77,start = 0.999,end=.15),shade = 0.40, expand = 0.7,border =2,xlab = 'Vaccine efficiancy(%)', ylab ='Vaccine coverage(%)', zlab = "cases",main = NULL)
colors<-colorRampPalette(c( "#FFFFCC","#FD8D3C","#FC4E2A","#7F0000"),bias=0.9)                              
Re_ct<-filled.contour(x=ve,y=vc,z = Re,plot.axes = {axis(1, seq(40, 100, by = 10)); axis(2, seq(0, 100, by = 20))},
                      key.title = title(main = "R0"),nlevels=20,color.palette = colors,key.axes = axis(4, seq(0,5, by = 1)),
                      frame.plot = T,plot.title=title(main="B.1.1.7",xlab='Vaccine coverage(%)',ylab='Vaccine efficiancy(%)'))
