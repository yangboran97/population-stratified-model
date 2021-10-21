#Basic paramter settings
library(tidyverse) 
library(deSolve) # ode solver
library(gridExtra)
library(RColorBrewer)
library(wesanderson)
library(egg)
library(foreach)
library(doParallel)
library(ggplot2)
library(gplots)
col_health_workers = "#FC9272" # Red 
col_normal = "#74C476" # Green

nolabels_theme <- theme(axis.title.x =element_blank(),
                        axis.text.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        plot.title = element_text(size = 12, face = "plain"),
                        legend.position = "none")
onlyx_theme <- theme(axis.title.y = element_blank(),
                     axis.text.y = element_blank(),
                     plot.title = element_text(size = 12, face = "plain"),
                     legend.position = "none")
onlyy_theme <- theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     plot.title = element_text(size = 12, face = "plain"),
                     legend.position = "none")
alllabels_theme <- theme(plot.title = element_text(size = 12, face = "plain"),
                         legend.position = "none")
theme_set(theme_minimal(base_size = 12))
d_E <- 1/4.4 
d_I <- 1/5 
C_o <- read.csv("XXX.csv",header=T)# Upload country-specific contact matrix
C <- as.matrix(C_o[1:11,2:12])
age_demo   <- c(0.1185748,0.11575,0.1286348,0.14328468,0.15014812,0.15436824,0.10537164,0.04967215,0.01849348,0.0157,0.0188,1443497378)
pop_total  <- age_demo[12]
age_demo   <- age_demo[1:11]
N_i        <- pop_total*age_demo
num_groups <- length(age_demo)
IFR        <- c(9.807703e-04, 3.277686e-03, 1.095386e-02, 3.660727e-02, 1.223397e-01, 4.088531e-01, 1.366367e+00,
                4.566330e+00, 1.526045e+01,0.6,9.807703e-04) 
IFR        <- IFR/100
v_ac <- c(0.231, 0.231, 0.323, 0.323, 0.323, 0.323, 0.323, 0.231, 0.231,0,0)#vaccnation coverage among normal population
u_var      <- c(0.4, 0.38, 0.79, 0.86, 0.8, 0.82, 0.88, 0.74, 0.74,0.04,0)
rate_I <-0.00016#rate of infections among all oversea input
oversea_input<-27610000
n_v_ac<-(sum(v_ac*N_i))/pop_total
C[1:9,11]<-C[1:9,11]*q_e
if (q_e ==1){
  C[10,11]<-2.02
}else {C[10,11]<-C[10,11]*5}

this_v_e   <- get_v_e(p = 0.5, y0 = 0.5, hinge_age = 50)
v_e_var    <- get_v_e(p = 0.5, y0 = 0.5, hinge_age = 50)
v_e_type   <- "aorn"

sero_none <- rep(0, 11) # no prior immunity
snm<-0.757

num_perday <- 1
list_all<-list_health_workers <- list_normal <-  vector(mode = "list")

scale_115 <- scale_u_for_R0(u_var, C, 1.15)
scale_12 <- scale_u_for_R0(u_var, C, 1.2)
scale_15 <- scale_u_for_R0(u_var, C, 1.5)
scale_16 <- scale_u_for_R0(u_var, C, 1.6)
scale_17 <- scale_u_for_R0(u_var, C, 1.7)
scale_18 <- scale_u_for_R0(u_var, C, 1.8)
scale_19 <- scale_u_for_R0(u_var, C, 1.9)
scale_26 <- scale_u_for_R0(u_var, C, 2.6)
scale_20 <- scale_u_for_R0(u_var, C, 2.0)
scale_25 <- scale_u_for_R0(u_var, C, 2.5)
scale_30 <- scale_u_for_R0(u_var, C, 3.0)
scale_35 <- scale_u_for_R0(u_var, C, 3.5)
scale_40 <- scale_u_for_R0(u_var, C, 4.0)
scale_45 <- scale_u_for_R0(u_var, C, 4.5)
scale_50 <- scale_u_for_R0(u_var, C, 5.0)

C_115 <- C/scale_115
C_12  <- C/scale_12
C_15  <- C/scale_15
C_16  <- C/scale_16
C_17  <- C/scale_17
C_18  <- C/scale_18
C_19  <- C/scale_19
C_20  <- C/scale_20
C_25  <- C/scale_25
C_26  <- C/scale_26
C_20  <- C/scale_20
C_30  <- C/scale_30
C_35  <- C/scale_35
C_45  <- C/scale_45
C_40  <- C/scale_40
C_50  <- C/scale_50
