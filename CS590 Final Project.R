#########################################################
###
### CS590 Final Project
### Colin Jackson
### Last Edit: 2/4/2019
###
#########################################################

setwd("C:/Users/cajacks7/Documents/SNP_Array_Info/Tropical_Pine")

library(ggplot2)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(gridExtra)

#############################################################
### 
### Venn Diagram
###
#############################################################

#Read in data for left flanking regions and select those with full flanking regions
Combo_l0 <- read.csv("Combo_left_flank_os.csv", h=T, sep=",")
Combo_l0<- Combo_l0[,c(1,3,5,7,9,11,2,4,6,8,10)]
colnames(Combo_l0)<- c("Left_flank", "ID_G", "ID_M", "ID_C", "ID_P", "ID_T", "Greg", "Max", "Carp", "Pat", "Tec")
Combo_l0 <- subset(Combo_l0, nchar(as.character(Left_flank)) == 35)

head(Combo_l0)

#Creates 0/1 incidence field for species found in
Combo_l0$Greg<- as.numeric(ifelse(Combo_l0$Greg == "", "0","1"))
Combo_l0$Max<- as.numeric(ifelse(Combo_l0$Max == "", "0","1"))
Combo_l0$Carp<- as.numeric(ifelse(Combo_l0$Carp == "", "0","1"))
Combo_l0$Pat<- as.numeric(ifelse(Combo_l0$Pat == "", "0","1"))
Combo_l0$Tec<- as.numeric(ifelse(Combo_l0$Tec == "", "0","1"))


#filter to just 0/1 field
Sp.sub_l0 <- Combo_l0[,7:11]
Sp.sub_l0$Shared <- rowSums(Sp.sub_l0)

Sp.sub_l0v <- Sp.sub_l0[,1:5]
colnames(Sp.sub_l0v) <- c("P. greggii", "P. maximinoi", "P. oocarpa", "P. patula", "P. tecunumanii")

library(venn)
pdf(file= "Venn_left_0_diff.pdf", height = 10, width = 10)
venn(Sp.sub_l0v, ellipse = F, zcolor = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),size = 36, cexil = 1.15,cexsn = 1.4)
dev.off()

#Right Flank data
Combo_r0 <- read.csv("Combo_right_flank_os.csv", h=T, sep=",")
Combo_r0<- Combo_r0[,c(1,3,5,7,9,11,2,4,6,8,10)]
colnames(Combo_r0)<- c("Right_flank", "ID_G", "ID_M", "ID_C", "ID_P", "ID_T", "Greg", "Max", "Carp", "Pat", "Tec")
Combo_r0 <- subset(Combo_r0, nchar(as.character(Right_flank)) == 35)

#Creates 0/1 incidence field for probes
Combo_r0$Greg<- as.numeric(ifelse(Combo_r0$Greg == "", "0","1"))
Combo_r0$Max<- as.numeric(ifelse(Combo_r0$Max == "", "0","1"))
Combo_r0$Carp<- as.numeric(ifelse(Combo_r0$Carp == "", "0","1"))
Combo_r0$Pat<- as.numeric(ifelse(Combo_r0$Pat == "", "0","1"))
Combo_r0$Tec<- as.numeric(ifelse(Combo_r0$Tec == "", "0","1"))

#filter to just 0/1 field
Sp.sub_r0 <- Combo_r0[,7:11]
Sp.sub_r0$Shared <- rowSums(Sp.sub_r0)

Sp.sub_r0v <- Sp.sub_r0[,1:5]
colnames(Sp.sub_r0v) <- c("P. greggii", "P. maximinoi", "P. oocarpa", "P. patula", "P. tecunumanii")

pdf(file= "Venn_right_0_diff.pdf", height = 10, width = 10)
venn(Sp.sub_r0v, ellipse = F, zcolor = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),size = 36, cexil = 1.15,cexsn = 1.4)
dev.off()

############################################################################################
###
###PCA Analysis
###
############################################################################################

#set seed for sampling reproducability
set.seed(5)

#Taking random sample of 50K probes from the 75K total observations for left flanks
sub_sample_l <- Sp.sub_l0v[sample(nrow(Sp.sub_l0v), 50000), ]
#sub_sample <-data.frame(Sp.sub_l0v[1:10000,])
sub_sample_l$n <- 1:length(sub_sample_l$Gre)
sub_sample_l$probe <- rep("Probe", length(sub_sample_l$n))
sub_sample_l$Probe <- paste(sub_sample_l$probe, sub_sample_l$n, sep = '_')
sub_sample_l <- sub_sample_l[,c(8,1,2,3,4,5)]

#Perform pca analysis using princomp function 
pca_l <- princomp(sub_sample_l[,-1], cor=T)
print(pca_l)
summary(pca_l)
plot(pca_l)

#Extract scores and loadings from the pca object for plotting
scores <- data.frame(pca_l$scores[,1:2]) 
loadings <- data.frame(pca_l$loadings[,1:2])
loadings2 <- cbind(rownames(loadings), data.frame(loadings, row.names=NULL))

#Define color palette
cbPalette <- c("#006A40FF" ,"#8AB8CFFF","#95828DFF", "#F2990CFF", "#5A5895FF","#708C98FF","#D86C4FFF","#F08892FF")

#plot the PCA results using ggplot and annotate 
left_flank_PCA <- ggplot(data=scores, aes(x=Comp.1, y=Comp.2))+
  geom_point(pch=16,color='white') + theme_bw()+
  geom_segment(data = loadings2, aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2,color= rownames(loadings)), 
               arrow=arrow(length=unit(0.4,"cm")),size=1)+
  scale_colour_manual(values=cbPalette, aesthetics = "colour") +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=.3)+
  #geom_vline(xintercept=0, linetype="dashed", color = "red", size=.3)+
  labs(y = "PC2 (23.7%)", x = "PC1 (28.8%)") +
  annotate('text', label='a)', x=-1, y=1, cex = 5,fontface =1) +
  annotate('text', label='P. maximinoi', x=-.7, y=-.25, cex = 5) +
  annotate('text', label='P. greggii', x=.35, y=.2, cex = 5) +
  annotate('text', label='P. patula', x=0, y=.75, cex = 5) +
  annotate('text', label='P. oocarpa', x=0, y=-.45, cex = 5) +
  annotate('text', label='P. tecunumanii', x=.65, y=-.2, cex = 5) +
  scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1.), limits=c(-1,1)) +
  scale_x_continuous(breaks=c(-1,-.5,0,.75,1.5), limits=c(-1,1.5)) + 
  theme(axis.text=element_text(size=10, colour = 1),
        axis.title=element_text(size=13,face="plain")) +
  theme(axis.line = element_line(size = .3, color = "black")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")


#Taking random sample of 50K probes from the 75K total observations for right flanks
sub_sample_r <- Sp.sub_r0v[sample(nrow(Sp.sub_r0v), 50000), ]
#sub_sample <-data.frame(Sp.sub_l0v[1:10000,])
sub_sample_r$n <- 1:length(sub_sample_r$Gre)
sub_sample_r$probe <- rep("Probe", length(sub_sample_r$n))
sub_sample_r$Probe <- paste(sub_sample_r$probe, sub_sample_r$n, sep = '_')
sub_sample_r <- sub_sample_r[,c(8,1,2,3,4,5)]

#Perform pca analysis using princomp function 
pca_r <- princomp(sub_sample_r[,-1], cor=T)
print(pca_r)
summary(pca_r)
plot(pca_r)

#Extract scores and loadings from the pca object for plotting
scores_r <- data.frame(pca_r$scores[,1:2]) 
loadings_r <- data.frame(pca_r$loadings[,1:2])
loadings_r2 <- cbind(rownames(loadings_r), data.frame(loadings_r, row.names=NULL))

#plot the PCA results using ggplot and annotate 
right_flank_PCA <- ggplot(data=scores_r, aes(x=Comp.1, y=Comp.2))+
  geom_point(pch=16,color='white') + theme_bw()+
  geom_segment(data = loadings_r2, aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2,color= rownames(loadings_r)), 
               arrow=arrow(length=unit(0.4,"cm")),size=1)+
  scale_colour_manual(values=cbPalette, aesthetics = "colour") +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=.3)+
  #geom_vline(xintercept=0, linetype="dashed", color = "red", size=.3)+
  labs(y = "PC2 (23.6%)", x = "PC1 (28.7%)") +
  annotate('text', label='b)', x=-1, y=1, cex = 5,fontface =1) +
  annotate('text', label='P. maximinoi', x=-.7, y=-.25, cex = 5) +
  annotate('text', label='P. greggii', x=.35, y=.15, cex = 5) +
  annotate('text', label='P. patula', x=0, y=.75, cex = 5) +
  annotate('text', label='P. oocarpa', x=0, y=-.45, cex = 5) +
  annotate('text', label='P. tecunumanii', x=.6, y=-.2, cex = 5) +
  scale_y_continuous(breaks=c(-1,-0.5,0,0.5,1.), limits=c(-1,1)) +
  scale_x_continuous(breaks=c(-1,-.5,0,.75,1.5), limits=c(-1,1.5)) + 
  theme(axis.text=element_text(size=10, colour = 1),
        axis.title=element_text(size=13,face="plain")) +
  theme(axis.line = element_line(size = .3, color = "black")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

#Combine both PCA graphics into one figure
PCA_both_flanks <- grid.arrange(left_flank_PCA,right_flank_PCA, ncol=2)

#print to pdf
pdf("PCA_sequence_sim_0_diff.pdf", height = 7, width = 12)
plot(PCA_both_flanks)
dev.off()


