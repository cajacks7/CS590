###############################################################################
#####
##### CS590 Exercise 9
##### By: Colin Jackson
##### Last Edit: 11/11/2019
#####
################################################################################

library(stringr)

setwd("C:\\Users\\cajacks7\\Documents")

nut <- read.csv("peanut_lines.csv", header = T)

head(nut, 25)

length(grep("Bailey", nut$Parentage))

nut$Bailey_Parent <- ifelse(grepl("Bailey",nut$Parentage)==T,"Yes","No")

round(nrow(nut[(nut$FAG =="ol ol" & nut$Bailey_Parent== "Yes"),])/nrow(nut[(nut$FAG != "Check"),]),3)*100

nut$Parentage <- str_replace_all(nut$Parentage, "Bailey", "N03081T")

head(nut,10)

yield <- read.delim("peanut_yield.txt", header = T, sep = "\t")

peanut_db <- merge(nut, yield, by.x = "NC_Accession", all.y = T)

peanut_db$Bailey_Parent <- ifelse(is.na(peanut_db$Bailey_Parent),"Missing",peanut_db$Bailey_Parent)

#means <- aggregate(peanut_db$Yield, by= list(peanut_db$Bailey_Parent), FUN = mean)

#mean(peanut_db$Yield[(peanut_db$NC_Accession=="Bailey"),])

#test <- ifelse(peanut_db$NC_Accession=="Bailey",)

z <- vector()
for(i in 1:length(peanut_db$NC_Accession)){
  if (peanut_db$NC_Accession[i]=="Bailey"){
    x <- print(peanut_db$Yield[i])
    z <- rbind(z,x)
  }
  
}
  
a <- vector()
for(i in 1:length(peanut_db$NC_Accession)){
  if (peanut_db$Bailey_Parent[i]=="Yes"){
     y <-print(peanut_db$Yield[i])
    a <- rbind(a,y)
  }
}

m.z <-mean(z)
m.a<- mean(a)
final <- rbind(m.z,m.a)
colnames(final)<- c("Yield")
row.names(final) <- c("Bailey","Other")

final

