#######################################3
###
### Week 5 Exercise
###
###################################################

library(xlsx)

setwd("C:\\Users\\cajacks7\\Documents")

family <- c('N12006', 'N13003', 'N13042', 'N14002', 'N16021')
OL_Ratio <- c(76.2, 82.1, 78.9, 74.3, 77.7)
Leaf_Spot <- c(5.5, 7.2, 6.5, 4.2, 3.1)
Drought <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
dat <- data.frame(row.names = family, OL_Ratio, Leaf_Spot, Drought)
dat

dat[dat['Drought']==T,]

peanut_lines <- read.csv("peanut_lines.csv", header = T)
peanut_disease <- read.table("peanut_disease.txt", sep = '\t', header = T)

colnames(peanut_lines)
colnames(peanut_disease)


disease_data <- merge(peanut_lines, peanut_disease, by.x='NC_Accession', by.y='NC_Accession', all.y = T)

leaf.spot <- disease_data[disease_data$Rating == 'LS',]

leaf.spot <- leaf.spot[!is.na(leaf.spot$Disease),]

mean(leaf.spot$Disease[leaf.spot$Year==2017], na.rm = T)

line_names = c('Bailey', 'Bailey II', 'Wynne', 'Sullivan', 'Emery', 'N96076L')
leaf.spot <- leaf.spot[leaf.spot$NC_Accession %in% line_names,]


combo <- aggregate(list(Disease = leaf.spot$Disease), list(Line = leaf.spot$NC_Accession), mean)
combo

write.xlsx(combo, 'Leaf Spot.xlsx', row.names=FALSE)