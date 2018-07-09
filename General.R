##General scripts ##

#Correlation matrix
cormat = cor(data)
write.xlsx(cormat, "Correlation_Matrix.xlsx")