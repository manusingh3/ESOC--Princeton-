######################FOGHORN Clean dataset####################################
#Read the files

library(foreign)
file = file.choose()
data = read.spss(file, use.value.labels = TRUE, to.data.frame = TRUE,
          trim_values = TRUE)

file_b = file.choose()
datab = read.spss(file_b, use.value.labels = TRUE, to.data.frame = TRUE,
                   trim_values = TRUE)
# merge the two versions client A and Client B together based on Id Number and wave. Please note that dataset B 
#only has information on waves 1-9. Thus the resulting dataset will have complete information ~1000 columns for waves 1-9
# and missing values ~485 columns for the waves 10-20.

total <- merge(data,datab,by=c("Idno","Wave"))
# Needs to be converted to character to change the naming convention
total$Province.x = as.character(total$Province.x)


#csv file written for easy importing

write.csv(total, file = "foghorn_merged.csv")


#####standardizing province and district names
unique(total$Province.x)
###the  comparison is made manually and the discrepencies are tracked as follows 
# the name Sari-pul does not match 

head(total[total$Province.x == "Sar-e Pul",1:15])
# replacing with standardized spelling 

total$Province.x[total$Province.x == "Sar-e Pul" ] = "Sari Pul"
total$Province.x[total$Province.x == "Herat" ] = "Hirat"
total$Province.x[total$Province.x == "Helmand" ] = "Hilmand"
total$Province.x[total$Province.x == "Wardak" ] = "Maydan Wardak"
total$Province.x[total$Province.x == "Panjshayr" ] = "Panjsher"

######Renaming colnames#######

names = colnames(total)
#as the colnames have a suffix of .x or .y depending on the dataset they came from, 
#the sapply function takes each names splits it where the '.' character appears and takes the first part of the string 
#for every name.

new_name = sapply(strsplit(names, split='.', fixed=TRUE), function(x) (x[1]))

# new names are assigned to the dataset

colnames(total) = new_name

# the final dataset is stored

write.csv(total, file = "foghorn_merged.csv")

