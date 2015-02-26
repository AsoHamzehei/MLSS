## =============================================================================
## [RUGS Visualisation]: Step 1 - Data Preparation
## =============================================================================

## Author: Jo-fai Chow

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Import Raw Data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mlssnames <- read.csv("./data/MLSS_contacts.csv", stringsAsFactors=FALSE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create a new dataframe and update lat/lon information
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_mlssnames <- data.frame(matrix(NA, nrow = nrow(mlssnames), ncol = (ncol(mlssnames) + 2)))
colnames(df_mlssnames) <- c(colnames(mlssnames), "lat", "lon")
df_mlssnames[, 1:4] <- mlssnames[,]

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load cities databases from 'maps' package
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(maps)
data(world.cities)
australia.cities<-world.cities[which(world.cities[,2] %in% c("Australia", "Germany", "Switzerland")),]
data(us.cities)
#data(canada.cities)
df_cities <- rbind(australia.cities, us.cities)#, canada.cities)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Update Information (also using this step to cross check the original rugs table)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (n_row in 1:nrow(df_mlssnames)) {
  row_match <- which(df_cities$name == df_mlssnames[n_row,]$City)
  if (length(row_match) > 0) df_mlssnames[n_row, 5:6] <- df_cities[row_match, 4:5]
}

## Filter out the records that may need correct
#print(df_rugs[which(is.na(df_rugs[,7])),])
print(df_mlssnames[which(is.na(df_mlssnames[,6])),])


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Update the missing lat/lon information as well as cross-checking
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggmap)

tmp_geo <- geocode("Edinburgh")
df_mlssnames[df_mlssnames[,"City"]=="Edinburgh",c(5,6)]<-tmp_geo[c("lat", "lon")]
tmp_geo <- geocode("New York")
df_mlssnames[df_mlssnames[,"City"]=="New York",c(5,6)]<-tmp_geo[c("lat", "lon")]
tmp_geo <- geocode("Berkeley")
df_mlssnames[df_mlssnames[,"City"]=="Berkeley",c(5,6)]<-tmp_geo[c("lat", "lon")]

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Adjust the lat/lon of multiple RUGS in the same city
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Find duplicated cities and seperate them
name_dup <- df_mlssnames[which(duplicated(df_mlssnames$City)), ]$City
row_dup <- which(df_mlssnames$City %in% name_dup)
df_rugs_multi <- df_mlssnames[row_dup, ]
df_rugs_single <- df_mlssnames[-row_dup, ]

df_rugs_multi[,"lat"]<-jitter(df_rugs_multi[,"lat"], factor=0.01)
df_rugs_multi[,"lon"]<-jitter(df_rugs_multi[,"lon"], factor=0.01)


## Create the final df_rugs
df_rugs <- rbind(df_rugs_multi, df_rugs_single)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write the final table to CSV
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_rugs[, 5] <- round(df_rugs[, 5], 3)
df_rugs[, 6] <- round(df_rugs[, 6], 3)
write.csv(df_rugs, file ="./data/mlss_updated.csv", row.names=F)

## =============================================================================
## End of Script
## =============================================================================
