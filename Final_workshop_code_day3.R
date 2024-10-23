rm(list = ls())

# List of required packages
packages <- c("sdm", "dismo", "tidyr", "mapview", "raster")

# Function to install and load packages
install_if_missing <- function(packages) {
  # Check for packages that are not installed
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install missing packages
  if (length(missing_packages)) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  invisible(lapply(packages, library, character.only = TRUE))
}

# Run the function to install and load the packages
install_if_missing(packages)

###Case study: Species Distribution Modelling of Golden mahseer (Tor Putitora)

installAll()
library(sdm)
library(dismo)
library(tidyr)
library(mapview)
library(raster)

##################### Getting the Species presence records 

sp<- read.csv("Golden_mahseer_thin1.csv")
spg <- as_tibble(sp) %>% dplyr::select(lon, lat)
# let's check the head of the table (data.frame) to make sure it is fine:
head(spg)
spg$species <- 1
spg <- spg %>% drop_na()
nrow(spg)
#------------
class(spg)
## Get rid of all columns except coordinate columns (lon & lat)
# from all columns, we only keep 2 columns (lon, lat)

coordinates(spg) <- c('lon','lat')

##################### Getting the Climate and hydrologial data 

bio <- list.files("./Current",full.names = T) 
bioc <-stack(bio)
bio
names(bioc)

plot(bioc[[1]])  

points(spg,col='red')

#Here, we test the collinearity issue among the 19 bioclim variables in the current time using the 
#`vifstep` function, and exclude collinear variables from all datasets (current and future times):

# Step 1: Extract values from RasterStack
ex <- raster::extract(bioc, spg)

# Step 2: Remove rows with NA values
ex_clean <- na.omit(ex)

# Step 3: Check the cleaned data
head(ex_clean)

# Step 4: Run vifstep() on the cleaned data
library(usdm)# contains the functions to deal with collinearity
v <- vifstep(ex_clean)

# Step 5: Check the VIF results
print(v)

#vifcor
v
bioc <- exclude(bioc, v)
#--------------------
library(sdm)

### (iii) Developing models using the sdm R package: 

#Now, we have both species (response) and climate (predictors) variables ready to 
#implement the modelling workflow using the sdm package. The package can be installed normally 
#from CRAN or from Github using: `devtools::install_github('babaknaimi/sdm'))`. 
#**To get all modelling methods available for training SDMs, some additional packages 
#also need to be installed. You may use the `installAll()` function one time after installing sdm to get all 
#required packages installed on your machine.**
  
#In the sdm package, two user-friendly functions should be used to develop the models.
#First, an sdmdata object should be created using the `sdmData` function, then, models 
#are trained and evaluated using the `sdm` function.

#The usage of the function:
## the first step in the sdm package is to create the data object:
# ---> 1000 background records will be generated using "gRandom":

d <- sdmData(species~., spg, predictors= bioc, bg = list(method='gRandom',n=1000))
# you may get a summary from the created object by executing the object:
d

getmethodNames()

## second step: to train and evaluate the models:

#----> 4 modelling methods are used: 
# =======>> glmp: polynomial generalised linear model; 
# =======>> brt: Boosted Regression Trees; 
# =======>> rf: Random Foress; 
# =======>> maxent: Maximum Entropy; 


#----> Replication method: sub-sampling (with 30 % of test data; repeats 3 times)


m <- sdm(species~., d, methods=c('glm','brt','rf','maxent'), replication=c('boot'),# sub-sampling, cross-validation, and bootstrapping
         test.p=30,n=3, parallelSetting=list(ncore=4,method='parallel'))

# to see a summary of the modelling outputs, you may check the object:
m

#m@models$species$rf$`13`@object

gui(m)

### (iv) Predict/Project the map of habitat suitability in current times
p1 <- predict(m, bioc)
p1
names(p1)
cl <- colorRampPalette(c('gray','orange','yellow','green','blue')) 

# let's visualise one of the outputs for each method:
plot(p1[[c(1,6,7,11)]],col=cl(200))

##### Ensemble:
## (i) Weighted-mean using values of AUC as weights
#en1 <- ensemble(m, p1, setting = list(method='weighted', stat= 'auc'))
## (ii) Weighted-mean using values of TSS as weights 
# opt=2 refers to the threshold obtained using max[Sensitivity + Specificity]

en1 <- ensemble(m, p1, setting=list(method='weighted',stat='tss',opt=2))

plot(en1)

##################Predict/Project the map of habitat suitability in Future times year 2050


biofut <- list.files("./Future_2050",full.names = T) 
biof <-stack(biofut)
biof
plot(biof[[1]])
names(biof)
names(biof) 
names(bioc)


##### Ensemble:
en2 <- ensemble(m, biof, setting=list(method='weighted',stat='tss',opt=2))
#--------------
plot(en2)
cl <- colorRampPalette(c('#3E49BB','#3498DB','yellow','orange','red','darkred'))
#----------
#### Here comes a glitch! terra-raster confusion!
#you need to convert your terra object (SpatRaster) to a raster

en1_raster <- raster(en1)
####Changes has to be made here then plot
plot(en1_raster, col=cl(200))
plot(en2, col=cl(200))

# Set up color palette
cl <- colorRampPalette(c('#3E49BB','#3498DB','yellow','orange','red','darkred'))

# Save the plot as a TIFF file with 300 DPI
tiff("comparison_plot.tif", width = 3500, height = 1500, res = 300)

# Set up a 1-row, 2-column plotting layout
par(mfrow = c(1, 2))
par(mar = c(5, 5, 5, 5))

# Plot en1 with the title "Current"
plot(en1_raster, col = cl(200), main = "Current")

# Plot en2 with the title "Future"
plot(en2, col = cl(200), main = "Future")

# Turn off the TIFF device to save the file
dev.off()


###Difference between current and Future 


library(mapview)
##if error comes install the package terra manually 
mapview(en1_raster, col.regions = cl(200), maxpixels = 3980338)



#-----------
ch <- en2 - en1_raster
cl2 <- colorRampPalette(c('red','orange','yellow','gray','green','blue'))
tiff("Difference.tif", width = 3000, height = 1500, res = 300)
plot(ch,col=cl2(200))
dev.off()


#----------------------Binarization 
df <- as.data.frame(d)
head(df)
df <- data.frame(species=df$species,coords(d))
xy <- as.matrix(df[,c('lon','lat')])
head(xy)
p <- raster::extract(en1_raster,xy)
head(p)
nrow(df)
length(p)
ev <- evaluates(df$species,p)
ev@statistics

th <- ev@threshold_based$threshold[2]

pa1 <- raster(en1_raster)

pa1[] <- ifelse(en1[] >= th, 1, 0)
plot(pa1)

pa2 <- raster(en2)

pa2[] <- ifelse(en2[] >= th, 1, 0)
plot(pa2)

# Save the plot as a TIFF file with 300 DPI
tiff("comparison_Binaray_plot.tif", width = 3500, height = 1500, res = 300)
# Set up a 1-row, 2-column plotting layout
par(mfrow = c(1, 2))
par(mar = c(5, 5, 5, 5))

# Plot en1 with the title "Current"
plot(pa1, main = "Current")

# Plot en2 with the title "Future"
plot(pa2, main = "Future")

# Turn off the TIFF device to save the file
dev.off()

chp <- pa2 - pa1
plot(chp,col=c('red','gray','blue'))

#---------------

rcurve(m,id=7:12)


#---------------
rcurve(m)
rcurve(m,id=7:12)

plot(getVarImp(m,method='rf'))

plot(getVarImp(m))

####End



