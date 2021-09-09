# This code is modified from Reisz et al. 2020 - Early Jurassic dinosaur fetal dental development and its significance 
# for the evolution of sauropod dentition
#
# Written by T. Dudgeon in 2019/2020 - modified by T. Dudgeon in September 2021.
# 
# This code goes through how to set the working directory, load relevant packages, load 2D landmark data, and run a PCA

########### Tooth Outline Data 30 Landmarks ###############
# Set your working directory - this will be a custom piece of code to specify where the files are in your computer that
# you want to work with. Alternatively, you can click "Session" in the toolbar above, and click "Set Working Directory"
# to choose the working directory manually.
setwd("~/Fall 2019/Sauropodomorph Teeth/Crown outline")

# install the relevant packages. Alternatively, you can click "Tools" in the toolbar above, and click "Install Packages"
# and search the relevant packages and install them manually.
install.packages("rgl", "geomorph", "Morpho")

# Once packages are installed, you must load the package each time you start a new R session. You do not need to install
# them again in subsequent sessions.
library(rgl)
library(geomorph)
library(Morpho)

##################################################
######## Load the data, GPA, and PCA ############
# Your working directory should contain your points files (i.e., the landmarks you want to study). Each specimen should
# have it's own file. Here, I have saved mine as .txt files, but other file formats are compatible. This functional 
# loads all of the names of the files in your folder into a list
ptslist <- dir(pattern='.txt',recursive=T) 
# look at the object to verify the list was created correctly
ptslist
# Now we need to specify to R what data to load for each point file. The dimensions (dim) tells R what data to expect. 
# It follows the format: dim = c(number of landmarks and semilandmarks, number of dimensions (3D or 2D), number of specimens)
ptsarray <- array(dim = c(30,2,18)) 

# Now we need to actually load the data for each file and have them read into an array. The easiest way to do this is 
# in a forloop, which loads the data automatically for each file based on the information we specified above. 
for(i in 1:length(ptslist)){
  print(i)
  ptsarray[,,i]<-as.matrix(read.table(file=ptslist[i],header=F,sep="",row.names=1))
}

# If your data set has sliding landmarks (i.e., describing a curve without hard landmarks that specify homologous points),
# we now need to specify which landmarks are the sliders. The best way to do this is to manually specify which landmarks
# are sliders, and which landmarks they will slide between. Enter "?define.sliders" for a detailed breakdown on how to
# do this.
#
# This code specifies which array we are working with, how many sliders we are specifying, and whether we want a file
# created (we do!).
slide<-define.sliders(ptsarray, nsliders = 28, surfsliders = NULL, write.file = TRUE)

# Now save the file as a .csv to the working directory
write.csv(slide, "curveslide.csv")
# You may need to manually edit the .csv file in Excel, so take a look at it to make sure the format it right. It 
# should have three columns, specifying the start and end landmarks for each slider.

# Once the sliders are formatted, we can load it as an object!
cur <- read.csv(file="curveslide.csv")

# Running a Principal Components Analysis (PCA)
#
# First, the landmarks must be optimally alligned to ensure that we are only comparing shape, with size and position
# excluded. To do this, we must conduct a Generalized Procrustes Alignment.
# Specify the array of the point data and the object with your sliders.
Z.gpa <- gpagen(ptsarray, curves=cur)#GPA alignment
# Now look at the results
Z.gpa
# Plot the results
plot(Z.gpa)

# Plot the alligned coordinates in the PCA
pca <- gm.prcomp(Z.gpa$coords)
# Return the proportion of variance for each PC axis
summary(pca)
# Plot the 1st and 2nd axes
plot(pca, axis1 = 1, axis2 = 2, labels = T)

# Average the shapes
ref <- mshape(Z.gpa$coords)
#Visualize morphologies at extreme ends of the PC axes. Change the comp number to visualize difference axes.
plotRefToTarget(ref,pca$shapes$shapes.comp1$max,method="vector")
plotRefToTarget(ref,pca$shapes$shapes.comp1$min,method="vector")

