# ST 558: Multivariate Analytics
# Module 8 R Activity Example Script File

###########
# Step 2  #
###########

# Set working directory 

setwd('/Users/scemerson/Documents/Old Computer Documents/ST 558 2017/Datasets')

# Install and load the 'ca' and 'psych' libraries

install.packages('ca')
install.packages('psych')

library(ca)
library(psych)

# Read in 'SportIceCreamData.csv' dataset 

sportIceCream <- read.csv('SportIceCreamData.csv', row.names=1)
sportIceCream

###########
# Step 3  #
###########

# Multidimensional Scaling

# isoMDS
# cmdscale

help(cmdscale)

# The 'cities' data is in the 'psych' package, and contains the 
# airline flight distances for 11 US cities.

data(cities)
cities

# Fit an MDS on the cities data
city.loc.mds0 <- cmdscale(cities, k=2) #ask for a 2 dimensional solution 
city.loc.mds0

# Note that when we plot these points, the map is upside-down and flipped
# east to west. 

plot(city.loc.mds0, pch=16)
text(city.loc.mds0, labels=row.names(cities), pos=1, cex=0.5)

# We can fix this by flipping each axis by multiplying by -1.

city.loc.mds <- -city.loc.mds0

plot(city.loc.mds, pch=16)
text(city.loc.mds, row.names(cities), pos=1, cex=0.5)


###########
# Step 4  #
###########

# Correspondence Analysis

help(ca)

sportIceCream.ca <- ca(sportIceCream)
sportIceCream.ca

names(sportIceCream.ca)

# Plot the resulting points from the correspondence analysis

plot(sportIceCream.ca)
plot(sportIceCream.ca, arrows=c(T,T))

