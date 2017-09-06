library(lattice)

fiori0 <- read.csv2("fioriture.csv", sep="\t",dec = ".")
fiori <- fiori0
fiori$data[1:8]
as.character(fiori$data)[1:8]
fiori$data <- as.Date(as.character(fiori$data), format="%m/%d/%Y")
fiori$data[1:8]
str(fiori)
sum(sort(table(fiori$specie)<500))
sum(sort(table(fiori$specie)<500))

mesi <- c("January", "February","March", "April", "May", "June","July", "August", "September","October","November", "December")
fiori$mesi_n <- as.numeric(factor(months(fiori$data), levels=mesi)
fiori$mesi <- factor(months(fiori$data), levels=mesi)


dim(table(fiori$sito..sottosito., fiori$specie))

#prendiamone uno
erbetta <- fiori[fiori$specie=="Dactylis glomerata",]
# consideriamo solo quelle del 2012
capodanno12 <-  as.Date("31/12/12", format="%d/%m/%y")
erbetta <- erbetta[erbetta$data<=capodanno12,]
for(i in 1:length(table(erbetta$sito..sottosito.)))
{
  if(table(erbetta$sito..sottosito.)[i]!=0){
  sito <- names((table(erbetta$sito..sottosito.)))[i]
  y <- erbetta$avanzamento.fenologico[erbetta$sito..sottosito.==sito]
  x <- erbetta$data[erbetta$sito..sottosito.==sito]
  y.s <- y*0.99+0.001
  y.logit <- log(y.s/(1-y.s))
  m1 <- lm(y.logit ~ as.numeric(x))
  X <- model.matrix(m1)
  x.new <- seq(min(X[,2]), max(X[,2]))
  y.hat <- predict(m1, newdata=data.frame(x=x.new))
  y.hat <- exp(y.hat)/(1+exp(y.hat))
  plot(x, y,
       xlab="Period", ylab="avanzamento fenologico", main=paste("Sito ",sito), xlim=c(min(erbetta$data),max(erbetta$data) ))
  lines(x.new, y.hat)
  }
}


# points on a map

library(ggplot2)
library(ggmap)

# creating a sample data.frame with your lat/lon points
lat <- as.double(substr(as.character(fiori$coordinate.N),1,2)) + as.double(substr(as.character(fiori$coordinate.N),4,5))/60 + as.double(substr(as.character(fiori$coordinate.N),6,9))/3600
lon <- as.double(substr(as.character(fiori$coordinate.E),1,2)) + as.double(substr(as.character(fiori$coordinate.E),4,5))/60 + as.double(substr(as.character(fiori$coordinate.E),6,9))/3600
df <- as.data.frame(cbind(lon,lat))

# getting the map
map_extended <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 10,
                      maptype = "terrain", scale = 2)

# plotting the map with the locations 
ggmap(map_extended) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# getting the map (piu' zoom solo dentro al GRA)
map_roma <- get_map(location = "rome", zoom = 11,
                      maptype = "terrain", scale = 2)

# plotting the map with the locations 
ggmap(map_roma) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)



##### temperature

min2 <- fiori[,26:37]
min3 <- fiori[,38:49]

max2 <- fiori[,50:61]
max3 <- fiori[,62:73]

P2 <- fiori[,74:85]
P3 <- fiori[,86:97]

TM2 <- fiori[,106:117]
TM3 <- fiori[,118:129]


X <- model.matrix(rep(1,nrow(fiori)) ~ fiori$mesi)
X[,1] <- 0
head(X)

temp <- data.frame(rep(NA,nrow(fiori)))
temp$max2 <- apply(X*max2,1,sum)
temp$max3 <- apply(X*max3,1,sum)
temp$min2 <- apply(X*min2,1,sum)
temp$min3 <- apply(X*min3,1,sum)
temp$P2 <- apply(X*P2,1,sum)
temp$P3 <- apply(X*P3,1,sum)
temp$TM2 <- apply(X*TM2,1,sum)
temp$TM3 <- apply(X*TM3,1,sum)
str(temp)


#su quante specie ci concentriamo?
quantisiti <- function(x) length(x)-sum(x==0)
quanteXspecie <- sort(apply(table(fiori$specie, fiori$sito..sottosito.),1,quantisiti), decreasing =TRUE)
plot(quanteXspecie)

sum(quanteXspecie>5)

months(fiori$data)
