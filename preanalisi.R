# fioriture import and data manipulation


fioriture <- readxl::read_xls("Fioriture_2.xls", sheet = "all")
fioriture
fioriture[,6:8] <- (fioriture[,6:8]-1)/5
fioriture
fior <- reshape(fioriture, varying = 3:8, sep="0", direction = 'long')
names(fior)[5] <- "year"
fior$year <- fior$year +2000
fior$id
fioriture <- fior[,-8]
rm(fior)
fioriture$specie <- factor(fioriture$specie)
fioriture$location <- factor(fioriture$location)
fioriture <- fioriture[which(!is.na(fioriture$y)),]
fioriture
#basic plots
library(ggplot2)
gg <- ggplot(fioriture)

# by location
gg + geom_smooth(aes(x=t, y=y, col=specie, lty=location)) + geom_point(aes(x=t, y=y)) + facet_wrap(~as.factor(specie))

# by year
gg + geom_smooth(aes(x=t, y=y, col=factor(year), lty=factor(year))) + geom_point(aes(x=t, y=y)) + facet_wrap(~as.factor(specie))


# define the basis 
# le osservazioni stanno in circa 200 giorni con step medio di circa 15 giorni
# mettiamo quindi 15 nodi tra
min(fioriture$t)
max(fioriture$t)
knots <- seq(15, 365, by=30)
bounds <- c(0, 365)

# per le basi scegliamo le integrated splines
library(splines2)
basi <- cbind(1, iSpline(seq(0,365, length=100), knots = knots, 
                Boundary.knots=bounds))
# appiccichiamole al dataframe
fioriture$isplines <- cbind(1, iSpline(fioriture$t, knots = knots, 
                              Boundary.knots=bounds))

fioriture$y[fioriture$y==1] <- 1-0.005
fioriture$y[fioriture$y==0] <- 0.005
y01 <- fioriture$y
y <- log(y01/(1-y01))
plot(y, logity)
x <- cbind(fioriture$temp, fioriture$temp^2) # da aggingere altro tipo altitudine
B <- fioriture$isplines
t <- fioriture$t
si <- as.numeric(fioriture$specie)
li <- as.numeric(fioriture$location)


