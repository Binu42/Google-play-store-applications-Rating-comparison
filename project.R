# Reading file stored
data <- read.csv(file.choose(), header = T)
attach(data)

# Finding probability of applic!ations Ratings
probabilityofstar5 <- Star5/Ratings
probabilityofstar4 <- Star4/Ratings
probabilityofstar3 <- Star3/Ratings
probabilityofstar2 <- Star2/Ratings
probabilityofstar1 <- Star1/Ratings
probability_of_rating <- Ratings/Downloads

# Combining the probability into data readed columnwise
data <- cbind(data, probability_of_rating, probabilityofstar5, probabilityofstar4, probabilityofstar3,
            probabilityofstar2, probabilityofstar1)
write.csv(data, file = "probability.csv")

# plotting graph for Different stars got :- (5, 4, 3, 2, 1)
a = barplot(probabilityofstar5, col = 'forestgreen', las = 1, axes = T, ylim = c(0, 1), main = 'Probability of Rating Applications 5 star'
            , col.main = 'brown', col.axis = 'red3', xlab = 'Applications', col.lab = 'blue', ylab = 'Probability')
text(a, srt = 60, adj = 1, xpd = T, labels = Name, cex = .75, par("usr")[3], font = 2, col = 'red2')

a = barplot(probabilityofstar4, col = 'chartreuse', las = 1, axes = T, ylim = c(0, .3), main = 'Probability of Rating Applications 4 star'
            , col.main = 'brown', col.axis = 'red3', xlab = 'Applications', col.lab = 'blue', ylab = 'Probability')
text(a, srt = 60, adj = 1, xpd = T, labels = Name, cex = .75, par("usr")[3], font = 2, col='red2')

a = barplot(probabilityofstar3, col = 'yellow', las = 1, axes = T, ylim = c(0, .1), main = 'Probability of Rating Applications 3 star'
            , col.main = 'brown', col.axis = 'red3', xlab = 'Applications', col.lab = 'blue', ylab = 'Probability')
text(a, srt = 60, adj = 1, xpd = T, labels = Name, cex = .75, par("usr")[3], font = 2, col = 'red2')




a = barplot(probabilityofstar2, col = 'tomato', las = 1, axes = T, ylim = c(0, .04), main = 'Probability of Rating Applications 2 star'
            , col.main = 'grey26', col.axis = 'red3', xlab = 'Applications', col.lab = 'blue', ylab = 'Probability')
text(a, srt = 60, adj = 1, xpd = T, labels = Name, cex = .75, par("usr")[3], font = 2, col = 'navyblue')

a = barplot(probabilityofstar1, col = 'red1', axes = T, las = 1, ylim = c(0, .15), main = 'Probability of Rating Applications 1 star'
            , col.main = 'brown', col.axis = 'darkblue', xlab = 'Applications', col.lab = 'blue', ylab = 'Probability')
text(a, srt = 60, adj = 1, xpd = T, labels = Name, cex = .75, par("usr")[3], font = 2, col = 'navyblue')

# Plotting graph for probability that user is rating application
a1 = barplot(probability_of_rating, col = 'deepskyblue', las = 1, ylim = c(0, .14), main = 'Probability of Rating Applications'
             , col.main = 'brown', col.axis = 'red3', xlab = 'Applications', col.lab = 'blue', ylab = 'Probability')
text(a1, srt = 60, adj = 1, xpd = T, labels = Name, cex = .76, par("usr")[3], font = 2, col = 'red2')

# Average rating for different applications
rating <- cbind(Star5*5, Star4*4, Star3*3, Star2*2, Star1*1)
avgrating <- rbind(apply(rating, 1, sum)/Ratings)

plt <- barplot(avgrating, ylim = c(0, 5), main = 'Average Rating of Applications', col.main = 'brown'
               , axes = F, col = 'orangered', xlab = 'Applications', ylab = 'Probability', col.lab = 'brown3')
axis(side = 2, at = c(0, 1, 2, 3, 4, 4.2, 4.4, 4.6, 4.8, 5), las = 1, col.axis = 'darkblue')
text(plt, srt = 60, adj = 1.001, xpd = T, labels = Name, par('usr')[3], cex = .75, font = 2, col.lab = 'navyblue')

# combining probability of ratings and ploting stacked plot
data3 <- rbind(probabilityofstar5, probabilityofstar4, probabilityofstar3, probabilityofstar2, probabilityofstar1)
a = barplot(as.matrix(data3), col = c('darkgreen', 'chartreuse', 'yellow', 'tomato', 'red1'), axes = F
            , main = "Probability For Different Ratings of Applications", col.main = 'brown', ylim = c(0, 1.28), xlab = 'Applications',
            ylab = 'Probability', col.lab = 'darkblue')
axis(side = 2, at = c(seq(0, 1, .05)), col.axis = 'red3', las = 1)
text(a, srt = 60, adj = 1, xpd = T, labels = Name, par('usr')[3.5], col = 'red2', font = 2, cex = 0.75)
legend('top', fill = c('forestgreen', 'chartreuse', 'yellow', 'tomato', 'red1')
       , legend = c('5 star', '4 star','3 star', '2 star','1 star'), adj = 0, horiz = T, bty = 'n', text.col = 'red3', text.font = 2)

# Calculating the mean
intial <- cbind(probabilityofstar5*5, probabilityofstar4*4, probabilityofstar3*3, probabilityofstar2*2, probabilityofstar1*1)
mean_cal <- cbind(apply(intial, 1, sum))

# Calculating the variance
square <- cbind((5-mean_cal)^2, (4-mean_cal)^2, (3-mean_cal)^2, (2-mean_cal)^2, (1-mean_cal)^2)
squarefin <- cbind(square[, 1]*probabilityofstar5, square[, 2]*probabilityofstar4, square[, 3]*probabilityofstar3
              , square[, 4]*probabilityofstar2, square[, 5]*probabilityofstar1)
variance <- cbind(apply(squarefin, 1, sum))

# Calculating Standard deviation
standdev <- cbind(apply(variance, 2, sqrt))

# creating data frame of application name, mean, variance and Standard deviation
datafin <- data.frame(Name, mean_cal, variance, standdev)
names(datafin) <- c("Applications Name" , "      Mean", "      Variance", "Standard Deviation")
datafin

# storing datfin in file
write.csv(datafin, file = "meanvariance.csv")

detach(data)

