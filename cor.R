### TESTING FOR CORRELATION ###

library('ggplot2')# this won't work on my system:, lib = 'C:/Progra~1/R/R-3.2.0/library')
install.packages('KernSmooth') # I had to install some missing fortran compliers to get this package installed...
install.packages('corrgram') # 
library('corrgram') #, lib = 'C:/Progra~1/R/R-3.2.0/library') 

data(faithful)
head(faithful)
cor(faithful$eruptions, faithful$waiting,
    method = c('pearson', 'kendall', 'spearman')) 
#pearson default method - linear assocation btwn two cont variables
# if both variables are normally distributed, pearsons tests for an association of any kind, not just linear

cor(faithful$eruptions, faithful$waiting,
    method = c('spearman'))
cor(faithful$eruptions, faithful$waiting,
    method = c('kendall'))

#are variables normally distributed????
ggplot(faithful, aes(x = eruptions, y= waiting)) + geom_point()
# i wonder if there is any overlap:
ggplot(faithful, aes(x = eruptions, y= waiting)) + geom_point(alpha = 0.25, size = 3)
ggplot(faithful, aes(x = eruptions, y= waiting)) + geom_jitter(alpha = 0.25, size = 3)

ggplot(faithful, aes(x = eruptions, y= waiting)) + geom_jitter(alpha = 0.25, size = 3) + geom_smooth()

m1.lm <- lm(formula = waiting ~ eruptions, data = faithful)
par(mfcol = c(2,2))
plot(m1.lm)

# faithful

faithful$eruptions.2 <- faithful$eruptions^2

m2.lm <- lm(formula = waiting ~ eruptions + eruptions.2, data = faithful)

plot(m2.lm)

pred.df <- data.frame(eruptions = seq(from = min(faithful$eruptions), to = max(faithful$eruptions), length.out = 500))
pred.df$eruptions.2 <- pred.df$eruptions^2

M1.Pred.df <- data.frame(pred.df, waiting = predict(object = m1.lm, newdata = pred.df))

M2.Pred.df <- data.frame(pred.df, waiting = predict(object = m2.lm, newdata = pred.df))

M1.Pred.df$Model = rep('M1', nrow(M1.Pred.df))

M2.Pred.df$Model = rep('M2', nrow(M2.Pred.df))

Both.Pred.df <- rbind(M1.Pred.df, M2.Pred.df)

summary(Both.Pred.df)

Both.Pred.df$Model <- factor(Both.Pred.df$Model)

p0 <- ggplot(faithful, aes(x = eruptions, y = waiting)) + geom_point(alpha = 0.25, size = 3)

p0 + geom_line(aes(colour = Model), data = Both.Pred.df)

ggplot(faithful, aes(x= eruptions)) + geom_histogram(aes(y=..density..), colour = 'black', fill = 'grey')
ggplot(faithful, aes(x= eruptions)) + geom_freqpoly(aes(y=..density..)) + geom_density(linetype = 2)
ggplot(faithful, aes(x= waiting)) + geom_histogram(aes(y=..density..), colour = 'black', fill = 'grey')

p1 <- ggplot(aes(x = eruptions), data = faithful)
p1 +
    geom_histogram(aes(y=..density..), colour = 'black', fill = 'grey') +
    geom_freqpoly(aes(y=..density..), colour = 'red') +
    geom_density(linetype = 2, colour = 'blue')

# not normal at all - yes it's bimodal perhaps a mixture of two normals would capture it...

#obtain p values
cor.test(faithful$eruptions, faithful$waiting,
    method = c('spearman'))
# Warning message:
# In cor.test.default(faithful$eruptions, faithful$waiting, method = c("spearman")) :
#  Cannot compute exact p-value with ties

cor.test(faithful$eruptions, faithful$waiting,
         method = c('pearson'))

#create a corrgram
corrgram(faithful, order=T, 
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         cor.method = 'pearson')
corrgram(faithful, order=T, 
         lower.panel = panel.shade,
         upper.panel = panel.pts,
         text.panel = panel.txt,
         cor.method = 'spearman')




plot(faithful$eruptions, faithful$waiting)
