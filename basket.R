library(readr)
library(corrplot)
library(ggplot2)
library(lattice)
library(caret)
#install.packages("arules")
library(arules)
#install.packages("tweenr")
#install.packages("graphlayouts")
#install.packages("arulesViz")
library(arulesViz)
Trans2017 <- read.transactions("C:\\Users\\bling\\Desktop\\Jens Class\\ElectronidexTransactions2017.csv", format = "basket", sep = "," , rm.duplicates = TRUE )
Trans2017B <- read.transactions("C:\\Users\\bling\\Desktop\\Jens Class\\ElectronidexTransactions2017.csv", format = "basket", sep = "," , rm.duplicates = FALSE )
inspect(Trans2017)
length(Trans2017)
length(Trans2017B)
size(Trans2017)
size(Trans2017B)
LIST(Trans2017)
LIST(Trans2017B)
itemLabels(Trans2017)
itemLabels(Trans2017B)
summary(Trans2017)
summary(Trans2017B)
#most frequent items Imac, HP Laptop, Apple Earpods, Cyberpower Gamer Desktop, and Macbook Air
# number of items purchased range from 0 (two lines show zero need to review) to 30
# will use Trans2017B going forward - because need to know if bought more than one of something
#most transactions were purchases between 1-4 items (62% of all transactions) 6125/9835
itemFrequencyPlot(Trans2017B, topN=20, type='absolute')
itemFrequencyPlot(Trans2017B, topN=40, type='absolute')
ifs <- itemFrequency(Trans2017B, type='absolute')
sort(ifs, decreasing = FALSE)
barplot(sort(itemFrequency(Trans2017B), decreasing=FALSE))
#least sold items, Logitech Wireless Keyboard, VGA Monitor Cable, Panasonic on-ear Stereo Headphones, 1TB Portable hard drive
#Canon Ink, Logitech Stereo Headset, Etherrnet Cable, Canon Office Printer, Gaming Mouse Pro, Audio Cable
#logitch multumeddia speakers, 5TB Desktop Hard drive, Roku Express
sort(ifs, decreasing = TRUE)
image(Trans2017B [1:200, 1:20])
image(Trans2017B [1:200, 1:10])
image(sample(Trans2017B, 100))
rules <- apriori(Trans2017B, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE, type= 'absolute')
summary(rules)
#set of 1787 rules 
#items with 4 items have the most rules
#minimum support is .001 (parameter) max support is .005
#minimum confidence is .80 (parameter) max is 1
#min lift is 3.12 max is 10.069
#minimum count is 10 and max is 52
inspect(rules[1:20])
plot(rules[1:20], jitter = 0)
#top rule Brother Printer, Acrylic Monitor Stand = IMAC (count 11 lift 3.9  confidence 1 support .0011)
#2nd rule ASUS monitor, Mackie CR Speakers, Viewsonic Monitor = IMAC (count 10, lift 3.904 confidence 1 supp .001)
#3rd rule Apple MAgic Keyboard, Rii Led Gfaming Keyboard and mouse combo, Viewsonic Monitor = IMAC count 17, confidence 1, lift 3.9 support .0017)
rulestwo <- apriori(Trans2017B, parameter = list(supp=0.005, conf=0.7))
rulestwo <- sort(rulestwo, by='confidence', decreasing = TRUE)
summary(rulestwo)
inspect(rulestwo)
plot(rulestwo, jitter = 0)
#wanted to look at rules with the highest support that still has a confidence of 70% only three above .005 in support
#[1] {Acer Aspire, Dell Desktop, ViewSonic Monitor}          => {HP Laptop} count 52, supp .005 confidence .81 lift 4,18
#[2] {ASUS 2 Monitor, Dell Desktop, Lenovo Desktop Computer} => {iMac}     count 51 supp .005 confidence .73 lift 2.88
#[3] {ASUS 2 Monitor, ASUS Monitor}                          => {iMac}      count 50 supp .005 confidence .71 lift 2.78
#minlen - should there be a minimum number of items required in the rule? no - most of our transactions are between 1-4 items
#the top results from both sets of rules were 3 and 4 item baskets (one would mnot appear since that is all they bought)
supprules <- apriori(Trans2017B, parameter = list(supp=0.005, conf=0.1), minlen = 2)
supprules <- sort(supprules, by='supp', decreasing = TRUE)
supprulesplot <- (supprules[1:20])
plot(supprulesplot)
summary(supprules)
inspect(supprules[1:10])
#had to add minlen - top supporting rules. HP Laptop = Imac Support .075 confidence .389
#Imac = HP Latop - supp .0755 confidence .294
#lenovo desktop = IMAC supp .058 conf .396
#imac = Lenovo Desktop supp .0587 conf .22
#cyberpower gamer desktop = IMAC supp .056 conf .308
subsets <- which(colSums(is.subset(rules, rules)) > 1)
rules <- rules[-subsets]
inspect(rules[1:15])
#re-ran to remove subsets for rules did not change the top result - it eliminated the second 
#new number three is Asys Monitor, Microsoft office = IMAC supp .001 confidence 1 lift 3.9 and count 10
rules <- sort(rules, by='lift', decreasing = TRUE)
inspect(rules[1:15])
#sorted rules by lift (measures importance of rule)
#Apple Macbook Pro, HP Black and Tri Color Ink, HP Lap Top, IMAC = Acer Aspire - supp .001 confidence .833 count 10 lift 10
#2 Dell Desktop, IMAC, Lenovo Desktop, Mackie CR Speakers = Viewsonic Monitor - supp .001 confidence 1 lift 9 count 11 - seems to be the most balanced rule
#3 is number 2 with the addition of a HP Lap top - supp .001 confidence 1 lift 9 and count 10
is.redundant(rules)
inspect(rules[is.redundant(rules)])
inspect(rules[!is.redundant(rules)])
nonredrules <- rules[!is.redundant(rules)]
inspect(nonredrules[1:15])              
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
inspect(rules[1:15])
quality(nonredrules)$improvement <- interestMeasure(nonredrules, measure = "improvement")
inspect(nonredrules[1:3])
#1 Apple Macbook Pro, HP Black & Tricolor Ink, HP Laptop, IMAC = ACER Aspire support .001 confidence .83 lift 10 count 10
#2Dell Desktop, iMac, Lenovo Desktop Computer, Mackie CR Speakers} => {ViewSonic Monitor} supp .001 confidence 1 lift 9 count 11
#3Dell Desktop, Lenovo Desktop Computer, Mackie CR Speakers}       => {ViewSonic Monitor} supp .001 confidence .92 lift 8.36 count 12
top15 <- nonredrules[1:15]
plot(top15)
plot(top15, method = "graph")
plot(top15, method="graph", control=list(type="items")) 
plot(nonredrules)
plot(rulestwo)
plot(supprules)
plot(top15, method = "two-key plot")
plot(nonredrules, method =  "two-key plot")
plot(nonredrules, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)
plot(top15, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)
plot(nonredrules, method = "matrix", engine = "3d", measure = "lift")
plot(nonredrules, method = "matrix", measure = "lift")
save.image()


