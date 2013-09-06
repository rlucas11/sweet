## Clean SPSS File for Public File
## Variables removed:
## Personality items, SWLS, Affect, Specific taste items, Loneliness items,
## Bathing items from Bargh and Shalev replication, race/ethnicity (to prevent identification),
## Age, Year in college, Religion, English as first language

require("foreign")
sweet <- read.spss("TASTEFILE.sav", use.value.labels=FALSE)
sweet <- as.data.frame(sweet)
sweet <- sweet[,c("ipipe","ipipa","ipipc","ipipn","ipipo","bitter","salty","sour","spicey","sweet",
                  "filter_.","gender")]

write.csv(sweet, "sweet.csv", row.names=F)

## Analyses:
sweet <- read.csv("sweet.csv")
table(sweet$gender) # 1 = Female; 2 = Male
table(sweet$filter_.) # Quality check: 1 = pass; 0 = failed

## Replication Analyses:
cor(sweet[,c("ipipe","ipipa","ipipc","ipipn","ipipo")], sweet[,c("bitter","salty","sour","spicey","sweet")],
    use="pairwise.complete.obs")
cor.test(~ ipipa + sweet, data=sweet)


## Replication Analyses removing those who did not pass quality check:
cor(sweet[which(sweet$filter_.==1),c("ipipe","ipipa","ipipc","ipipn","ipipo")],
    sweet[which(sweet$filter_.==1),c("bitter","salty","sour","spicey","sweet")],
    use="pairwise.complete.obs")
cor.test(~ ipipa + sweet, data=sweet, subset=(filter_.==1))

## Controlling for gender
require("ppcor")
pcor(sweet[complete.cases(sweet[,c("ipipa","sweet","gender")]),c("ipipa","sweet","gender")])
genderModel <- lm(sweet ~ ipipa + gender, data=sweet)
summary(genderModel)
