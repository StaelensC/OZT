# verwijder alle globale variabelen met rm(list = ls())

## Beschouw het dataframe mtcars dat standaard in R zit ##

## Geef gemiddelde van de variabele mpg (3 cijfers afgerond)
data("mtcars")
attach(mtcars)
round(mean(mpg), digits=3)

## Geef Q1, Q2, Q3
quantile(mpg) # Q1(25%) = 15.425 ; Q2 (50%) = 19,200 ; Q3 (75%) = 22,800

## Een steekproef van 113 studenten levert een gemiddelde hartslag van 80,08 slagen/minuut. 
## Men weet dat de standaard afwijking σ= 12,13 slagen/minuut.

## Bereken het 95% betrouwbaarheidsinterval voor de gemiddelde hartslag in de populatie van de studenten 
## en toon hoe je eraan komt. (rond je z-waarde af tot drie cijfers na de komma)

# a = 0.05, dus wegens symmetrie links en rechts 0.025
# in z-tabel is 0.025 gelijk aan 1.959 (zie tabel OF qnorm(.025))
# 133 studenten in steekproef, dus wegens centrale limietstelling -> pop gem = steekproefgem
# en steekproefSD binnen CTL -> sd/sqrt(n)
# steekproefgem - 1.959 * sd/sqrt(n) voor linker
# steekproefgem + 1.959 * sd/sqrt(n) voor rechter
# 80.08 - 1.959*12.13/sqrt(113)
# 80.08 + 1.959*12.13/sqrt(113)
80.08 + qnorm(0.025)*12.13/sqrt(113)
80.08 + qnorm(0.975)*12.13/sqrt(113)

## Bereken het 99% betrouwbaarheidsinterval voor de gemiddelde hartslag in de populatie van de studentenen 
## en toon hoe je eraan komt. (rond je z-waarde af tot drie cijfers na de komma)
80.08 + qnorm(0.005)*12.13/sqrt(113)
80.08 + qnorm(1- 0.005)*12.13/sqrt(113)

## Een uitvinder heeft een nieuwe, energie-efficiënte grasmaaimachine ontwikkeld. 
## Hij beweert dat de motorgedurende 5 uur (300 minuten) voortdurend op een enkele liter gewone benzine loopt. 
## Uit zijn voorraad van2000 motoren selecteert de uitvinder een aselecte steekproef van 50 motoren. 
## De motoren lopen gemiddeld295 minuten, met een standaardafwijking van 20 minuten. 
## Test de nulhypothese dat de gemiddelde looptijd 300 minuten is tegen de alternatieve hypothese 
## dat de gemiddelde looptijd geen 300 minuten bedraagt.Gebruik α= 0,05 en gebruik de methode met de p-waarde. 
## (Veronderstel dat de looptijden van motorennormaal zijn verdeeld en rond tussentijdse resultaten af 
## tot drie cijfers na de komma)

## Geef H0 en H1
# H0: Gemiddelde draaitijd machines = 300 minuten per l benzine
# H1: Gemiddelde draaitijd machines != 300 minuten per l benzine

## Voer de test uit
a <- 0.05 # s i g n i f i c a n t i e n i v e a u
m0 <- 300 # h y p o t h e t i s c h p o p u l a t i e g e m i d d e l d e
n <- 2000 # s t e e k p r o e f g r o o t t e
sm <- 295 # s t e e k p r o e f g e m i d d e l d e
sd <- 20 # s t a n d a a r d a f w i j k i n g
p <- 2*(1 - pnorm(sm, m0, sd / sqrt(n))) #tweezijdige test omdat we moeten testen op ongelijkheik, niet op greater or lower then
# indien de sample sm hoger was dan m0, dan was de formule 2*(1-pnorm(sm, m0, sd/sqrt(n)), lower.tail=FALSE)
## Wat concludeer je?
# Ik concludeer dat dat de nulhypothese niet kan worden verworpen op basis van de proef.
# De p-waarde is boven het significantieniveau en het gemiddelde van 295 minuten kan dus op toevallige basis.

## Open het bestandbreakingbad.csv. 
## We vinden hierin informatie rond de ratings ingegeven in IMDB voorde serie breaking bad (echte gegevens). 
## We zijn geïnteresseerd in de volgende variabelen
## UserRating Gemiddelde user rating per afleveringID
## Identificatie van een afleveringSeason.
## EpisodeSeizoen identifciatie gevolgd door de identificatie van het afleveringsnummer van dat seizoen
breaking_bad_dataset <- read.csv("D:/Onderzoekstechnieken/breakingbad.csv")
## Geef het meetniveau voorUserRating
# Ratio
## Geef het meetniveau voorSeason.episode
# Ordinaal
## We zijn geïnteresseerd of er verband bestaat tussen deID&UserRating
## Geef het wiskundig verband indien we een lineair verband wensen te onderzoeken (niet afronden)
# ik begrijp de vraag niet
## Geef de waarde van de metriek die de sterkte van dit verband geeft en interpreteer die.
# ik begrijp de vraag niet
## Geef de waarde van de maat die informatie geeft over de mate 
## waarin het opgestelde model de werkelijkedata benadert en interpreteer deze maat.
# ik begrijp de vraag niet
## Geef de gemiddelde scores voor seizoen 1 en seizoen 5. 
## (greplen reguliere expressies kunnen je hierbijhelpen - niet vergeten af te ronden)

seizoen_1 <- subset(breaking_bad_dataset, grepl("^(1.)", breaking_bad_dataset$Season.episode))
gem_s1 <- round(mean(seizoen_1$UserRating), digits = 3) #8.729
seizoen_5 <- subset(breaking_bad_dataset, grepl("^(5.)", breaking_bad_dataset$Season.episode))
gem_s5 <- round(mean(seizoen_5$UserRating), digits = 3) #9.381

## Beschouw de studie gevoerd met als resultaatkids.csv. 
## Er werd aan de kinderen gevraagd in verschillendeschooldistricten wat ze belangrijk vinden. 
## Ze gaven aan of dat goede punten, sportief zijn of populariteithet belangrijkste was voor hen. 
## Ze moesten ook volgende elementen rangschikken: 
## schoolresultaten, sport,uiterlijk en geld in de orde van belangrijkheid voor hen. 
## Er werd ook gevraagd naar hun geslacht, hun graaden andere informatie. Volgende variabelen worden gedefinieerd:
## Gender: Boy or girl
## Grade: 4, 5 or 6
## Age: Age in years
## Race: White, Other
## Urban/Rural: Rural, Suburban, or Urban school district
## School: Brentwood Elementary, Brentwood Middle, Ridge, Sand, Eureka, Brown, Main, Portage, WestdaleMiddle
## Goals: Student's choice in the personal goals question where options were 
## 1 = Make Good Grades, 
## 2 = BePopular, 
## 3 = Be Good in Sports
## Grades: Rank of "make good grades"(1=most important for popularity, 4=least important)
## Sports: Rank of "being good at sports"(1=most important forpopularity, 4=least important)
## Looks: Rank of "being handsome or pretty"(1=most important forpopularity, 4=least important)
## Money: Rank of "having lots of money"(1=most important for popularity, 4=least important)
kids_dataset <- read.csv("D:/Onderzoekstechnieken/kids.csv")
grade_goals <- subset(kids_dataset, kids_dataset$Goals=="Grades")
other_goals <- subset(kids_dataset, kids_dataset$Goals!="Grades")
## Geef de hypotheses indien we een hypothesetoets willen uitvoeren die het verband zoekt 
## tussen de variabelen goals & grade
# De hypothese is dat studenten waarvan het doel is goede punten te halen ook werkelijk betere punten halen
# H0: De gemiddelde punten van de studenten waarvan het doel is goede punten te halen 
# = de gemiddelde punten van de andere studenten
# H1: De gemiddelde punten van de studenten waarvan het doel is goede punten te halen
# > de gemiddelde punten van de andere studenten

## Voer de toets uit gebruik makende van de methode met kritieke grenswaarde evenals met de p-waarde(gebruikα= 0.01)
# Met kritieke grenswaarde:
a <- 0.01
zo <- qnorm(a) #ondergrens zScore -2.326
zb <- qnorm(1-a) # bovengrens zScore 2.326
m0 <- mean(kids_dataset$Grades)
sd <- sd(kids_dataset$Grades)
n <- nrow(kids_dataset)
go <- m0 + zo*sd/sqrt(n)
gb <- m0 + zb*sd/sqrt(n)
#formule u +- z*o/sqrt(n)
gem_grade_goals <- mean(grade_goals$Grades, na.rm = TRUE) #na.rm niet nodig, dient als voorbeeld
gem_other_goals <- mean(other_goals$Grades, na.rm = TRUE)
# Verrassing, de gemiddelde punten van degenen die punten belangrijk vonden is minder dan de rest
# Beide gemiddelden zijn trouwens buiten de grenswaarden wat er op wijst dat het geen toeval kan zijn
# test overschrijdingskans
p <- 1 - pnorm(gem_grade_goals, m0, sd / sqrt(nrow(grade_goals)), lower.tail = FALSE) #0.0005
p <- 1 - pnorm(gem_other_goals, m0, sd / sqrt(nrow(grade_goals))) #0.0002
# In beide gevallen is de p-waarde kleiner dan 0.05 en kunnen we met zekerheid uitsluiten dat het toeval was

## Geef de naam van de metriek en de metriek zelf om de sterkte van dit verband uit te drukken
# Ik begrijp de vraag niet

## Welke conclusie trek je hier uit?
# Er is een correlatie tussen het belangrijk vinden van goede punten en het behalen van slechte punten