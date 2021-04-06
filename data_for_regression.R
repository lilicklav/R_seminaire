library(dplyr)
library(ggplot2)
library(readxl)

# read files
data_covid <- read.csv("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", 
                       sep = ";", header = TRUE)

estim_pop_dep_sexe_gca_1975_2021 <- read_excel("C:/Users/alave/Downloads/estim-pop-dep-sexe-gca-1975-2021.xls", sheet = "2021")

data_temp <- read.csv("https://www.data.gouv.fr/fr/datasets/r/dd0df06a-85f2-4621-8b8b-5a3fe195bcd7", sep = ";", header = TRUE)

data_pauvre <- read_excel("C:/Users/alave/Downloads/taux de pauvrete.xlsx", sheet = "DEP")

data_fisc <- read_excel("C:/Users/alave/Downloads/Revenus Fiscaux.xlsx", sheet = "DEP")

data_csp <- read_excel("C:/Users/alave/Downloads/csp.xlsx", sheet = "DEP")

data_obese <- read_excel("C:/Users/alave/Downloads/Obésité.xlsx", sheet = "DEP")



# number of peoples estimations -- data manipulation for join
colnames(estim_pop_dep_sexe_gca_1975_2021)[1] <- 'dep'
data_population <- subset(estim_pop_dep_sexe_gca_1975_2021, 
                          select = c(dep, ...8, ...3, ...4, ...5, ...6, ...7))
data_population <-data.frame(data_population)
data_population <- data_population[c(5:nrow(data_population)),]
colnames(data_population)[2] <- 'nbr_pop'
data_population$...3 <- type.convert(data_population$...3, dec = ".")
data_population$...4 <- type.convert(data_population$...4, dec = ".")
data_population$...5 <- type.convert(data_population$...5, dec = ".")
data_population$...6 <- type.convert(data_population$...6, dec = ".")
data_population$...7 <- type.convert(data_population$...7, dec = ".")
data_population$nbr_pop <- type.convert(data_population$nbr_pop, dec = ".")
data_population["pourcentage_10_19ans"] <- (data_population$...3/data_population$nbr_pop)*100
data_population["pourcentage_20_39ans"] <- (data_population$...4/data_population$nbr_pop)*100
data_population["pourcentage_40_59ans"] <- (data_population$...5/data_population$nbr_pop)*100
data_population["pourcentage_60_74ans"] <- (data_population$...6/data_population$nbr_pop)*100
data_population["pourcentage_75_etplus"] <- (data_population$...7/data_population$nbr_pop)*100
data_population <- subset(data_population, select = -c(...3, ...4, ...5, ...6, ...7 ))

                                        

# temp data manipulation 
data_temp <- subset(data_temp, select = c(date_obs, code_insee_departement, tmoy))

# pauvre data manipulation
colnames(data_pauvre)[1] <- 'dep'
data_pauvre <- data_pauvre[c(4:nrow(data_pauvre)),]
data_pauvre <- subset(data_pauvre, select = c(dep, ...3, ...4, ...5, ...6, ...7, ...8, ...9))
data_pauvre$...3 <- type.convert(data_pauvre$...3, dec = ".")
data_pauvre$...4 <- type.convert(data_pauvre$...4, dec = ".")
data_pauvre$...5 <- type.convert(data_pauvre$...5, dec = ".")
data_pauvre$...6 <- type.convert(data_pauvre$...6, dec = ".")
data_pauvre$...7 <- type.convert(data_pauvre$...7, dec = ".")
data_pauvre$...8 <- type.convert(data_pauvre$...8, dec = ".")
data_pauvre$...9 <- type.convert(data_pauvre$...9, dec = ".")
colnames(data_pauvre)[2] <- 'taux_pauvrete_moy'
colnames(data_pauvre)[3] <- 'taux_pauvrete_30etmoins'
colnames(data_pauvre)[4] <- 'taux_pauvrete_30_39ans'
colnames(data_pauvre)[5] <- 'taux_pauvrete_40_49ans'
colnames(data_pauvre)[6] <- 'taux_pauvrete_50_59ans'
colnames(data_pauvre)[7] <- 'taux_pauvrete_60_74ans'
colnames(data_pauvre)[8] <- 'taux_pauvrete_75etplus'

# fisc data manipulation
colnames(data_fisc)[1] <- 'dep'
colnames(data_fisc)[5] <- 'med_revenu'
colnames(data_fisc)[4] <- 'pourcentage_menage_imposes'
data_fisc <- data_fisc[c(2:nrow(data_fisc)),]
data_fisc <- subset(data_fisc, select = c("dep", "med_revenu", "pourcentage_menage_imposes"))
data_fisc$med_revenu <- type.convert(data_fisc$med_revenu, dec = ".")
data_fisc$pourcentage_menage_imposes <- type.convert(data_fisc$pourcentage_menage_imposes, dec = ".")

# csp data manipulation
data_csp <- data_csp[c(3:nrow(data_csp)),]
names(data_csp) <- as.matrix(data_csp[1, ])
data_csp <- data_csp[-1, ]
data_csp[] <- lapply(data_csp, function(x) type.convert(as.character(x)))
colnames(data_csp)[1] <- 'dep'
colnames(data_csp)[2] <- 'dep_name'
data_csp <- subset(data_csp, select = -c(dep_name))
data_csp$`Part des agriculteurs exploitants` <- type.convert(data_csp$`Part des agriculteurs exploitants`, dec = ".")
data_csp$`Part des artisans, comm., chefs d'entr.` <- type.convert(data_csp$`Part des artisans, comm., chefs d'entr.`, dec = ".")
data_csp$`Part des cadres, professions intellect. supérieures` <- type.convert(data_csp$`Part des cadres, professions intellect. supérieures`, dec = ".")
data_csp$`Part des professions interméd.` <- type.convert(data_csp$`Part des professions interméd.`, dec = ".")
data_csp$`Part des employés` <- type.convert(data_csp$`Part des employés`, dec = ".")
data_csp$`Part des ouvriers` <- type.convert(data_csp$`Part des ouvriers`, dec = ".")
data_csp$Autres <- type.convert(data_csp$Autres, dec = ".")

# obese data manipulation
data_obese <- data_obese[c(4:nrow(data_obese)),]
colnames(data_obese)[1] <- 'dep'
colnames(data_obese)[3] <- 'frequence_obesite'
data_obese <- subset(data_obese, select = c("dep", "frequence_obesite"))
data_obese$frequence_obesite <- type.convert(data_obese$frequence_obesite, dec = ".")

# join data_covid and data_population on number dep
data_covid <- subset(data_covid, sexe == 0)
data <-left_join(data_covid,data_population, by = "dep")
data["hosp_pop"] <- (data$hosp / as.integer(data$nbr_pop))*100000
data["rea_pop"] <- (data$rea / as.integer(data$nbr_pop)) * 100000

################################################################################################ CREATE DATAFRAME FOR REGRESSION
# INFO: PIC COVID DAY -- HOSP & REA,  NB POPULATION, TEMP APRIL, TAUX PAUVRETE, REVENU_MED

data_for_regression <- subset(data, 
                              select = c(dep, jour, hosp_pop, rea_pop, nbr_pop, pourcentage_10_19ans, pourcentage_20_39ans,
                                         pourcentage_40_59ans, pourcentage_60_74ans, pourcentage_75_etplus))
#  SELECT THE DAY                                         
data_for_regression <- subset(data_for_regression, data_for_regression$jour == "2020-04-15")

# SELECT THE TEMPERATURE OF THE MONTH BEFORE  AND THE MONTH OF THE SELECTED DAY

data_temp_for_regression <- data.frame(data_temp$code_insee_departement, data_temp$date_obs,data_temp$tmoy)
data_temp_for_regression$data_temp.tmoy <- type.convert(data_temp_for_regression$data_temp.tmoy, dec = ".")

colnames(data_temp_for_regression)[1] <- 'dep'
colnames(data_temp_for_regression)[2] <- 'jour'
colnames(data_temp_for_regression)[3] <- 'tmoy'
data_temp_for_regression$jour <- as.Date(data_temp_for_regression$jour)
data_temp_for_regression <- data_temp_for_regression[!is.na(data_temp_for_regression$tmoy),] 
data_temp_for_regression["month"] <- format(data_temp_for_regression$jour, '%B-%Y')
data_temp_for_regression <- data_temp_for_regression[, c("dep", "tmoy", "month")]
data_temp_for_regression <- subset(data_temp_for_regression, 
                                   data_temp_for_regression$month == "mars-2020" | data_temp_for_regression$month == "avril-2020")

# PASS THESE MONTH BY COLUMNS
data_temp_for_regression<- reshape(aggregate(.~dep+month,data_temp_for_regression, mean), 
                                   direction = "wide", idvar = "dep", timevar = "month")

# JOIN WTH OTHER INFORMATIONS
data_for_regression <- left_join(data_for_regression, data_pauvre, by = "dep")
data_for_regression <- left_join(data_for_regression, data_fisc, by = "dep")
data_for_regression <- left_join(data_for_regression, data_obese, by = "dep")
data_for_regression <- left_join(data_for_regression, data_csp, by = "dep")
data_for_regression <- left_join(data_for_regression, data_temp_for_regression, by = "dep")

#Retrieve dep & jour columns
data_for_regression <- subset(data_for_regression, select = -c(dep, jour))


data_for_regression <- data_for_regression[!is.na(data_for_regression$`tmoy.avril-2020`),]

##############################################################################################################################################

##########################################################   REGRESSION LINEAIRE   ###########################################################

##############################################################################################################################################

modele <- lm(data_for_regression$hosp_pop ~., data = data_for_regression)
sm <- summary(modele)
sm
library(MASS)
step <- stepAIC(modele, direction = "both", trace = FALSE)
step


###############################################################################################################################################

###########################################################  RIDGE REGRESSION   ##################################################################

###############################################################################################################################################


library(glmnet)

x = as.matrix(subset(data_for_regression, select = -c(hosp_pop)))
y_train = data_for_regression$hosp_pop

# Cherche optimal lambdas
lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

ridge_reg = glmnet(x, y_train, alpha = 0, family = 'gaussian', lambda = optimal_lambda)

ridge_reg$beta

# SELECT THE BEST MODEL --- SELECT BEST VARIABLES
step_ridge <- stepAIC(ridge_reg, direction = "both", trace = FALSE)

Data_central <- scale(data_for_regression)
