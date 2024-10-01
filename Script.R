### ECONOMETRICS
### GROUP PROJECT: SOLOW-SWAN MODEL
### EDOARDO PUTIGNANO(3195342), TOMMASO RAVASIO(3192281), LEONARDO TONELLI(3216378), ALESSANDRO CAMPI(3195247)


## IMPORT THE LIBRARIES
library(lmtest) # to run ols assumptions tests
library(data.table) # to format dataframes
library(nlme) #The “nlme” package is used for fitting and analyzing linear and nonlinear mixed-effects models. Among  these models there is the FGLS.
library("reshape2") #for data cleaning
library(readr) #library necessary to read csv files 
library(readxl) #library necessary to read excel files 










### DATA CLEANING ###
#IMPORT DATASET
GDP_per_capita <- read_xls("GDP_per_capita.xls", skip=3)
POP_GROW <- read_xls("POP_GROWTH.xls",skip=3)
Saving_rate <- read_xls("Saving_rate.xls",skip=3)
Research_and_development_expenditure <- read_xls("Research_and_development_expenditure.xls", skip=3)
population <- read_xls("popolazione.xls", skip = 3)

# CONVERT DATAFRAMES FROM WIDE TO LONG FORMAT
melted_pop = melt(POP_GROW, id.vars = 1:4)
melted_saving = melt(Saving_rate, id.vars = 1:4)
melted_gdppc = melt(GDP_per_capita, id.vars = 1:4)
melted_res = melt(Research_and_development_expenditure, id.vars = 1:4)

# CHANGE THE NAME OF OUR VALUES' COLUMNS
colnames(melted_pop)[colnames(melted_pop) == "value"] <- "pop_growth"
colnames(melted_saving)[colnames(melted_saving) == "value"] <- "sav_rate"
colnames(melted_gdppc)[colnames(melted_gdppc) == "value"] <- "gdp_pc"
colnames(melted_res)[colnames(melted_res) == "value"] <- "rnd_exp"

# DELETE USELESS COLUMNS
columns_to_delete <- c("Country Code", "Indicator Name", "Indicator Code")
clean_pop <- melted_pop[, !names(melted_pop) %in% columns_to_delete]
clean_sav <- melted_saving[, !names(melted_saving) %in% columns_to_delete]
clean_gdppc <- melted_gdppc[, !names(melted_gdppc) %in% columns_to_delete]
clean_res <- melted_res[, !names(melted_res) %in% columns_to_delete]

# MERGE THE DATAFRAMES IN ONE 
merged = merge(merge(merge(clean_pop, clean_sav, by = 1:2), clean_gdppc, by = 1:2), clean_res, by = 1:2)

# CHANGE NAME OF DATE'S COLUMN
colnames(merged)[colnames(merged) == "variable"] <- "Year"

# DROP GENERIC LISTINGS (E.G. EU, ASIA...)
to_be_dropped<- c(
  "Arab World",
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Europe & Central Asia",
  "European Union",
  "Fragile and conflict affected situations",
  "Heavily indebted poor countries (HIPC)",
  "High income",
  "IBRD only",
  "IDA & IBRD total",
  "IDA blend",
  "IDA only",
  "IDA total",
  "Lao PDR",
  "Late-demographic dividend",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Least developed countries: UN classification",
  "Low & middle income",
  "Low income",
  "Lower middle income",
  "Micronesia, Fed. Sts.",
  "Middle East & North Africa (excluding high income)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "Middle East & North Africa",
  "Middle income",
  "Not classified",
  "OECD members",
  "Other small states",
  "Post-demographic dividend",
  "Pre-demographic dividend",
  "South Africa",
  "South Asia (IDA & IBRD)",
  "South Asia",
  "Sub-Saharan Africa (excluding high income)",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "Sub-Saharan Africa",
  "Upper middle income",
  "World",
  "Caribbean small states" ,
  "Central Europe and the Baltics",
  "Early-demographic dividend",
  "East Asia & Pacific (excluding high income)",
  "East Asia & Pacific (IDA & IBRD countries)" ,
  "East Asia & Pacific",
  "Euro area", 
  "North America",
  "Pacific island small states", 
  "Russian Federation" ,
  "Small states" )
merged_v2 <- subset(merged, !(merged[, "Country Name"] %in% to_be_dropped))

#FILTRO: REMOVE NO OIL COUNTRY
no_oil = c("Bahrain",
           "Gabon",
           "Iran, Islamic Rep.", 
           "Iraq",
           "Kuwait",
           "Oman", 
           "Saudi Arabia", 
           "United Arab Emirates")
merged_v3 <- subset(merged_v2, !(merged_v2[, "Country Name"] %in% no_oil))

#COUNTRIES WITH POPULATION HIGHER THAN 1,000,000:
columns_to_delete <- c("Country Code", "Indicator Name", "Indicator Code")
clean_population <- population[, !names(population) %in% columns_to_delete]
means <- rowMeans(clean_population[, -1])
new_df <- data.frame(
  Column1 = clean_population["Country Name"],
  Mean = means)
final_df <- subset(new_df, Mean > 1000000)
definitive_pop_df <- subset(final_df, !(final_df[, "Country.Name"] %in% to_be_dropped))
countries <- definitive_pop_df$Country.Name
countries
#We modify some of the names that are not written in the same way
countries <- c(
  "Afghanistan", "Angola", "Albania", "United Arab Emirates", "Argentina", 
  "Armenia", "Australia", "Austria", "Azerbaijan", "Burundi", 
  "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", 
  "Bosnia and Herzegovina", "Belarus", "Bolivia", "Brazil", "Botswana", 
  "Central African Republic", "Canada", "Switzerland", "Chile", "China", 
  "Cote d'Ivoire", "Cameroon", "Congo, Dem. Rep.", "Congo, Rep.", 
  "Colombia", "Costa Rica", "Cuba", "Czechia", "Germany", "Denmark", 
  "Dominican Republic", "Algeria", "Ecuador", "Egypt, Arab Rep.", "Eritrea", 
  "Spain", "Estonia", "Ethiopia", "Finland", "France", "Gabon", 
  "United Kingdom", "Georgia", "Ghana", "Guinea", "Gambia, The", 
  "Guinea-Bissau", "Greece", "Guatemala", "Hong Kong SAR, China", 
  "Honduras", "Croatia", "Haiti", "Hungary", "Indonesia", "India", 
  "Ireland", "Iran, Islamic Rep.", "Iraq", "Israel", "Italy", 
  "Jamaica", "Jordan", "Japan", "Kazakhstan", "Kenya", "Kyrgyz Republic", 
  "Cambodia", "Korea, Rep.", "Kuwait", "Lebanon", "Liberia", 
  "Libya", "Sri Lanka", "Lesotho", "Lithuania", "Latvia", "Morocco", 
  "Moldova", "Madagascar", "Mexico", "North Macedonia", "Mali", 
  "Myanmar", "Mongolia", "Mozambique", "Mauritania", "Mauritius", 
  "Malawi", "Malaysia", "Namibia", "Niger", "Nigeria", "Nicaragua", 
  "Netherlands", "Norway", "Nepal", "New Zealand", "Oman", "Pakistan", 
  "Panama", "Peru", "Philippines", "Papua New Guinea", "Poland", 
  "Puerto Rico", "Korea, Dem. People's Rep.", "Portugal", "Paraguay", 
  "Romania", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", "Singapore", 
  "Sierra Leone", "El Salvador", "Somalia", "Serbia", "South Sudan", 
  "Slovak Republic", "Slovenia", "Sweden", "Syrian Arab Republic", 
  "Chad", "Togo", "Thailand", "Tajikistan", "Turkmenistan", "Trinidad and Tobago", 
  "Tunisia", "Turkiye", "Tanzania", "Uganda", "Ukraine", "Uruguay", 
  "United States", "Uzbekistan", "Venezuela, RB", "Viet Nam", "Kosovo", 
  "Yemen, Rep.", "Zambia")
merged_v4 = subset(merged_v3, (merged_v3[, "Country Name"] %in% countries))

# RENAME THE DATAFRAME
data = merged_v4

#EXPORT THE DATAFRAME AS CSV FILE IN THE WORKING DIRECTORY
#write.csv(data, "cleaned_final.csv", row.names = FALSE)
#getwd()










### TASK 1 ###

## IMPORT THE DATASET
data <- read.csv("cleaned_final.csv")
data <- data[order(data$Year),]

## COUNT THE NEGATIVE VALUES 
na_count <- colSums(is.na(data))
na_count
# a lot of nas for all the variables, especially for sav_rate and rnd_exp
negative_count <- colSums(data < 0, na.rm = TRUE)
negative_count
# consistent amount of negative values for population growth, cannot take the log

## REMOVE NEGATIVE VALUES TO RUN LOG-REGRESSION
#The negative values are not a very large part of the dataset, so we decided to drop them to avoid any problems with the log
data_ng <- data[data$pop_growth > 0 & data$sav_rate > 0, ]

## RUN NORMAL OLS REGRESSION
model_OLS = lm(log(gdp_pc) ~ log(sav_rate) + log(pop_growth), data = data_ng)
summary(model_OLS)
#coefficient of sav_rate is +0.83, statistically significant
#coefficient of pop_growth is -0.86, statistically significant
#opposite direction, -> in line with the economic theory of the Solow model

## RUN ASSUMPTIONS' TESTS ON THIS REGRESSION
dwtest(model_OLS) #Durbin Watson Test, -> Rejected
bgtest(model_OLS) #Breusch-Godfrey Test -> Rejected
bptest(model_OLS) #Breusch-Pagan Test -> Rejected
resettest(model_OLS) #Reset Test -> Rejected 

## PLOT THE OLS RESIDUALS OF WHOLE DATASET
residuals_ols = residuals(model_OLS)
residual_index = 1:length(residuals_ols)
plot(residual_index, residuals_ols, type = "p", pch = 16, cex = 0.5, main = "Residuals Plot with Regression Line", xlab = "Observation", ylab = "Residuals")
# HORIZONTAL LINE FOR REFERENCE
abline(h = 0, col = "red", lwd=3)
# LINEAR REGRESSION FOR REFERENCE
residual_index <- 1:length(residuals_ols)
residual_model <- lm(residuals_ols ~ residual_index)
# PLOT THE FITTED CURVE TO PROVE SERIAL CORRELATION
abline(residual_model, col = "blue", lwd=3)

## DO OLS REGRESSION FOR ONE SINGLE YEAR TO SHOW THAT SERIAL
## CORRELATION IS GIVEN BY SUBSEQUENT YEARS
sin_data = data.table(data_ng)
single = sin_data[Year == 2016]
model_single = lm(log(gdp_pc) ~ log(sav_rate) + log(pop_growth), data = single)
summary(model_single)

## RUN TESTS AND PLOT THE RESIDUALS
dwtest(model_single) #Durbin Watson Test, -> Not rejected
bgtest(model_single) #Breusch-Godfrey Test -> Not rejected
bptest(model_single) #Breusch-Pagan Test -> Not rejected
resettest(model_single) #Reset Test -> Rejected, The model is a simplification, real model is not linear

## PLOT RESIDUALS
single_residuals = residuals(model_single)
single_residual_index = 1:length(single_residuals)
plot(single_residual_index, single_residuals, type = "p", pch = 16, cex = 0.5, main = "Residuals Plot with Regression Line", xlab = "Observation", ylab = "Residuals")
abline(h = 0, col = "red", lwd=3)
single_residual_model <- lm(single_residuals ~ single_residual_index)
abline(single_residual_model, col = "blue", lwd=3)

## FGLS TO CHECK OUR INTUITION 
data_gls = data_ng[complete.cases(data_ng$sav_rate, data_ng$pop_growth, data_ng$gdp_pc), ] # omit the NAs just for the sav_rate, pop_growth and gdp_pc
fgls_model = gls(log(gdp_pc) ~ log(sav_rate) + log(pop_growth), data = data_gls, correlation = corAR1()) # FGLS model with built in function
summary(fgls_model)
# very similar results compared to OLS

## COMPARE RESIDUALS OLS v. GLS
gls_residuals = resid(fgls_model)
plot(residuals_ols, type = "l", col = "blue", xlab = "Sample Index", ylab = "Residuals", main = "Residuals for Standard Linear Regression")
plot(gls_residuals, type = "l", col = "red", xlab = "Sample Index", ylab = "Residuals", main = "Residuals for GLS Regression")
# very similar residuals, still serial correlation. FGLS does not solve the problem because of the ordering of our dataset

## COMMENT ON OLS RESULTS (LIMITATION: NOT BLUE)
# The signs of the coefficients are in line with economic theory: higher savings rate is related
# to higher GDP per capita, and higher population growth is related to a decrease of gdp per
# capita instead. The size of the coefficients gives us a measure of elasticities, since we are
# analysing a log-log relationship for all the variables. An increase of one percent of the
# saving rate is associated with an increase of 0.84% of the GDP per capita, while an
# increase of 1% of the population growth is associated with a decrease of 0.87% in the
# GDP per capita.










### TASK 2 ###

#CHOOSEN VARIABLE --> Research and development expenditure (% of GDP)
#R&D activities are crucial for technological progress, 
#which is a key driver of long-term economic growth. 
#By investing in R&D, firms and industries can develop new technologies, 
#improve existing ones, and enhance productivity levels. 
#Including R&D expenditure in the Solow model acknowledges 
#the role of technological advancement in shaping the growth of an economy.
#Including R&D expenditure in the Solow growth model acknowledges 
#the central role of technological progress, knowledge spillovers, 
#human capital accumulation, competitive advantage, 
#and dynamic effects in driving long-term economic growth.

#ECONOMIC THEORY: 
#R&D investment fosters innovation and productivity, 
#thus they are very likely to be positively correlated with GDP. 
#Developed nations are usually characterised by a negative or near zero population growth,
#but they are also the countries that typically allocate more to R&D, 
#while developing ones with high population growth are usually the ones that invest less, 
#this is why we expect a #negative correlations between rnd_exp and pop_growth.
#Savings often serve as a primary investment capital source, 
#so when they rise, usually also the investment are positively affected, 
#thus we expect a positive correlation between the two

#OLS:
data_gls_2 = na.omit(data_ng)   #We omit all the lines with Na entries, considering also the “new” column rnd_exp
model_OLS_2 = lm(log(gdp_pc) ~ log(sav_rate) + log(pop_growth)+ log(rnd_exp), data = data_gls_2) 
summary(model_OLS_2)
#coefficient sav_rate is +0.25164 (lower than before), statistically significant
#coefficient pop_growth is -0.23530 (greater than before), statistically significant
#coefficient rnd_exp is +0.82340, statistically significant
#Results in line with our economic theory

#CORRELATION AMONG THE INDEPENDENT VARIABLES:
#WITHOUT THE LOG
cor(data_gls_2$pop_growth, data_gls_2$rnd_exp)
#Negative correlated
cor(data_gls_2$sav_rate, data_gls_2$rnd_exp)
#Positive correlated
# WITH LOG
cor(log(data_gls_2$pop_growth), log(data_gls_2$rnd_exp))
#Negative correlated
cor(log(data_gls_2$sav_rate), log(data_gls_2$rnd_exp))
#Positive correlated
#The introduction of this new variable, lead to a drop in the coefficient of the “sav_rate” 
#and an increase in the coefficient of the “pop_growth”, 
#this because the new variable reduced an upward bias resulting from the positive correlation 
#between the sav_rate and the rnd_exp and it reduced a downward bias resulting from the negative correlation 
#between the pop_growth and the rnd_exp.








#OLS SINGLE YEAR: (JUST A TEST)
sin_data_2 = data.table(data_gls_2)
single = sin_data_2[Year == 2012]
model_single = lm(log(gdp_pc) ~ log(sav_rate) + log(pop_growth) + log(rnd_exp), data = single)
summary(model_single)

#FGLS (JUST A TEST):
fgls_model_2 = gls(log(gdp_pc) ~ log(sav_rate) + log(pop_growth) + log(rnd_exp), data = data_gls_2, correlation = corAR1())
summary(fgls_model_2)

