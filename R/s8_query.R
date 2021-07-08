# This script downloads s8 stormwater data from the WA Dept of Ecology and
# generates a Kaplan Meier emperial cummulative distribution function
# in order to estimate statistics for censored data

if (!require('plyr')) install.packages('plyr')
if (!require('NADA')) install.packages('NADA')
if (!require('RSocrata')) install.packages('RSocrata')
if (!require('Hmisc')) install.packages('Hmisc')
if (!require('urltools')) install.packages('urltools')
library(NADA)
library(RSocrata)
library(dplyr)
library(Hmisc)



#get tacoma data
get_s8_data <- function(
  parameter = "all", study_name = "all", location_name = "all", reject_flags = c("REJ")) {

  query <- paste0("https://data.wa.gov/resource/d958-q2ci.json")
  if(parameter != "all"){

  query <- param_set(query,"parameter",parameter)
  }
  if(study_name != "all"){
    query <- param_set(query,"study_name",study_name)
  }
  if(location_name != "all"){
    query <- param_set(query,"location_name",location_name)
  }

return(read.socrata(query) %>% dplyr::filter(!result_data_qualifier %in% reject_flags))

}


# if(!is.null(reject_flags)){
#   query <-paste0(query,"$where =result_data_qualifier not_in ",paste0(reject_flags))
# }
# "Copper - Water - Total")
# "Copper - Water - Dissolved",
# "Zinc - Water - Dissolved")
# "Nitrite-Nitrate - Water - Dissolved",
# "Total Kjeldahl Nitrogen - Water - Total")
# "Total Phosphorus - Water - Total",
# "Total Suspended Solids - Water - Total",
# "Total PAH - Water - Total")

#read.socrata
tacoma <- "City of Tacoma Phase I Municipal Stormwater Permit"
total_zinc <- "Zinc - Water - Total"


#"Zinc - Water - Total")#, #list of parameters
# "Copper - Water - Total")
# "Copper - Water - Dissolved",
# "Zinc - Water - Dissolved")
# "Nitrite-Nitrate - Water - Dissolved",
# "Total Kjeldahl Nitrogen - Water - Total")
# "Total Phosphorus - Water - Total",
# "Total Suspended Solids - Water - Total",
# "Total PAH - Water - Total")

zinc_data <-get_s8_data(parameter = total_zinc)
all_data <- get_s8_data()


codes <-  readr::read_csv("data-raw/DataQualifiers.csv")

qualifiers <- all_data %>% select(result_data_qualifier) %>%
  unique()

flag_dictionary <- qualifiers %>%
  merge(codes,by.x ="result_data_qualifier",by.y="Result Data Qualifier Code" )

#Filter based on parameters; remove rejected samples
data2 <- data%>%
  filter(parameter %in% parameters)

tfwfd6 <- data2 %>%
  filter

data2 <- data2[ which(data2$result_data_qualifier != 'REJ'
                      | is.na(data2$result_data_qualifier )), ]

  #filter(paramstatus %in% 'Include') %>%
  filter(result_data_qualifier != 'REJ') %>%
  mutate(nondetect_flag = recode(nondetect_flag,
                                 "WARNING" = "FALSE"))


data$nondetect_flag <- as.logical(data$nondetect_flag)
data$new_result_value <- as.numeric(data$new_result_value)

obs <- data$new_result_value
censored <- data$nondetect_flag

# With groups
groups <- data$type
groups <-as.factor(groups)

#Kaplan Meier ECDF
mycenfit <- cenfit(obs, censored, groups)
plot(mycenfit,conf.int=FALSE,
     ylim=c(0, 0.962))
legend("bottomright",
       legend=unique(groups),
       #horiz=FALSE,
       bty='n',
       lty=c(1,2,3,4))
title(main = parameters, sub = "Concentration ug/L", xlab = NULL, ylab = NULL,
      line = NA, outer = NA)

#summary stats
summary(mycenfit)
quantile(mycenfit, conf.int=TRUE) #prints quantiles
predict(mycenfit, c(10, 20, 100), conf.int=TRUE) #used for probability prediction

# Formula interface -- no groups
cenfit(Cen(obs, censored))

# Formula interface -- with groups
cenfit(Cen(obs, censored)~groups)

