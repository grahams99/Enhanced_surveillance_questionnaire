#Enhanced Surveillance Questionnaire 

#set working directory
setwd("[removed for UKHSA privacy reasons]") 

#libraries
library("haven")
library("magrittr")
library("dplyr")
library("skimr")
library("lubridate")
library("rstatix")
library("janitor")
library("tibble")
library("openxlsx")
library("flextable")
library("rio")
library("questionr")
library("dplyr")
library("epiR")
library("gtsummary")
library("finalfit")
library("EpiStats")
library("bit64")
library("tidyr")
library("tidyverse")
library("purrr")
library("broom")

#read in the data 
data <- import("[removed for UKHSA privacy reasons]")

##remove duplicates based on patid
##check how many duplicates there are 
#sum(table(QTND$patid)-1)
##remove duplicates 
data <- distinct(data,patid, .keep_all = TRUE)

#remove individuals with previous infections
#QTND %>% tabyl(pos_dec7, idcompleted)
#QTND %>% tabyl(pospriortocohort, idcompleted)

#replace blank cells with NA
data[data == ""] <- NA

#label the data 
data %<>%
  mutate(
    iddate_final = as.Date(iddate_final, format = "%d/%m/%Y"),
    epistart1 = as.Date(epistart1, format = "%d/%m/%Y"),
    date_of_onset_SUS = as.Date(date_of_onset_SUS, format = "%d/%m/%Y"),
    date_of_test_SUS = as.Date(date_of_test_SUS, format = "%d/%m/%Y"),
    dob = as.Date(dob, format = "%d/%m/%Y"),
    dose2_date = as.Date(dose2_date, format = "%d/%m/%Y"),
    dose1_date = as.Date(dose1_date, format = "%d/%m/%Y"),
    sgss_symptom_date = as.Date(sgss_symptom_date, format = "%d/%m/%Y"),
    sgss_test_date = as.Date(sgss_test_date, format = "%d/%m/%Y"),
    idcompleted = factor(
      idcompleted,
      levels = c("notcompleted", "completed"),
      labels = c("notcompleted", "completed")
    ),
    primarydiagnosiscode1 = as.character(primarydiagnosiscode1),
    admit = factor(
      admit,
      levels = c("0", "1"),
      labels = c("not admitted to hospital", "admitted to hospital")
    ),
    accomodation_type_1 = factor(
      accomodation_type_1,
      levels = c("Privatehome", "Council/housingassociation", "Shelteredaccommodation", "Carehome/Nursinghome", "Other"),
      labels = c("Private home", "Council/ housing association", "Sheltered accommodation", "Carehome/ Nursing home", "Other")
    ),
    no_ppl_household = factor(
      no_ppl_household,
      levels = c("0", "1", "2", "3", "4", "5ormore"),
      labels = c("0", "1", "2", "3", "4", "5 or more")
    ),
    cev2 = factor(
      cev2,
      levels = c(0, 1),
      labels = c("not CEV", "CEV")
    ),
    CHD = factor(
      CHD,
      levels = c(0, 1),
      labels = c("no CHD", "Chronic heart disease")
    ),
    diar = factor(
      diar,
      levels = c(0, 1),
      labels = c("no Diarrhoea", "Diarrhoea")#not sure why there are no NAs for this
    ),
    CKD = factor(
      CKD,
      levels = c(0, 1),
      labels = c("no CKD", "Chronic kidney disease")
    ),
    CLD = factor(
      CLD,
      levels = c(0, 1),
      labels = c("no CLD", "Chronic liver disease")
    ),
    CRD = factor(
      CRD,
      levels = c(0, 1),
      labels = c("no CRD", "Chronic respiratory disease")
    ),
    asthma_med = factor(
      asthma_med,
      levels = c(0, 1),
      labels = c("no asthma req medication", "Asthma requring medication")
    ),
    cancer = factor(
      cancer, 
      levels = c(0, 1),
      labels = c("no cancer", "Cancer")
    ),
    organ_bone_transplant = factor(
      organ_bone_transplant,
      levels = c(0, 1),
      labels = c("no organ or bone transplant", "Organ or bone transplant")
    ),
    HIV_immuno = factor(
      HIV_immuno,
      levels = c(0, 1),
      labels = c("no HIV/immunodeficiency", "HIV/immunodeficiency")
    ),
    immuno_med = factor(
      immuno_med,
      levels = c(0, 1),
      labels = c("no immunosuppression due to medication", "Immunosuppression due to medication")
    ),
    seizure_dis = factor(
      seizure_dis,
      levels = c(0, 1),
      labels = c("no seizure disorder", "Seizure disorder")
    ),
    CND = factor(
      CND,
      levels = c(0, 1),
      labels = c("no chronic neurological disease", "Chronic neurological disease")
    ),
    asplenia = factor(
      asplenia,
      levels = c(0, 1),
      labels = c("no asplenia or dys spleen", "Aslpenia or dysfunctioning spleen")
    ),
    BMI_40 = factor(
      BMI_40,
      levels = c(0, 1),
      labels = c("BMI not =>40", "BMI =>40")
    ),
    None_above_conditions = factor(
      None_above_conditions,
      levels = c(0, 1),
      labels = c("above conditions", "None above conditions")
    ),
    invitation = factor(
      invitation,
      levels = c(0, 1),
      labels = c("no vaccine invitation", "invited for vaccination")
    ),
    doses = factor(
      doses,
      levels = c(0, 1, 2),
      labels = c("no doses", "one dose", "two doses")
    ),
    dur_invite = factor(
      dur_invite,
      levels = c(1, 2, 3, 4),
      labels = c("less than 2 weeks", "2-3 weeks", "4 or more weeks", "I had my vaccine before elegible")
    ),
    not_aware_eleg = factor(
      not_aware_eleg,
      levels = c(0, 1),
      labels = c("aware I was elegible", "not aware I was elegible")
    ),
    no_app_av = factor(
      no_app_av,
      levels = c(0, 1),
      labels = c("appointments available", "no appointments available")
    ),
    pref_wait = factor(
      pref_wait,
      levels = c(0, 1),
      labels = c("not prefer to wait", "prefer to wait to be vaccinated")
    ),
    delayed_covid = factor(
      delayed_covid,
      levels = c(0, 1),
      labels = c("did not delay because I had COVID-19 or symptoms", "delayed because I had COVID-19 or symptoms")
    ),
    isolating = factor(
      isolating,
      levels = c(0, 1),
      labels = c("was not isolating", "isolating")
    ),
    vacc_time = factor(
      vacc_time,
      levels = c(0, 1),
      labels = c("had time", "did not have time")
    ),
    vacc_other = factor(
      vacc_other,
      levels = c(0, 1),
      labels = c("not other", "other")
    ),
    dose1_manufac2 = factor(
      dose1_manufac2,
      levels = c(1, 2),
      labels = c("Pfizer", "AstraZeneca")
    ),
    walk_cyc = factor(
      walk_cyc,
      levels = c(0, 1),
      labels = c("did not walk/cycle", "walk/cycle")
    ),
    car_alone_dose1 = factor(
      car_alone_dose1,
      levels = c(0, 1),
      labels = c("not care alone", "car alone or memb household")
    ),
    car_diff_dose1 = factor(
      car_diff_dose1,
      levels = c(0, 1),
      labels = c("not car different household", "car with different household")
    ),
    public_dose1 = factor(
      public_dose1,
      levels = c(0, 1),
      labels = c("not public transport", "public transport")
    ),
    transport_other_dose1 = factor(
      transport_other_dose1,
      levels = c(0, 1),
      labels = c("not other transport", "other transport")
    ),
    mixing_dose1 = factor(
      mixing_dose1,
      levels = c(1, 2, 3),
      labels = c("mixed same amount", "mixed more", "mixed less")
    ),
    dose2_manufac2 = factor(
      dose2_manufac2,
      levels = c(1, 2),
      labels = c("Pfizer", "AstraZeneca")
    ),
    transport_dose2 = factor(
      transport_dose2, 
      levels = c(0, 1, 2, 3, 4),
      labels = c("Walking / cycling", "car alone or memb house", "car diff house", "public transport", "other")
    ),
    mixing_dose2 = factor(
      mixing_dose2,
      levels = c(1, 2, 3),
      labels = c("mixed same amount", "mixed more", "mixed less")
    ),
    not_vacc_not_called = factor(
      not_vacc_not_called,
      levels = c(0, 1),
      labels = c("called for vaccine", "not called for vaccine")
    ),
    not_vacc_elig = factor(
      not_vacc_elig,
      levels = c(0, 1),
      labels = c("aware elegible", "not aware elegible")
    ),
    not_vacc_app = factor(
      not_vacc_app,
      levels = c(0, 1),
      labels = c("appointments available", "no appointments available")
    ),
    not_vacc_prefer_not = factor(
      not_vacc_prefer_not,
      levels = c(0, 1),
      labels = c("preferred not to wait", "prefer to wait")
    ),
    not_vacc_soon = factor(
      not_vacc_soon,
      levels = c(0, 1),
      labels = c("dont expect to get vaccinated soon", "expect to get vaccinated soon")
    ),
    not_vacc_unwell = factor(
      not_vacc_unwell,
      levels = c(0, 1),
      labels = c("have not been unwell", "have been unwell")
    ),
    not_vacc_isol = factor(
      not_vacc_isol,
      levels = c(0, 1),
      labels = c("not isolating", "isolating")
    ),
    not_vacc_time = factor(
      not_vacc_time,
      levels = c(0, 1),
      labels = c("had time", "did not have time")
    ),
    not_vacc_other = factor(
      not_vacc_other,
      levels = c(0, 1),
      labels = c("not other", "other")
    ),
    test_COVID = factor(
      test_COVID,
      levels = c(0, 1),
      labels = c("did not have covid", "had covid")
    ),
    test_case = factor(
      test_case,
      levels = c(0, 1),
      labels = c("not in contact with case", "in contact with case")
    ),
    test_care = factor(
      test_care,
      levels = c(0, 1),
      labels = c("not tested in care home", "tested in care home")
    ),
    test_hosp = factor(
      test_hosp,
      levels = c(0, 1),
      labels = c("not tested in hospital", "tested in hospital")
    ),
    test_surge = factor(
      test_surge,
      levels = c(0, 1),
      labels = c("not tested as part of surge testing", "tested as part of surge testing")
    ),
    test_illness = factor(
      test_illness,
      levels = c(0, 1),
      labels = c("did not have an illness", "illness")
    ),
    test_other = factor(
      test_other,
      levels = c(0, 1),
      labels = c("not other", "other")
    ),
    confirm_symptoms2 = factor(
      confirm_symptoms2, 
      levels = c(0, 1),
      labels = c("did not have symptoms", "had symptoms")
    ),
    date_symt_onset = as.Date(date_symt_onset, format = "%d/%m/%Y"),
    fever = factor(
      fever,
      levels = c(0, 1),
      labels = c("no fever", "fever")
    ),
    runny_nose = factor(
      runny_nose,
      levels = c(0, 1),
      labels = c("no runny nose", "runny nose")
    ),
    cough = factor(
      cough,
      levels = c(0, 1),
      labels = c("no cough", "cough")
    ),
    short_breath = factor(
      short_breath,
      levels = c(0, 1),
      labels = c("no shortness of breath", "shortness of breath")
    ),
    sore_throat = factor(
      sore_throat,
      levels = c(0, 1),
      labels = c("no sore throat", "sore throat")
    ),
    loss_taste = factor(
      loss_taste,
      levels = c(0, 1),
      labels = c("no loss taste", "loss taste")
    ),
    nausea = factor(
      nausea,
      levels = c(0, 1),
      labels = c("no nausea", "nausea")
    ),
    diarrhoea = factor(
      diarrhoea,
      levels = c(0, 1),
      labels = c("no diarrhoea", "diarrhoea")
    ),
    headache = factor(
      headache,
      levels = c(0, 1),
      labels = c("no headache", "headache")
    ),
    body_pain = factor(
      body_pain,
      levels = c(0, 1),
      labels = c("no body pain", "body pain")
    ),
    fatigue = factor(
      fatigue,
      levels = c(0, 1),
      labels = c("no fatigue", "fatigue")
    ),
    symptoms_other = factor(
      symptoms_other,
      levels = c(0, 1),
      labels = c("no other symptoms", "other symptoms")
    ),
    severity = factor(
      severity,
      levels = c(1, 2, 3),
      labels = c("mild", "moderate", "severe")
    ),
    gp = factor(
      gp,
      levels = c(0, 1),
      labels = c("did not access gp", "Accessed GP")
    ),
    nhs_111 = factor(
      nhs_111,
      levels = c(0, 1),
      labels = c("did not access nhs 111", "Accessed NHS 111")
    ),
    hospital = factor(
      hospital,
      levels = c(0, 1),
      labels = c("did not go to hosp", "Hospital")
    ),
    healthcare_other = factor(
      healthcare_other,
      levels = c(0, 1),
      labels = c("did not access other healthcare", "Other healthcare")
    ),
    healthcare_none = factor(
      healthcare_none,
      levels = c(0, 1),
      labels = c("accessed these healthcare", "Did not access any of these healthcare services")
    ),
    emergency = factor(
      emergency,
      levels = c(0, 1),
      labels = c("not admitted emergency", "Admitted to emergency department")
    ),
    admitted_hosp = factor(
      admitted_hosp,
      levels = c(0, 1, 99),
      labels = c("not admitted to hosp", "admitted to hosp", "missing")
    ),
    hosp_covid = factor(
      hosp_covid,
      levels = c(0, 1, 99),
      labels = c("unrelated to covid", "covid related", "missing")
    ),
    date_hosp_admission = as.Date(date_hosp_admission, format = "%d/%m/%Y"),
    no_days_in_hosp = as.numeric(no_days_in_hosp),
    week_car = factor(
      week_car,
      levels = c(0, 1, 2),
      labels = c("did not travel in car with someone outside house", "unsure", "car with someone outside house")
    ),
    week_did_you_go = factor(
      week_did_you_go,
      levels = c(0, 1, 2),
      labels = c("not indoors with others outside house", "unsure", "indoors with others outside house")
    ),
    week_public = factor(
      week_public,
      levels = c(0, 1, 2),
      labels = c("not public transport", "unsure", "public transport")
    ),
    flu_vacc = factor(
      flu_vacc,
      levels = c(0, 1, 2),
      labels = c("No 2020/21 flu vaccination", "Unsure", "2020/21 flu vaccination")
    ),
    manufacturer_dose1 = factor(
      manufacturer_dose1,
      levels = c("PF", "AZ"),
      labels = c("Pfizer", "AstraZeneca")
    ),
    manufacturer_dose2 = factor(
      manufacturer_dose2,
      levels = c("PF", "AZ"),
      labels = c("Pfizer", "AZ")
    ),
    CEV_flag_2611 = factor(
      CEV_flag_2611,
      levels = c(0, 1),
      labels = c("Not CEV", "CEV")
    ),
    risk_nims = factor(
      risk_nims,
      levels = c(0, 1),
      labels = c("not risk", "risk")
    ),
    date_of_test = as.Date(date_of_test, format = "%d/%m/%Y"),
    vaccine_date_dose1 = as.Date(vaccine_date_dose1, format = "%d/%m/%Y"),
    vaccine_date_dose2 = as.Date(vaccine_date_dose2, format = "%d/%m/%Y"),
    date_of_onset = as.Date(date_of_onset, format = "%d/%m/%Y"),
    Age_Mar31 = as.numeric(Age_Mar31),
    res = factor(
      res,
      levels = c(0, 1),
      labels = c("Negative", "Positive")
    ),
    carehome = factor(
      carehome,
      levels = c(0, 1),
      labels = c("Not care home", "Care home")
    ),
    carehome_new = factor(
      carehome_new,
      levels = c(0, 1),
      labels = c("not care home", "care home")
    ),
    eth1 = factor(
      eth1,
      levels = c(1, 2, 3, 6, 8, 11, 16, 17, 18, 21, 22, 23, 24),
      labels = c("African", "Another Asian background", "Another Black background", "Another ethnic background", "Arab", "Bangladeshi", "Caribbean", "Chinese", "Indian", "Mixed or multiple ethnic groups", "Pakistani", "Prefer not to say", "White")
    ),
    imd5 = factor(
      imd5,
      levels = c(1, 2, 3, 4, 5),
      labels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)")
    ),
    gender = factor(
      gender,
      levels = c(1, 2),
      labels = c("Female", "Male")
    ),
    symptoms = factor(
      symptoms,
      levels = c(0, 1),
      labels = c("no symptoms", "symptoms")
    ),
    nhsregion = factor(
      nhsregion,
      levels = c(1:7),
      labels = c("East of England", "London", "Midlands", "North East and Yorkshire", "Northwest", "South East", "South West")
    ),
    age5 = factor(
      age5,
      levels = c(15:19),
      labels = c("70-74", "75-79", "80-84", "85-89", "=>90")
    ),
    week_onset = factor(
      week_onset,
      levels = c(1:7),
      labels = c("Jan week 1", "Jan week 2", "Jan week 3", "Jan week 4", "Feb week 1", "Feb week 2", "Feb week 3")
    ),
    week_test = factor(
      week_test,
      levels = c(5:7),
      labels = c("Feb week 1", "Feb week 2", "Feb week 3")
    ),
    pos_dec7 = factor(
      pos_dec7,
      levels = c(0, 1),
      labels = c("No previous positive test", "Positive previous test")
    )
  )

####creating new variables
#combine ethnicity 
data %<>%
  mutate(
    eth2 = factor(
      case_when(
        eth1 == "White" ~ "0",
        eth1 == "African" | eth1 == "Another Asian background" | eth1 == "Another Black background" | eth1 == "Another ethnic background" | eth1 == "Arab" | eth1 == "Bangladeshi" | eth1 == "Caribbean" | eth1 == "Chinese" | eth1 == "Indian" | eth1 == "Mixed or multiple ethnic groups" | eth1 == "Pakistani" ~ "1",
        eth1 == "Prefer not to say" ~ "2"
      ),
      levels = c(0, 1, 2),
      labels = c("White", "Non-White", "Prefer not to say")
    )
  )

#combine NHS regions
data %<>%
  mutate(
    nhsregion2 = factor(
      case_when(
        nhsregion == "London" ~ "0",
        nhsregion == "East of England" | nhsregion == "South East" | nhsregion == "South West" ~ "1",
        nhsregion == "Midlands" | nhsregion == "North East and Yorkshire" | nhsregion == "Northwest" ~ "2"
      ),
      levels = c(0, 1, 2),
      labels = c("London", "South England ex-London", "North England")
    )
  )

#if dose 1 from questionnaire doesn't occur before 21st March then set to missing
#duplicate date column
data$dose1_date2 = data$dose1_date

x <- data$dose1_date2 >= "2021-03-21"

data$dose1_date2[x] <- NA

#if dose 2 from questionnaire doesn't occur before 21st March then set to missing
data$dose2_date2 = data$dose2_date

y <- data$dose2_date2 >= "2021-03-21"

data$dose2_date2[y] <- NA

#create new variable: vaccination status from the questionnaire - during TND study period
a <- !is.na (data$dose1_date2)
b <- !is.na (data$dose2_date2)
c <- is.na (data$dose1_date2)
d <- is.na (data$dose2_date2) 

data %<>%
  mutate(
    vaccination_status_q = factor(
      case_when(
        c == "TRUE" & d == "TRUE" ~ "1",
        a == "TRUE" & d == "TRUE" ~ "2",
        a == "TRUE" & b == "TRUE" ~ "3"
      ),
      levels = c(1, 2, 3),
      labels = c("no doses", "one dose", "two doses")
    )
  )

data %<>%
  mutate(
    vaccine_status_q_2levels = factor(
      case_when(vaccination_status_q == "no doses" ~ "0",
                vaccination_status_q == "one dose" | vaccination_status_q == "two doses" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("Not vaccinated", "Vaccinated")
    )
  )

##mutate date variables
data %<>%
  mutate(dose1_date2 = as.Date(dose1_date2, format = "%d/%m/%Y"))

data %<>%
  mutate(dose2_date2 = as.Date(dose2_date2, format = "%d/%m/%Y"))

#create new variable: vaccination status from TND - during study period
e <- !is.na (data$vaccine_date_dose1)
f <- !is.na (data$vaccine_date_dose2)
g <- is.na (data$vaccine_date_dose1)
h <- is.na (data$vaccine_date_dose2)

data %<>%
  mutate(
    vaccine_status_TND = factor(
      case_when(
        g == "TRUE" & h == "TRUE" ~ "1",
        e == "TRUE" & h == "TRUE" ~ "2",
        e == "TRUE" & f == "TRUE" ~ "3"
      ),
      levels = c(1, 2, 3),
      labels = c("No doses", "One dose", "Two doses")
    )
  )

data %<>%
  mutate(
    vaccine_status_TND_2levels = factor(
      case_when(vaccine_status_TND == "No doses" ~ "0",
                vaccine_status_TND == "One dose" | vaccine_status_TND == "Two doses" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("Not vaccinated", "Vaccinated")
    )
  )

#create new variable: 
#check how often date of SGSS onset is before vaccination date - first dose
data %<>%
  mutate(
    onset_after_vacc = factor(
      case_when(
        vaccine_date_dose1 > date_of_onset ~ "0",
        vaccine_date_dose1 <= date_of_onset ~ "1"
      ),
      levels = c(0, 1),
      labels = c("vaccine after onset", "vaccine before onset")
    )
  )

#check how often date of SGSS onset is before vaccination date - second dose
data %<>%
  mutate(
    onset_after_vacc_dose_2 = factor(
      case_when(
        vaccine_date_dose2 > date_of_onset ~ "0",
        vaccine_date_dose2 <= date_of_onset ~ "1"
      ),
      levels = c(0, 1),
      labels = c("vaccine after onset", "vaccine before onset")
    )
  )

#create new variable - vaccination before onset
#questionnaire 
data$dose1_date4 = data$dose1_date2

x <- data$dose1_date4 > data$date_of_onset

data$dose1_date4[x] <- NA

#dose 2
data$dose2_date4 = data$dose2_date2

x <- data$dose2_date4 > data$date_of_onset

data$dose2_date4[x] <- NA

m <- !is.na (data$dose1_date4)
n <- !is.na (data$dose2_date4)
o <- is.na (data$dose1_date4)
p <- is.na (data$dose2_date4)

data %<>%
  mutate(
    vaccine_status_q_onset = factor(
      case_when(
        o == "TRUE" & p == "TRUE" ~ "1",
        m == "TRUE" & p == "TRUE" ~ "2",
        m == "TRUE" & n == "TRUE" ~ "3"
      ),
      levels = c(1, 2, 3),
      labels = c("No doses", "One dose", "Two doses")
    )
  )

data %<>%
  mutate(
    vaccine_status_q_2levels_onset = factor(
      case_when(vaccine_status_q_onset == "No doses" ~ "0",
                vaccine_status_q_onset == "One dose" | vaccine_status_q_onset == "Two doses" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("Not vaccinated", "Vaccinated")
    )
  )

##VACCINATION STATUS
#first - if vaccination date doesn't occur before onset - set to missing
#dose 1
data$vaccine_date_dose1_3 = data$vaccine_date_dose1

x <- data$vaccine_date_dose1_3 > data$date_of_onset

data$vaccine_date_dose1_3[x] <- NA

#dose 2
data$vaccine_date_dose2_3 = data$vaccine_date_dose2

x <- data$vaccine_date_dose2_3 > data$date_of_onset

data$vaccine_date_dose2_3[x] <- NA

m <- !is.na (data$vaccine_date_dose1_3)
n <- !is.na (data$vaccine_date_dose2_3)
o <- is.na (data$vaccine_date_dose1_3)
p <- is.na (data$vaccine_date_dose2_3)

data %<>%
  mutate(
    vaccine_status_TND_onset = factor(
      case_when(
        o == "TRUE" & p == "TRUE" ~ "1",
        m == "TRUE" & p == "TRUE" ~ "2",
        m == "TRUE" & n == "TRUE" ~ "3"
      ),
      levels = c(1, 2, 3),
      labels = c("No doses", "One dose", "Two doses")
    )
  )

data %<>%
  mutate(
    vaccine_status_TND_2levels_onset = factor(
      case_when(vaccine_status_TND_onset == "No doses" ~ "0",
                vaccine_status_TND_onset == "One dose" | vaccine_status_TND_onset == "Two doses" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("Not vaccinated", "Vaccinated")
    )
  )

#create new variable: doses 2 levels 
data %<>%
  mutate(
    doses2levels = factor(
      case_when(doses == "no doses" ~ "0",
                doses == "one dose" | doses == "two doses" ~ "1" 
      ),
      levels = c(0, 1),
      labels = c("no doses", "1 or 2 dose")
    )
  )

#create vaccination categories
data %<>%
  mutate(
    vaccination_categories = factor(
      case_when(
        vaccine_status_TND_2levels_onset == "Not vaccinated" ~ "0",
        vacc1_onset >= 0 & vacc1_onset <= 3 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "1", 
        vacc1_onset > 3 & vacc1_onset <= 6 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "2",
        vacc1_onset > 6 & vacc1_onset <= 9 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "3",
        vacc1_onset > 9 & vacc1_onset <= 13 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "4",
        vacc1_onset > 13 & vacc1_onset <= 20 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "5",
        vacc1_onset > 20 & vacc1_onset <= 27 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "6",
        vacc1_onset > 27 & vacc1_onset <= 34 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "7",
        vacc1_onset > 34 & vacc1_onset <= 41 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "8",
        vacc1_onset > 41 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="AstraZeneca" ~ "9",
        vacc2_onset >= 0 & vacc2_onset <= 3 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="AZ" ~ "10",
        vacc2_onset > 3 & vacc2_onset <= 6 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="AZ" ~ "11",
        vacc2_onset > 6 & vacc2_onset <= 13 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="AZ" ~ "12",
        vacc2_onset > 13 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="AZ" ~ "13",
        vacc1_onset >= 0 & vacc1_onset <= 3 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "14", 
        vacc1_onset > 3 & vacc1_onset <= 6 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "15",
        vacc1_onset > 6 & vacc1_onset <= 9 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "16",
        vacc1_onset > 9 & vacc1_onset <= 13 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "17",
        vacc1_onset > 13 & vacc1_onset <= 20 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "18",
        vacc1_onset > 20 & vacc1_onset <= 27 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "19",
        vacc1_onset > 27 & vacc1_onset <= 34 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "20",
        vacc1_onset > 34 & vacc1_onset <= 41 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "21",
        vacc1_onset > 41 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "22",
        vacc2_onset >= 0 & vacc2_onset <= 3 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="Pfizer" ~ "23",
        vacc2_onset > 3 & vacc2_onset <= 6 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="Pfizer" ~ "24",
        vacc2_onset > 6 & vacc2_onset <= 13 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="Pfizer" ~ "25",
        vacc2_onset > 13 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="Pfizer" ~ "26"
      ),
      levels = c(0:26),
      labels = c("not vaccinated", "AZ 1 dose 0-3", "AZ 1 dose 4-6", "AZ 1 dose 7-9", "AZ 1 dose 10-13", "AZ 1 dose 14-20", "AZ 1 dose 21-27", "AZ 1 dose 28-34", "AZ 1 dose 35-41", "AZ 1 dose 42+", "AZ 2 dose 0-3", "AZ 2 dose 4-6", "AZ 2 dose 7-13", "AZ dose 2 14+", "PF 1 dose 0-3", "PF 1 dose 4-6", "PF 1 dose 7-9", "PF 1 dose 10-13", "PF 1 dose 14-20", "PF 1 dose 21-27", "PF 1 dose 28-34", "PF 1 dose 35-41", "PF dose 1 42+", "PF 2 dose 0-3", "PF 2 dose 4-6", "PF 2 dose 7-13", "PF dose 2 14+")
    )
  )

#Pfizer only - combined 
data %<>%
  mutate(
    vaccination_categories_Pfizer = factor(
      case_when(
        vaccine_status_TND_2levels_onset == "Not vaccinated" ~ "0",
        vaccination_categories == "PF 1 dose 0-3" | vaccination_categories == "PF 1 dose 4-6" | vaccination_categories == "PF 1 dose 7-9" | vaccination_categories == "PF 1 dose 10-13" ~ "1",
        vaccination_categories == "PF 1 dose 14-20" | vaccination_categories == "PF 1 dose 21-27" | vaccination_categories == "PF 1 dose 28-34" | vaccination_categories == "PF 1 dose 35-41" | vaccination_categories == "PF dose 1 42+" ~ "2",
        vaccination_categories == "PF 2 dose 0-3" | vaccination_categories == "PF 2 dose 4-6" | vaccination_categories == "PF 2 dose 7-13" | vaccination_categories == "PF dose 2 14+" ~ "3"
      ),
      levels = c(0:3),
      labels = c("not vaccinated", "PF 1 dose 0-13", "PF 1 dose 14+", "PF 2 dose")
    )
  )

#combine vaccination categories
data %<>%
  mutate(
    vaccination_categories_combined = factor(
      case_when(
        vaccine_status_TND_2levels_onset == "Not vaccinated" ~ "0",
        vaccination_categories == "AZ 1 dose 0-3" | vaccination_categories == "AZ 1 dose 4-6" | vaccination_categories == "AZ 1 dose 7-9" | vaccination_categories == "AZ 1 dose 10-13" ~ "1",
        vaccination_categories == "AZ 1 dose 14-20" | vaccination_categories == "AZ 1 dose 21-27" | vaccination_categories == "AZ 1 dose 28-34" | vaccination_categories == "AZ 1 dose 35-41" | vaccination_categories == "AZ 1 dose 42+" ~ "2",
        vaccination_categories == "AZ 2 dose 0-3" | vaccination_categories == "AZ 2 dose 4-6" | vaccination_categories == "AZ 2 dose 7-13" ~ "3",
        vaccination_categories == "AZ dose 2 14+" ~ "4",
        vaccination_categories == "PF 1 dose 0-3" | vaccination_categories == "PF 1 dose 4-6" | vaccination_categories == "PF 1 dose 7-9" | vaccination_categories == "PF 1 dose 10-13" ~ "5",
        vaccination_categories == "PF 1 dose 14-20" | vaccination_categories == "PF 1 dose 21-27" | vaccination_categories == "PF 1 dose 28-34" | vaccination_categories == "PF 1 dose 35-41" | vaccination_categories == "PF dose 1 42+" ~ "6",
        vaccination_categories == "PF 2 dose 0-3" | vaccination_categories == "PF 2 dose 4-6" | vaccination_categories == "PF 2 dose 7-13" ~ "7",
        vaccination_categories == "PF dose 2 14+" ~ "8"
      ),
      levels = c(0:8),
      labels = c("not vaccinated", "AZ 1 dose 0-13", "AZ 1 dose 14+", "AZ 2 dose 0-13", "AZ dose 2 14+", "PF 1 dose 0-13", "PF 1 dose 14+", "PF 2 dose 0-13", "PF dose 2 14+")
    )
  )

#dose 1 only  
data %<>%
  mutate(
    vaccination_categories_dose1 = factor(
      case_when(
        vaccine_status_TND_2levels_onset == "Not vaccinated" ~ "0",
        vaccination_categories == "AZ 1 dose 0-3" ~ "1",
        vaccination_categories == "AZ 1 dose 4-6" ~"2",
        vaccination_categories == "AZ 1 dose 7-9" ~ "3",
        vaccination_categories == "AZ 1 dose 10-13" ~ "4",
        vaccination_categories == "AZ 1 dose 14-20" ~ "5",
        vaccination_categories == "AZ 1 dose 21-27" ~ "6",
        vaccination_categories == "AZ 1 dose 28-34" ~ "7",
        vaccination_categories == "AZ 1 dose 35-41" | vaccination_categories == "AZ 1 dose 42+" ~ "8",
        vaccination_categories == "PF 1 dose 0-3" ~ "9",
        vaccination_categories == "PF 1 dose 4-6" ~ "10",
        vaccination_categories == "PF 1 dose 7-9" ~ "11",
        vaccination_categories == "PF 1 dose 10-13" ~ "12",
        vaccination_categories == "PF 1 dose 14-20" ~ "13",
        vaccination_categories == "PF 1 dose 21-27" ~ "14",
        vaccination_categories == "PF 1 dose 28-34" ~ "15",
        vaccination_categories == "PF 1 dose 35-41" | vaccination_categories == "PF dose 1 42+" ~ "16"
      ),
      levels = c(0:16),
      labels = c("not vaccinated", "AZ 1 dose 0-3", "AZ 1 dose 4-6", "AZ 1 dose 7-9", "AZ 1 dose 10-13", "AZ 1 dose 14-20", "AZ 1 dose 21-27", "AZ 1 dose 28-34", "AZ 1 dose 35+", "PF 1 dose 0-3", "PF 1 dose 4-6", "PF 1 dose 7-9", "PF 1 dose 10-13", "PF 1 dose 14-20", "PF 1 dose 21-27", "PF 1 dose 28-34", "PF 1 dose 35+")
    )
  )

#dose 1 combined
data %<>%
  mutate(
    vaccination_categories_dose1_combined = factor(
      case_when(
        vaccine_status_TND_2levels_onset == "Not vaccinated" ~ "0",
        vaccination_categories_dose1 == "AZ 1 dose 0-3" | vaccination_categories_dose1 == "AZ 1 dose 4-6" | vaccination_categories_dose1 == "AZ 1 dose 7-9" | vaccination_categories_dose1 == "AZ 1 dose 10-13" ~ "1",
        vaccination_categories_dose1 == "AZ 1 dose 14-20" | vaccination_categories_dose1 == "AZ 1 dose 21-27" | vaccination_categories_dose1 == "AZ 1 dose 28-34" | vaccination_categories_dose1 == "AZ 1 dose 35+" ~ "2",
        vaccination_categories_dose1 == "PF 1 dose 0-3" | vaccination_categories_dose1 == "PF 1 dose 4-6" | vaccination_categories_dose1 == "PF 1 dose 7-9" | vaccination_categories_dose1 == "PF 1 dose 10-13" ~ "3",
        vaccination_categories_dose1 == "PF 1 dose 14-20" | vaccination_categories_dose1 == "PF 1 dose 21-27" | vaccination_categories_dose1 == "PF 1 dose 28-34" | vaccination_categories_dose1 == "PF 1 dose 35+" ~ "4"
      ),
      levels = c(0:4),
      labels = c("not vaccinated", "AZ 1 dose 0-13", "AZ 1 dose 14+", "PF 0-13", "PF 14+")
    )
  )

#create variable: dob from TND
fixed_date <- "2021/03/31"

fixed_date = as.Date(fixed_date, format = "%Y/%m/%d")

data %<>%
  mutate(dob_tnd = fixed_date - (Age_Mar31 * 365.25)) 

data %<>%
  mutate(dob_tnd = as.Date(dob_tnd, format = "%Y/%m/%d"))

#create variable: care home questionnaire
data %<>%
  mutate(
    carehomeq2levels = factor(
      case_when(accomodation_type_1 == "Private home"| accomodation_type_1 == "Council/ housing association" | accomodation_type_1 == "Sheltered accommodation" | accomodation_type_1 == "Other" ~ "0",
                accomodation_type_1 == "Carehome/ Nursing home" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("not care home", "care home")
    )
  )

#create variable: month dose 1
dec20_dose1 <- data$vaccine_date_dose1 >= "2020-12-01" & data$vaccine_date_dose1 <= "2020-12-31"
jan21_dose1 <- data$vaccine_date_dose1 >= "2021-01-01" & data$vaccine_date_dose1 <= "2021-01-31"
feb21_dose1 <- data$vaccine_date_dose1 >= "2021-02-01" & data$vaccine_date_dose1 <= "2021-02-28"
mar21_dose1 <- data$vaccine_date_dose1 >= "2021-03-01" & data$vaccine_date_dose1 <= "2021-03-31"
apr21_dose1 <- data$vaccine_date_dose1 >= "2021-04-01" & data$vaccine_date_dose1 <= "2021-04-30"
may21_dose1 <- data$vaccine_date_dose1 >= "2021-05-01" & data$vaccine_date_dose1 <= "2021-05-31"

data %<>%
  mutate(
    month_dose1 = factor(
      case_when(
        dec20_dose1 == "TRUE" ~ "1",
        jan21_dose1 == "TRUE" ~ "2",
        feb21_dose1 == "TRUE" ~ "3",
        mar21_dose1 == "TRUE" ~ "4",
        apr21_dose1 == "TRUE" ~ "5",
        may21_dose1 == "TRUE" ~ "6"
      ),
      levels = c(1:6),
      labels = c("dec20", "jan21", "feb21", "mar21", "apr21", "may21")
    )
  )

#create variable: month dose 2
dec20_dose2 <- data$vaccine_date_dose2 >= "2020-12-01" & data$vaccine_date_dose2 <= "2020-12-31"
jan21_dose2 <- data$vaccine_date_dose2 >= "2021-01-01" & data$vaccine_date_dose2 <= "2021-01-31"
feb21_dose2 <- data$vaccine_date_dose2 >= "2021-02-01" & data$vaccine_date_dose2 <= "2021-02-28"
mar21_dose2 <- data$vaccine_date_dose2 >= "2021-03-01" & data$vaccine_date_dose2 <= "2021-03-31"
apr21_dose2 <- data$vaccine_date_dose2 >= "2021-04-01" & data$vaccine_date_dose2 <= "2021-04-30"
may21_dose2 <- data$vaccine_date_dose2 >= "2021-05-01" & data$vaccine_date_dose2 <= "2021-05-31"

data %<>%
  mutate(
    month_dose2 = factor(
      case_when(
        dec20_dose2 == "TRUE" ~ "1",
        jan21_dose2 == "TRUE" ~ "2",
        feb21_dose2 == "TRUE" ~ "3",
        mar21_dose2 == "TRUE" ~ "4",
        apr21_dose2 == "TRUE" ~ "5",
        may21_dose2 == "TRUE" ~ "6"
      ),
      levels = c(1:6),
      labels = c("dec20", "jan21", "feb21", "mar21", "apr21", "may21")
    )
  )

#how many individuals completed the questionnaire
#addmargins(table(Qneg$idcompleted)) 

#df = table(QTND$idcompleted)
#prop.table(df)*100

#split data into respondents and non-respondents 
datares <- subset(data, idcompleted=="completed")

#split the data into positive test and no positive test
Qneg <- subset(data, pos_dec7=="No previous positive test")
Qpos <- subset(data, pos_dec7=="Positive previous test")

#split the data into respondents and non-respondents 
QTNDres <- subset(Qneg, idcompleted=="completed")
QTNDnon <- subset(Qneg, idcompleted=="notcompleted")

##EXPOSURE MISCLASSIFICATION
####comparing variables in questionnaire and TND - dates

#date dose 1
Date_difference_dose1 <- difftime(QTNDres$vaccine_date_dose1,QTNDres$dose1_date2,units=c("days"))
#assuming that vaccine date from the questionnaire is the earlier time

QTNDres %<>% 
  mutate(
    Date_difference_dose1 = as.numeric(Date_difference_dose1))

QTNDres %>%
  get_summary_stats(Date_difference_dose1, show = c("mean", "sd", "median", "iqr", "min", "max", "q1", "q2"))

hist(QTNDres$Date_difference_dose1, col="#74a9cf", main="Difference between questionnaire and NIMS vaccination dose 1 date", xlab="Days", xlim=c(-100,100))

#QTNDres %<>%
mutate(
  Date_differce_dose1_q_vs_TND = factor(
    case_when(
      Date_difference_dose1 < 0 ~ '0',
      Date_difference_dose1 == 0 ~ '1',
      Date_difference_dose1 > 0 ~ '2'
    ),
    levels = c(0, 1, 2),
    labels = c("TND dose 1 earlier", "Date the same", "Questionnaire dose 1 date earlier")
  )
)

#QTNDres %>% tabyl(Date_differce_dose1_q_vs_TND, show_na = FALSE)

#Describe those with different dates 
#data of those with missing dates
QTNDres_diffvacc1 <- subset(QTNDres, vaccine_date_dose1 != dose1_date2 & !is.na (vaccine_date_dose1) & !is.na (dose1_date2))

#describe this sample
QTNDres_diffvacc1 %>%
  select(Age_Mar31, age5, gender, eth2, nhsregion, imd5, week_onset, week_test, carehome, res, CEV_flag_2611, vaccine_status_TND_onset, pos_dec7, flu_vacc) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    percent = "col", #change this to "col" or remove this row for column percentages
    label = list(
      Age_Mar31 ~ "Age (years)",
      age5 ~ "Age group (years)",
      gender ~ "Gender",
      eth2 ~ "Ethnicity",
      nhsregion ~ "Geographical region",
      imd5 ~ "IMD",
      week_onset ~ "Week COVID-19 symptom onset",
      week_test ~ "Week COVID-19 test",
      carehome ~ "Care home status",
      res ~ "Test result",
      CEV_flag_2611 ~ "CEV NIMS",
      vaccine_status_TND_onset ~ "Vaccine status before COVID-19 onset",
      pos_dec7 ~ "Positive test before 7 Dec 2020",
      flu_vacc ~ "Flu vaccination"),
    missing_text = "Missing"
  ) %>%
  add_p()

#date dose 2
Date_difference_dose2 <-difftime(QTNDres$vaccine_date_dose2, QTNDres$dose2_date2,units=c("days"))
#assuming that vaccination dose from the questionnaire is earlier

QTNDres %<>% 
  mutate(
    Date_difference_dose2 = as.numeric(Date_difference_dose2))

QTNDres %>%
  get_summary_stats(Date_difference_dose2, show = c("mean", "sd", "median", "iqr", "min", "max", "q1", "q2"))

hist(QTNDres$Date_difference_dose2, col="#74a9cf", main="Difference between questionnaire and NIMS vaccination dose 2 date", xlab="Days", xlim=c(-100,100))

#QTNDres %<>%
mutate(
  Date_differce_dose2_q_vs_TND = factor(
    case_when(
      Date_difference_dose2 < 0 ~ '0',
      Date_difference_dose2 == 0 ~ '1',
      Date_difference_dose2 > 0 ~ '2'
    ),
    levels = c(0, 1, 2),
    labels = c("TND dose 2 earlier", "zero", "Questionnaire dose 2 earlier")
  )
)

#QTNDres %>% tabyl(Date_differce_dose2_q_vs_TND, show_na = FALSE)

#Vaccination status with vaccination date from the questionnaire
#first create vaccination dates that use the questionnaire date (dose1_date2), but if missing uses TND - dose 1
QTNDres$date_vacc1_combine <- with(QTNDres,ifelse(is.na(dose1_date2),vaccine_date_dose1, dose1_date2))

QTNDres %<>%
  mutate(date_vacc1_combine = as.Date(date_vacc1_combine))

#dose 2
QTNDres$date_vacc2_combine <- with(QTNDres,ifelse(is.na(dose2_date2),vaccine_date_dose2, dose2_date2))

QTNDres %<>%
  mutate(date_vacc2_combine = as.Date(date_vacc2_combine))

#then need to create the vaccination categories
QTNDres %<>%
  mutate(
    vaccination_categories_vaccq_pfizer = factor( 
      case_when(
        vaccine_status_TND_qvacc == "No doses" ~ "0", 
        vacc1_onset >= 0 & vacc1_onset <= 13 & !is.na (date_vacc1_combine) & is.na (date_vacc2_combine) & manufacturer_dose1=="Pfizer" ~ "1",
        vacc1_onset > 13 & !is.na (date_vacc1_combine) & is.na (date_vacc2_combine) & manufacturer_dose1=="Pfizer" ~ "2",
        vacc2_onset >= 0 & vacc2_onset <= 13 & !is.na (date_vacc1_combine) & !is.na (date_vacc2_combine) & manufacturer_dose2=="Pfizer" ~ "3",
        vacc2_onset > 13 & !is.na (date_vacc1_combine) & !is.na (date_vacc2_combine) & manufacturer_dose2=="Pfizer" ~ "4"
      ),
      levels = c(0:4),
      labels = c("not vaccinated", "PF 1 dose 0-13", "PF 1 dose 14+", "PF 2 dose 0-13", "PF dose 2 14+")
    )
  )

#Updated vaccination status
m <- !is.na (QTNDres$date_vacc1_combine)
n <- !is.na (QTNDres$date_vacc2_combine)
o <- is.na (QTNDres$date_vacc1_combine)
p <- is.na (QTNDres$date_vacc2_combine)

QTNDres %<>%
  mutate(
    vaccine_status_TND_qvacc = factor(
      case_when(
        o == "TRUE" & p == "TRUE" ~ "1",
        m == "TRUE" & p == "TRUE" ~ "2",
        m == "TRUE" & n == "TRUE" ~ "3"
      ),
      levels = c(1, 2, 3),
      labels = c("No doses", "One dose", "Two doses")
    )
  )

#QTNDres %>% tabyl(vaccine_status_TND_qonset)
#QTNDres %>% tabyl(vaccine_status_TND_onset)

#Describe individuals with different dose 2
#data of those with missing dates
QTNDres_diffvacc2 <- subset(QTNDres, vaccine_date_dose2 != dose2_date2 & !is.na (vaccine_date_dose2) & !is.na (dose2_date2))

#describe this sample
QTNDres_diffvacc2 %>%
  select(Age_Mar31, age5, gender, eth2, nhsregion, imd5, week_onset, week_test, carehome, res, CEV_flag_2611, vaccine_status_TND_onset, pos_dec7, flu_vacc) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    percent = "col", #change this to "col" or remove this row for column percentages
    label = list(
      idcompleted ~ "Completed questionnaire",
      Age_Mar31 ~ "Age (years)",
      age5 ~ "Age group (years)",
      gender ~ "Gender",
      eth2 ~ "Ethnicity",
      nhsregion ~ "Geographical region",
      imd5 ~ "IMD",
      week_onset ~ "Week COVID-19 symptom onset",
      week_test ~ "Week COVID-19 test",
      carehome ~ "Care home status",
      res ~ "Test result",
      CEV_flag_2611 ~ "CEV NIMS",
      vaccine_status_TND_onset ~ "Vaccine status before COVID-19 onset",
      pos_dec7 ~ "Positive test before 7 Dec 2020",
      flu_vacc ~ "Flu vaccination"),
    missing_text = "Missing"
  ) %>%
  add_p()

#OUTCOME MISCLASSIFICATION
#QTNDres %>% 
tabyl(confirm_symptoms2, symptoms) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  adorn_title(
    row_name = "Confirm symptoms Questionniare",
    col_name = "Confirm symptoms TND"
  )

##Key confounders by asymptomatic status  
QTNDres %>%
  select(confirm_symptoms2, no_ppl_household, accomodation_type_1, flu_vacc, frail, cev2, CEV_flag_2611, CHD, CKD, CLD, CRD, asthma_med, cancer, organ_bone_transplant, HIV_immuno, immuno_med, seizure_dis, CND, asplenia, BMI_40, None_above_conditions) %>%
  tbl_summary(
    by = confirm_symptoms2,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      no_ppl_household ~ "number per household",
      accomodation_type_1 ~ "accomodation type",
      flu_vacc ~ "flu vaccination",
      frail ~ "frail",
      cev2 ~ "CEV questionnaire",
      CEV_flag_2611 ~ "CEV NIMS",
      CHD ~ "CHD",
      CKD ~ "CKD",
      CLD ~ "CLD",
      CRD ~ "CRD",
      asthma_med ~ "asthma with medications",
      cancer ~ "cancer",
      organ_bone_transplant ~ "organ or bone transplant",
      HIV_immuno ~ "HIV or immunodeficiency",
      immuno_med ~ "immunosuppressive medications",
      seizure_dis ~ "seizure disorder",
      CND ~ "chronic neurological disease",
      asplenia ~ "asplenia",
      BMI_40 ~ "BMI 40 or over",
      None_above_conditions ~ "none above conditions"),
    missing_text = "Missing"
  ) %>%
  add_p()

#split the data into vaccinated and non-vaccinated
QTNDres_vacc <- subset(QTNDres, vaccine_status_TND_2levels_onset=="Vaccinated")
QTNDres_nonvacc <- subset(QTNDres, vaccine_status_TND_2levels_onset=="Not vaccinated")

#Describe confounders by symptomatic status by vacc status
QTNDres_vacc %>%
  select(confirm_symptoms2, no_ppl_household, accomodation_type_1, flu_vacc, frail, cev2, CEV_flag_2611, CHD, CKD, CLD, CRD, asthma_med, cancer, organ_bone_transplant, HIV_immuno, immuno_med, seizure_dis, CND, asplenia, BMI_40, None_above_conditions) %>%
  tbl_summary(
    by = confirm_symptoms2,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      no_ppl_household ~ "number per household",
      accomodation_type_1 ~ "accomodation type",
      flu_vacc ~ "flu vaccination",
      frail ~ "frail",
      cev2 ~ "CEV questionnaire",
      CEV_flag_2611 ~ "CEV NIMS",
      CHD ~ "CHD",
      CKD ~ "CKD",
      CLD ~ "CLD",
      CRD ~ "CRD",
      asthma_med ~ "asthma with medications",
      cancer ~ "cancer",
      organ_bone_transplant ~ "organ or bone transplant",
      HIV_immuno ~ "HIV or immunodeficiency",
      immuno_med ~ "immunosuppressive medications",
      seizure_dis ~ "seizure disorder",
      CND ~ "chronic neurological disease",
      asplenia ~ "asplenia",
      BMI_40 ~ "BMI 40 or over",
      None_above_conditions ~ "none above conditions"),
    missing_text = "Missing"
  ) %>%
  add_p()

QTNDres_nonvacc %>%
  select(confirm_symptoms2, no_ppl_household, accomodation_type_1, flu_vacc, frail, cev2, CEV_flag_2611, CHD, CKD, CLD, CRD, asthma_med, cancer, organ_bone_transplant, HIV_immuno, immuno_med, seizure_dis, CND, asplenia, BMI_40, None_above_conditions) %>%
  tbl_summary(
    by = confirm_symptoms2,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      no_ppl_household ~ "number per household",
      accomodation_type_1 ~ "accomodation type",
      flu_vacc ~ "flu vaccination",
      frail ~ "frail",
      cev2 ~ "CEV questionnaire",
      CEV_flag_2611 ~ "CEV NIMS",
      CHD ~ "CHD",
      CKD ~ "CKD",
      CLD ~ "CLD",
      CRD ~ "CRD",
      asthma_med ~ "asthma with medications",
      cancer ~ "cancer",
      organ_bone_transplant ~ "organ or bone transplant",
      HIV_immuno ~ "HIV or immunodeficiency",
      immuno_med ~ "immunosuppressive medications",
      seizure_dis ~ "seizure disorder",
      CND ~ "chronic neurological disease",
      asplenia ~ "asplenia",
      BMI_40 ~ "BMI 40 or over",
      None_above_conditions ~ "none above conditions"),
    missing_text = "Missing"
  ) %>%
  add_p()

##COVID-19 symptom onset date 

#create a column that combines onset dates with date_symt_onset taking precedence
QTNDres$date_onset_combine <- with(QTNDres,ifelse(is.na(date_symt_onset),sgss_symptom_date, date_symt_onset))

QTNDres %<>%
  mutate(
    date_onset_combine = as.Date(date_onset_combine, origin = "1970-01-01"))

QTNDres %<>%
  mutate(date_onset_combine = as.Date(date_onset_combine))

#date difference
Date_difference_onset <-difftime(QTNDres$sgss_symptom_date, QTNDres$date_onset_combine,units=c("days"))
#assuming that questionnaire is earlier

QTNDres %<>% 
  mutate(
    Date_difference_onset = (as.numeric(Date_difference_onset)))

#QTNDres %<>%
mutate(
  Date_differce_onset_q_vs_TND = factor(
    case_when(
      Date_difference_onset < 0 ~ '0',
      Date_difference_onset == 0 ~ '1',
      Date_difference_onset > 0 ~ '2'
    ),
    levels = c(0, 1, 2),
    labels = c("TND onset earlier", "zero", "Questionnaire onset earlier")
  )
)

#QTNDres %>% tabyl(Date_differce_onset_q_vs_TND, show_na = FALSE)

##repeat using questionnaire onset 
#first dose
#QTNDres %<>%
mutate(
  qonset_after_vacc = factor(
    case_when(
      vaccine_date_dose1 > date_onset_combine ~ "0",
      vaccine_date_dose1 <= date_onset_combine ~ "1"
    ),
    levels = c(0, 1),
    labels = c("vaccine after onset", "vaccine before onset")
  )
)

#second dose
#QTNDres %<>%
mutate(
  qonset_after_vacc_dose_2 = factor(
    case_when(
      vaccine_date_dose2 > date_onset_combine ~ "0",
      vaccine_date_dose2 <= date_onset_combine ~ "1"
    ),
    levels = c(0, 1),
    labels = c("vaccine after onset", "vaccine before onset")
  )
)

#VACCINATION STATUS - USING UPDATED ONSET DATE
#dose 1
QTNDres$vaccine_date_dose1_4 = QTNDres$vaccine_date_dose1

x <- QTNDres$vaccine_date_dose1_4 > QTNDres$date_onset_combine

QTNDres$vaccine_date_dose1_4[x] <- NA

#dose 2
QTNDres$vaccine_date_dose2_4 = QTNDres$vaccine_date_dose2

x <- QTNDres$vaccine_date_dose2_4 > QTNDres$date_onset_combine

QTNDres$vaccine_date_dose2_4[x] <- NA

m <- !is.na (QTNDres$vaccine_date_dose1_4)
n <- !is.na (QTNDres$vaccine_date_dose2_4)
o <- is.na (QTNDres$vaccine_date_dose1_4)
p <- is.na (QTNDres$vaccine_date_dose2_4)

QTNDres %<>%
  mutate(
    vaccine_status_TND_qonset = factor(
      case_when(
        o == "TRUE" & p == "TRUE" ~ "1",
        m == "TRUE" & p == "TRUE" ~ "2",
        m == "TRUE" & n == "TRUE" ~ "3"
      ),
      levels = c(1, 2, 3),
      labels = c("No doses", "One dose", "Two doses")
    )
  )

#QTNDres %>% tabyl(vaccine_status_TND_qonset)
#QTNDres %>% tabyl(vaccine_status_TND_onset)

#UNMEASURED CONFOUNDERS
#COVID-19 risk factors, CEV and frailty by vaccination status 
#QTNDres %>%
select(vaccine_status_TND_2levels_onset, no_ppl_household, accomodation_type_1, flu_vacc, Frail, cev2, CEV_flag_2611, CHD, CKD, CLD, CRD, asthma_med, cancer, organ_bone_transplant, HIV_immuno, immuno_med, immuno_all, seizure_dis, CND, asplenia, BMI_40, None_above_conditions) %>%
  tbl_summary(
    by = vaccine_status_TND_2levels_onset,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      no_ppl_household ~ "number per household",
      accomodation_type_1 ~ "accomodation type",
      flu_vacc ~ "flu vaccination",
      Frail ~ "frail",
      cev2 ~ "CEV questionnaire",
      CEV_flag_2611 ~ "CEV NIMS",
      CHD ~ "CHD",
      CKD ~ "CKD",
      CLD ~ "CLD",
      CRD ~ "CRD",
      asthma_med ~ "asthma with medications",
      cancer ~ "cancer",
      HIV_immuno ~ "HIV or immunodeficiency",
      organ_bone_transplant ~ "organ or bone transplant",
      immuno_med ~ "immunosuppressive medications",
      immuno_all ~ "all immunosuppressive conditions",
      seizure_dis ~ "seizure disorder",
      CND ~ "chronic neurological disease",
      asplenia ~ "asplenia",
      BMI_40 ~ "BMI 40 or over",
      None_above_conditions ~ "none above conditions"),
    missing_text = "Missing"
  ) %>%
  add_p()

#COVID-19 risk factors, CEV and frailty by test result 
#QTNDres %>%
select(res, no_ppl_household, accomodation_type_1, flu_vacc, Frail, cev2, CEV_flag_2611, CHD, CKD, CLD, CRD, asthma_med, cancer, immuno_all, organ_bone_transplant, HIV_immuno, immuno_med, seizure_dis, CND, asplenia, BMI_40, None_above_conditions) %>%
  tbl_summary(
    by = res,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      no_ppl_household ~ "number per household",
      accomodation_type_1 ~ "accomodation type",
      flu_vacc ~ "flu vaccination",
      Frail ~ "frail",
      cev2 ~ "CEV questionnaire",
      CEV_flag_2611 ~ "CEV NIMS",
      CHD ~ "CHD",
      CKD ~ "CKD",
      CLD ~ "CLD",
      CRD ~ "CRD",
      asthma_med ~ "asthma with medications",
      cancer ~ "cancer",
      immuno_all ~ "all immunosuppressive conditions",
      organ_bone_transplant ~ "organ or bone transplant",
      HIV_immuno ~ "HIV or immunodeficiency",
      immuno_med ~ "immunosuppressive medications",
      seizure_dis ~ "seizure disorder",
      CND ~ "chronic neurological disease",
      asplenia ~ "asplenia",
      BMI_40 ~ "BMI 40 or over",
      None_above_conditions ~ "none above conditions"),
    missing_text = "Missing"
  ) %>%
  add_p()

#HEALTHY VACCINEE BIAS FROM VACCINE DELAY
##using dosage variable in questionnaire
#QTNDres %>%
select(doses2levels, dur_invite) %>%
  tbl_summary(
    by = doses2levels,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      dur_invite ~ "Duration from invite"),
    missing_text = "Missing"
  )

##amongst those that delayed the reason for delay
#first describe by duration from invite 
#QTNDres %>%
select(dur_invite, not_aware_eleg, no_app_av, pref_wait, delayed_covid, isolating, vacc_time, vacc_other) %>%
  tbl_summary(
    by = dur_invite,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      not_aware_eleg ~ "Was not aware I was elegible",
      no_app_av ~ "No appointments available",
      pref_wait ~ "Prefer to wait to be vaccinated",
      delayed_covid ~ "I delayed getting vaccinated because I had COVID-19",
      isolating ~ "I was isolating",
      vacc_time ~ "I did not have time",
      vacc_other ~ "Other reasons"
    ),
    missing_text = "Missing"
  )

#amongst those that have not been vaccinated, the reason for not being vaccinated
#by TND vaccination status before COVID-19 test, stratified by test result
#split the data
QTNDres_pos <- subset(QTNDres, res=="positive")
QTNDres_neg <- subset(QTNDres, res=="negative")

#amongst cases
#QTNDres_pos %>%
select(vaccine_status_TND_2levels_onset, not_vacc_not_called, not_vacc_elig, not_vacc_app, not_vacc_prefer_not, not_vacc_soon, not_vacc_unwell, not_vacc_isol, not_vacc_time, not_vacc_other) %>%
  tbl_summary(
    by = vaccine_status_TND_2levels_onset,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      not_vacc_not_called ~ "Have not been called for a vaccine",
      not_vacc_elig ~ "I was not aware I was elegible",
      not_vacc_app ~ "There were no appointments available",
      not_vacc_prefer_not ~ "I would prefer not to get vaccinated at the moment",
      not_vacc_soon ~ "I expect to get vaccinated soon",
      not_vacc_unwell ~ "I have been unwell or have had COVID-19 infection",
      not_vacc_isol ~ "I am isolating",
      not_vacc_time ~ "I have not had time",
      not_vacc_other ~ "Other"
    ),
    missing_text = "Missing"
  )

#amongst controls
#QTNDres_neg %>%
select(vaccine_status_TND_2levels_onset, not_vacc_not_called, not_vacc_elig, not_vacc_app, not_vacc_prefer_not, not_vacc_soon, not_vacc_unwell, not_vacc_isol, not_vacc_time, not_vacc_other) %>%
  tbl_summary(
    by = vaccine_status_TND_2levels_onset,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      not_vacc_not_called ~ "Have not been called for a vaccine",
      not_vacc_elig ~ "I was not aware I was elegible",
      not_vacc_app ~ "There were no appointments available",
      not_vacc_prefer_not ~ "I would prefer not to get vaccinated at the moment",
      not_vacc_soon ~ "I expect to get vaccinated soon",
      not_vacc_unwell ~ "I have been unwell or have had COVID-19 infection",
      not_vacc_isol ~ "I am isolating",
      not_vacc_time ~ "I have not had time",
      not_vacc_other ~ "Other"
    ),
    missing_text = "Missing"
  )

#by questionnaire number of doses
#QTNDres %>%
select(doses2levels, not_vacc_not_called, not_vacc_elig, not_vacc_app, not_vacc_prefer_not, not_vacc_soon, not_vacc_unwell, not_vacc_isol, not_vacc_time, not_vacc_other) %>%
  tbl_summary(
    by = doses2levels,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      not_vacc_not_called ~ "Have not been called for a vaccine",
      not_vacc_elig ~ "I was not aware I was elegible",
      not_vacc_app ~ "There were no appointments available",
      not_vacc_prefer_not ~ "I would prefer not to get vaccinated at the moment",
      not_vacc_soon ~ "I expect to get vaccinated soon",
      not_vacc_unwell ~ "I have been unwell or have had COVID-19 infection",
      not_vacc_isol ~ "I am isolating",
      not_vacc_time ~ "I have not had time",
      not_vacc_other ~ "Other"
    ),
    missing_text = "Missing"
  )

#RISKIER BEHAVIOUR AFTER VACCINATION 
#mixing after first dose
#by TND vaccination status - end of study period
QTNDres %>%
  select(vaccine_status_TND_2levels, mixing_dose1) %>%
  tbl_summary(
    by = vaccine_status_TND_2levels,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose1 ~ "Mixing after dose 1"
    ),
    missing_text = "Missing"
  )

#by questionniare doses
QTNDres %>%
  select(doses2levels, mixing_dose1) %>%
  tbl_summary(
    by = doses2levels,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose1 ~ "Mixing after dose 1"
    ),
    missing_text = "Missing"
  )

##there was no one that reported they had no doses and answered this question. As above, majority of individuals (65%) reported that they mixed the same amount, followed by mixing less (30%), followed by mixing more (5.4%).

#by test result 
QTNDres %>%
  select(res, mixing_dose1) %>%
  tbl_summary(
    by = res,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose1 ~ "Mixing after dose 1"
    ),
    missing_text = "Missing"
  )

#mixing dose 2
#by TND vaccination status
QTNDres %>%
  select(vaccine_status_TND_2levels, mixing_dose2) %>%
  tbl_summary(
    by = vaccine_status_TND_2levels,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose2 ~ "Mixing after dose 2"
    ),
    missing_text = "Missing"
  )

#by questionnaire doses
QTNDres %>%
  select(doses, mixing_dose2) %>%
  tbl_summary(
    by = doses,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose2 ~ "Mixing after dose 2"
    ),
    missing_text = "Missing"
  )

#by test result 
QTNDres %>%
  select(res, mixing_dose2) %>%
  tbl_summary(
    by = res,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose2 ~ "Mixing after dose 2"
    ),
    missing_text = "Missing"
  )

#stratified by month of vaccination
#dose 1
QTNDres %>%
  select(month_dose1, mixing_dose1) %>%
  tbl_summary(
    by = month_dose1,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose1 ~ "Mixing after dose 1"
    ),
    missing_text = "Missing"
  )

#dose 2
QTNDres %>%
  select(month_dose1, mixing_dose2) %>%
  tbl_summary(
    by = month_dose1,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      mixing_dose2 ~ "Mixing after dose 2"
    ),
    missing_text = "Missing"
  )

#riskier behaviours after vaccination
QTNDres %<>% 
  mutate(
    vacc1_onset = as.numeric(vacc1_onset),
    vacc2_onset = as.numeric(vacc2_onset)
  )

#first create a variable - those that had onset within 2 weeks of vaccination 
#first dose
QTNDres %<>%
  mutate(
    onset_vaccination_dose1_2weeks_orless = factor(
      case_when(
        vacc1_onset > 14 ~ '0',
        vacc1_onset >= 0 & vacc1_onset <= 14 ~ '1'
      ),
      levels = c(0, 1),
      labels = c("more than 2 weeks", "2 weeks or less")
    )
  )

#second dose
QTNDres %<>%
  mutate(
    onset_vaccination_dose2_2weeks_orless = factor(
      case_when(
        vacc2_onset > 14 ~ '0',
        vacc2_onset >= 0 & vacc2_onset <=14 ~ '1'
      ),
      levels = c(0, 1),
      labels = c("more than 2 weeks", "2 weeks or less")
    )
  )

#create a variable for riskier behaviour before onset amongst those that were vaccinated
QTNDres %<>% mutate(QTNDres, riskier_behaviour = ifelse(week_car == "car with someone outside house" | week_did_you_go == "indoors with others outside house" | week_public == "public transport", "1", "0"))

QTNDres %<>%
  mutate(
    riskier_behaviour = factor(
      riskier_behaviour,
      levels = c("0", "1"),
      labels = c("not riskier behaviour", "riskier behaviour")
    ))

#riskier behaviour amongst those that had onset within 2 weeks of vaccination - first dose
QTNDres %>% 
  tabyl(onset_vaccination_dose1_2weeks_orless, riskier_behaviour, show_na = FALSE) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  adorn_title(
    row_name = "Onset to vaccination less than 2 weeks first dose",
    col_name = "Riskier behaviour"
  )

#riskier behaviour amongst those that had a test within 2 weeks of vaccination - second dose
QTNDres %>% 
  tabyl(onset_vaccination_dose2_2weeks_orless, riskier_behaviour, show_na = FALSE) %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front") %>%
  adorn_title(
    row_name = "Onset to vaccination less than 2 weeks second dose",
    col_name = "Riskier behaviour"
  )

#stratify by month of responding to the questionnaire - first dose only 
QTNDres %>% 
  tabyl(onset_vaccination_dose1_2weeks_orless, riskier_behaviour, month_questionnaire, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title

#VACCIANTION ITSELF ASSOCIATED WITH COVID  - REMOVED SEE PREVIOUS VERSION IF YOU NEED
#create a new variable - onset within 2 weeks after vaccination - first dose
lessthan14 <- QTNDres$vacc1_onset >= "0" & QTNDres$vacc1_onset <= "14"
greaterthan14 <- QTNDres$vacc1_onset > "14" 

QTNDres %<>%
  mutate(
    onset_2wks1dose = factor(
      case_when(
        res == "Positive" & lessthan14 == "TRUE" ~ "0",
        res == "Positive" & greaterthan14 == "TRUE" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("less than 2 weeks", "more than 2 weeks")
    )
  )

#stratify positive test by transport to first dose
QTNDres %>%
  select(onset_2wks1dose, walk_cyc, car_alone_dose1, car_diff_dose1, public_dose1, transport_other_dose1) %>%
  tbl_summary(
    by = onset_2wks1dose,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      walk_cyc ~ "walked/cycled",
      car_alone_dose1 ~ "car alone with members of household",
      car_diff_dose1 ~ "car with members outside household",
      public_dose1 ~ "public transport",
      transport_other_dose1 ~ "other"
    ),
    missing_text = "Missing"
  )

#second dose
lessthan14_dose2 <- QTNDres$vacc2_onset >= "0" & QTNDres$vacc1_onset <= "14"
greaterthan14_dose2 <- QTNDres$vacc2_onset > "14" 

QTNDres %<>%
  mutate(
    pos_2wks2dose = factor(
      case_when(
        res == "positive" & lessthan14_dose2 == "TRUE" ~ "0",
        res == "positive" & greaterthan14_dose2 == "TRUE" ~ "1"
      ),
      levels = c(0, 1),
      labels = c("less than 2 weeks", "more than 2 weeks")
    )
  )

#stratify positive test by transport to first dose
QTNDres %>%
  select(pos_2wks2dose, transport_dose2) %>%
  tbl_summary(
    by = pos_2wks2dose,
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 1,
    type = all_categorical() ~ "categorical",
    label = list(
      transport_dose2 ~ "transport second dose"
    ),
    missing_text = "Missing"
  )

#MODELLING
##UNMEASURED CONFOUNDERS

#adjusted for CEV, household type and size
lr34 <- glm(res ~ vaccination_categories_Pfizer + age5 + gender + eth2 + nhsregion2 + imd5 + carehome + week_onset + no_ppl_household + cev2 + accomodation_type_1, data = QTNDres, family = binomial)

t3 <- odds.ratio(lr34)

##EXPOSURE MISCLASSIFICATION = RUN THIS 
#run model with vaccination date from the questionnaire 
lr23 <- glm(res ~ vaccination_categories_vaccq_pfizer + age5 + gender + eth2 + nhsregion2 + imd5 + carehome + week_onset, data = QTNDres, family = binomial)

t8 <- odds.ratio(lr23)

##OUTCOME MISCLASSIFICATION FROM SYMPTOMATIC STATUS 
#exclude those that said they were asymptomatic
QTNDres_symptomatic <- subset(QTNDres, confirm_symptoms2 == "had symptoms")

#re-run adjusted VE estimates in this subset (without adjusting for unmeasured confounders)
lr23 <- glm(res ~ vaccination_categories_Pfizer + age5 + gender + eth2 + nhsregion2 + imd5 + carehome + week_onset, data = QTNDres_symptomatic, family = binomial)

t8 <- odds.ratio(lr23)

##OUTCOME MISCLASSIFICATION FROM SYMPTOM ONSET DATE: RUN THIS 
#first need to create new variable vaccination to onset date
#dose 1 
vacc1_onsetq <- difftime(QTNDres$date_onset_combine,QTNDres$vaccine_date_dose1,units=c("days"))

QTNDres %<>% 
  mutate(
    vacc1_onsetq = as.numeric(vacc1_onsetq))

#dose 2
vacc2_onsetq <- difftime(QTNDres$date_onset_combine,QTNDres$vaccine_date_dose2,units=c("days"))

QTNDres %<>% 
  mutate(
    vacc2_onsetq = as.numeric(vacc2_onsetq))

#then need to create vaccine stratified by time to onset
QTNDres %<>%
  mutate(
    vaccination_categories_onsetq_pfizer = factor(
      case_when(
        vaccine_status_TND_qonset == "No doses" ~ "0",
        vacc1_onsetq >= 0 & vacc1_onsetq <= 13 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "1",
        vacc1_onsetq > 13 & !is.na (vaccine_date_dose1_3) & is.na (vaccine_date_dose2_3) & manufacturer_dose1=="Pfizer" ~ "2",
        vacc2_onsetq >= 0 & vacc2_onsetq <= 13 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="Pfizer" ~ "3",
        vacc2_onsetq > 13 & !is.na (vaccine_date_dose1_3) & !is.na (vaccine_date_dose2_3) & manufacturer_dose2=="Pfizer" ~ "4"
      ),
      levels = c(0:4),
      labels = c("not vaccinated", "PF 1 dose 0-13", "PF 1 dose 14+", "PF 2 dose 0-13", "PF dose 2 14+")
    )
  )

#then run the model
lr_newonset <- glm(res ~ vaccination_categories_onsetq_pfizer + age5 + gender + eth2 + nhsregion2 + imd5 + carehome + week_onset, data = QTNDres, family = binomial)

t_newonset <- odds.ratio(lr_newonset)

##VACCINE DEFFERAL: 
#exclude everyone that said they delayed because they had covid-19 / covid-19 like symptoms 
#check first that delayed covid varible includes everyone in questionnaire 
summary(QTNDres$delayed_covid)

#first split the data
QTNDres_notdelaycovid <- subset(QTNDres, delayed_covid=="did not delay because I had COVID-19 or symptoms")

#then run model excluding those that reported they delayed 
lr_excludedelayed <- glm(res ~ vaccination_categories_combined + age5 + gender + eth2 + nhsregion2 + imd5 + carehome + week_onset, data = QTNDres_notdelaycovid, family = binomial)

t_excludedelayed <- odds.ratio(lr_excludedelayed)

#FULL MODEL 
#create new population symptomatic and did not delay
QTNDres_symptomatic_nodelay <- subset(QTNDres, confirm_symptoms2 == "had symptoms" & delayed_covid=="did not delay because I had COVID-19 or symptoms")

#give a population count
QTNDres_symptomatic_nodelay %>% tabyl()

#then create vaccination categories using questionnaire vaccination and onset dates 
QTNDres %<>%
  mutate(
    vaccination_categories_vaccq_onsetq_pfizer = factor( 
      case_when(
        vaccine_status_TND_qvacc == "No doses" ~ "0", 
        vacc1_onsetq >= 0 & vacc1_onset <= 13 & !is.na (date_vacc1_combine) & is.na (date_vacc2_combine) & manufacturer_dose1=="Pfizer" ~ "1",
        vacc1_onsetq > 13 & !is.na (date_vacc1_combine) & is.na (date_vacc2_combine) & manufacturer_dose1=="Pfizer" ~ "2",
        vacc2_onsetq >= 0 & vacc2_onset <= 13 & !is.na (date_vacc1_combine) & !is.na (date_vacc2_combine) & manufacturer_dose2=="Pfizer" ~ "3",
        vacc2_onsetq > 13 & !is.na (date_vacc1_combine) & !is.na (date_vacc2_combine) & manufacturer_dose2=="Pfizer" ~ "4"
      ),
      levels = c(0:4),
      labels = c("not vaccinated", "PF 1 dose 0-13", "PF 1 dose 14+", "PF 2 dose 0-13", "PF dose 2 14+")
    )
  )

#run the models including those that are symptomatic, did not delay, with new vaccination and onset date and adjusting for confounders 
lr19 <- glm(res ~ vaccination_categories_vaccq_onsetq_pfizer + age5 + gender + eth2 + nhsregion2 + imd5 + carehome + week_onset + no_ppl_household + cev2 + accomodation_type_1, data = QTNDres_symptomatic_nodelay, family = binomial)

t6 <- odds.ratio(lr19)

##Riskier behvaiour after vaccination 
#sub-set to population that had their vaccine before onset 
QTNDres_vaccine_before_onset <- subset(QTNDres, onset_after_vacc=="vaccine before onset")

#Association between mixing more and COVID-19 test - dose 1
lr26 <- glm(res ~ mixing_dose1 + age5 + gender + eth1 + CEV_flag_2611, data = QTNDres_vaccine_before_onset, family = binomial)

odds.ratio(lr26)

#Association between mixing more and COVID-19 test - dose 2
lr27 <- glm(res ~ mixing_dose2 + age5 + gender + eth1 + CEV_flag_2611, data = QTNDres, family = binomial)

odds.ratio(lr27)

#Vaccination itself associated with COVID-19 
#create a variable for travel to vaccine dose 1
QTNDres %<>%
  mutate(
    vaccinated_travel_dose1 = factor(
      case_when(
        vaccine_status_TND_onset == "Vaccinated" & walk_cyc == "walk/cycle" ~ "0",
        vaccine_status_TND_2levels_onset == "Vaccinated" & car_alone_dose1 == "car alone or memb household" ~ "1",
        vaccine_status_TND_2levels_onset == "Vaccinated" & car_diff_dose1 == "car with different household" ~ "2",
        vaccine_status_TND_2levels_onset == "Vaccinated" & public_dose1 == "public transport" ~ "3",
        vaccine_status_TND_2levels_onset == "Vaccinated" & transport_other_dose1 == "other transport" ~ "4"
      ),
      levels = c(0, 1, 2, 3, 4),
      labels = c("Vd1_walked/cycled", "Vd1_car_alone", "Vd1_car_different", "Vd1_public_transport", "Vd1_other")
    )
  )

#create variable for riskier behaviour to vaccine - dose 1
QTNDres %<>%
  mutate(
    risky_travel_dose1 = factor(
      case_when(
        vaccinated_travel_dose1 == "Vd1_walked/cycled" | vaccinated_travel_dose1 == "Vd1_car_alone" ~ "0",
        vaccinated_travel_dose1 == "Vd1_car_different" | vaccinated_travel_dose1 == "Vd1_public_transport" ~ "1",
        vaccinated_travel_dose1 == "other transport" ~ "2"
      ),
      levels = c(0, 1, 2),
      labels = c("not risky travel dose 1", "risky travel dose 1", "other travel dose 1")
    )
  )

#association between risky travel and COVID-19 
lr28 <- glm(res ~ risky_travel_dose1 + age5 + gender + eth1 + CEV_flag_2611, data = QTNDres, family = binomial)

odds.ratio(lr28)
