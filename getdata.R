library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# Pull the data from ClinicalTrials.gov
data <- list()
silly <- GET("https://clinicaltrials.gov/api/v2/studies",query=list(pageSize=1000,
                                                        filter.advanced="AREA[StudyType]INTERVENTIONAL AND AREA[OverallStatus]RECRUITING"))
data[[1]] = fromJSON(rawToChar(silly$content),simplifyVector = TRUE)
mytoken = data[[1]]$nextPageToken

i = 2
while(is.null(mytoken)==FALSE){
  silly <- GET("https://clinicaltrials.gov/api/v2/studies",query=list(pageSize=10000,
                                                                      pageToken=mytoken,
                                                                      filter.advanced="AREA[StudyType]INTERVENTIONAL AND AREA[OverallStatus]RECRUITING"))
  data[[i]] <- fromJSON(rawToChar(silly$content),,simplifyVector = TRUE)
  mytoken =  data[[i]]$nextPageToken
  i=i+1
}


# Parse the JSON files
temp2 <- list()
data_part2 <- list()

for(j in 1:length(data)){
  study <- pluck(data[[j]],"studies")
  protocol_section = pluck(study,"protocolSection")
  identification_module = pluck(protocol_section,"identificationModule")
  nct_id = pluck(identification_module,"nctId")
  org_study_id = pluck(pluck(identification_module,"orgStudyIdInfo"),"id")
  organization = pluck(pluck(identification_module,"organization"),"fullName")
  brief_title = pluck(identification_module,"briefTitle")
  brief_summary = pluck(pluck(protocol_section,"descriptionModule"),"briefSummary")
  conditions = pluck(pluck(protocol_section,"conditionsModule"),"conditions")
  condition_str = lapply(conditions, function(x) paste0(x,collapse =", " ))
  keyword_arr = pluck(pluck(protocol_section,"conditionsModule"),"keywords")
  keyword = lapply(keyword_arr, function(x) paste0(x,collapse =", " ))
  study_type = pluck(pluck(protocol_section,"designModule"),"studyType")
  study_phase = pluck(pluck(protocol_section,"designModule"),"phases")
  study_phase2 = lapply(study_phase, function(x) paste0(x,collapse =", " ))
  interventions_list = pluck(pluck(protocol_section,"armsInterventionsModule"),"interventions")
  intervention_types = lapply(interventions_list, function(x) x %>% dplyr::select("type")) #[intervention.get("type", "") for intervention in interventions_list]
  intervention_type = lapply(intervention_types, function(x) paste0(x,collapse =", " ))
  
  minimum_age = pluck(pluck(protocol_section,"eligibilityModule"),"minimumAge")
  maximum_age = pluck(pluck(protocol_section,"eligibilityModule"),"maximumAge")
  locations = pluck(pluck(protocol_section,"contactsLocationsModule"),"locations")
  facility = map(locations,"facility")
  city = map(locations,"city")
  state = map(locations,"state")
  zip = map(locations,"zip")
  country = map(locations,"country")
  status = map(locations,"status")
  geoPoint = map(locations,"geoPoint")
  
  # Pull out all the locations for each study
  for(loc in 1:length(locations)){
    temp2[[loc]] <- data.frame(
      country=if(is.null(country[[loc]])==TRUE) {NA} else {unlist(country[loc])},
      facility = if(is.null(facility[[loc]])==TRUE) {NA} else {unlist(facility[loc])},
      city = if(is.null(city[[loc]])==TRUE) {NA} else {unlist(city[loc])},
      state = if(is.null(state[[loc]])==TRUE) {NA} else {unlist(state[loc])},
      zip = if(is.null(zip[[loc]])==TRUE) {NA} else {unlist(zip[loc])},
      status = if(is.null(status[[loc]])==TRUE) {NA} else {unlist(status[loc])},
      geoPoint.lat = if(is.null(geoPoint[[loc]]$lat)==TRUE) {NA} else {geoPoint[[loc]]$lat},
      geoPoint.lon = if(is.null(geoPoint[[loc]]$lon)==TRUE) {NA} else {geoPoint[[loc]]$lon}
    ) %>% 
      mutate(
        nct_id = nct_id[loc],
        org_study_id = org_study_id[[loc]],
        organization = organization[loc],
        brieftitle = brief_title[loc],
        briefsummary = brief_summary[loc],
        condition = unlist(condition_str[loc]),
        keyword = unlist(keyword[loc]),
        studytype = study_type[loc],
        intervention_type = unlist(intervention_type[loc]),
        minimum_age = minimum_age[loc],
        maximum_age = maximum_age[loc],
        phase = study_phase2[loc]
        
      ) %>%
      mutate(studyurl = paste0("https://clinicaltrials.gov/study/",nct_id),)
  }
  
  # Combine all the dataframes in temp2 into a single dataframe 
  temp2_df <- do.call(rbind,temp2)
  
  # create a dataframe of all unique study-facility-contact combinations
  locations_df <- map_df(locations,dplyr::bind_rows,.id="study_number") %>% 
    mutate(study_number = as.numeric(study_number),
           rownumber = seq(1,nrow(.)),
           geoPoint.lat = geoPoint$lat,
           geoPoint.lon = geoPoint$lon)
  
  nct_id_df <- data.frame(nct_id = nct_id,
                          study_number = seq(1:length(nct_id)))
  locations_df2 <- left_join(locations_df,nct_id_df)
  contacts_df <- map_df(locations_df$contacts,bind_rows,.id="facility_number") %>% 
    filter(role=="CONTACT") %>% 
    mutate(facility_number = as.numeric(facility_number))
  FullContactInfo <- left_join(locations_df2,contacts_df,by=c( "rownumber"="facility_number")) %>%
    select(facility,nct_id,zip,name,phone,email,geoPoint.lat,geoPoint.lon)
  
  # join the contact info to the study/facility info and save to a list
  temp2_df2 <- left_join(temp2_df,FullContactInfo,
                         by=c("nct_id"="nct_id","facility"="facility","zip"="zip","geoPoint.lat"="geoPoint.lat","geoPoint.lon"="geoPoint.lon"),
                         relationship = "many-to-many")
  data_part2[[j]] <- temp2_df2
}

finaldata <- do.call(rbind,data_part2) %>% filter(country == "United States")
finaldata$phase = as.vector(do.call(rbind,finaldata$phase))
finaldata$state <- ifelse(finaldata$state == "Washington" & is.na(finaldata$zip) == FALSE & as.numeric(substr(finaldata$zip, start = 1, stop = 5)) < 30000, "District of Columbia",finaldata$state)

finaldata <- finaldata %>% filter(str_detect(tolower(condition),tolower("diabetes"))==TRUE |
                                   str_detect(tolower(keyword),tolower("diabetes"))==TRUE |
                                   str_detect(tolower(briefsummary),tolower("diabetes"))==TRUE)

# Get Missing Geo-locations

library(tidygeocoder)

nogeopoints <- finaldata %>% filter(is.na(geoPoint.lat)==TRUE)
newgeo = tryCatch(geo(address = unique(nogeopoints$zip),method="mapbox"),error=function(e) {
  Sys.sleep(3)
  geo(address = unique(nogeopoints$zip),method="mapbox")
})
newgeo <- left_join(nogeopoints,newgeo,by=c("zip"="address"))
newgeo <- newgeo %>% 
  mutate(geoPoint.lat = lat,
         geoPoint.lon = long) %>%
  select(-c("lat","long"))

finaldata = rbind(finaldata[is.na(finaldata$geoPoint.lat) == FALSE,],newgeo) 

# Format data for app
finaldata <- finaldata %>% mutate(
  
  MINAGE = case_when( 
    str_detect(tolower(minimum_age),"year") == TRUE ~ parse_number(minimum_age),
    str_detect(tolower(minimum_age),"month") == TRUE ~ parse_number(minimum_age)/12,
    str_detect(tolower(minimum_age),"week") == TRUE ~ parse_number(minimum_age)/52,
    str_detect(tolower(minimum_age),"day") == TRUE ~ parse_number(minimum_age)/365.25,
    str_detect(tolower(minimum_age),"hour") == TRUE ~ parse_number(minimum_age)/8760,
    TRUE ~ 0),
  
  MAXAGE = case_when( 
    str_detect(tolower(maximum_age),"year") == TRUE ~ parse_number(maximum_age),
    str_detect(tolower(maximum_age),"month") == TRUE ~ parse_number(maximum_age)/12,
    str_detect(tolower(maximum_age),"week") == TRUE ~ parse_number(maximum_age)/52,
    str_detect(tolower(maximum_age),"day") == TRUE ~ parse_number(maximum_age)/365.25,
    str_detect(tolower(maximum_age),"hour") == TRUE ~ parse_number(maximum_age)/8760,
    TRUE ~ 100),
  
  phase = str_replace_all(str_replace_all(phase,"PHASE","Phase "),"EARLY_","Early "),
  
  CityState = paste0(city,", ",state),
  FacilityLoc = paste0(facility,"; ",city,", ",state),
  LATEST_REFRESH = Sys.time(),
  CONTACT = paste0(name," | ",email," | ",phone)) %>% 
# Custom groupings
mutate(
  
  Type2 = ifelse(str_detect(tolower(condition),regex("type+2"))|
                   str_detect(tolower(condition),regex("type.+2"))|
                   str_detect(tolower(condition),regex("type+ii"))|
                   str_detect(tolower(condition),regex("type.ii")) |
                   str_detect(tolower(condition),regex("t2d")) |
                   
                   str_detect(tolower(keyword),regex("type+2"))|
                   str_detect(tolower(keyword),regex("type.+2"))|
                   str_detect(tolower(keyword),regex("type+ii"))|
                   str_detect(tolower(keyword),regex("type.ii")) |
                   str_detect(tolower(keyword),regex("t2d")) |
                   
                   str_detect(tolower(briefsummary),regex("type+2"))|
                   str_detect(tolower(briefsummary),regex("type.+2"))|
                   str_detect(tolower(briefsummary),regex("type+ii"))|
                   str_detect(tolower(briefsummary),regex("type.ii")) |
                   str_detect(tolower(briefsummary),regex("t2d")
                   )==TRUE,1,0),
  
  Type1 = ifelse(str_detect(tolower(condition),regex("type+1"))|
                   str_detect(tolower(condition),regex("type.+1"))|
                   str_detect(tolower(condition),regex("type+i "))|
                   str_detect(tolower(condition),regex("type.i ")) |
                   str_detect(tolower(condition),regex("t1d")) |
                   
                   str_detect(tolower(keyword),regex("type+1"))|
                   str_detect(tolower(keyword),regex("type.+1"))|
                   str_detect(tolower(keyword),regex("type+i "))|
                   str_detect(tolower(keyword),regex("type.i ")) |
                   str_detect(tolower(keyword),regex("t1d")) |
                   
                   str_detect(tolower(briefsummary),regex("type+1"))|
                   str_detect(tolower(briefsummary),regex("type.+1"))|
                   str_detect(tolower(briefsummary),regex("type+i "))|
                   str_detect(tolower(briefsummary),regex("type.i ")) |
                   str_detect(tolower(briefsummary),regex("t1d")
                   )==TRUE,1,0),
  
  Prediabetes = ifelse(str_detect(tolower(condition),regex("prediabet"))|
                         str_detect(tolower(condition),regex("pre.diabet")) |
                         
                         str_detect(tolower(keyword),regex("prediabet"))|
                         str_detect(tolower(keyword),regex("pre.diabet")) |
                         
                         str_detect(tolower(briefsummary),regex("prediabet"))|
                         str_detect(tolower(briefsummary),regex("pre.diabet")
                         )==TRUE,1,0),
  
  Neuropathy= ifelse(str_detect(tolower(condition),regex("neuropath"))|
                       str_detect(tolower(condition),regex("foot"))|
                       str_detect(tolower(condition),regex("ulcer"))|
                       str_detect(tolower(condition),regex("weakness"))|
                       str_detect(tolower(condition),regex("amputation"))|
                       
                       str_detect(tolower(keyword),regex("neuropath"))|
                       str_detect(tolower(keyword),regex("foot"))|
                       str_detect(tolower(keyword),regex("ulcer"))|
                       str_detect(tolower(keyword),regex("weakness"))|
                       str_detect(tolower(keyword),regex("amputation"))|
                       
                       str_detect(tolower(briefsummary),regex("neuropath"))|
                       str_detect(tolower(briefsummary),regex("foot"))|
                       str_detect(tolower(briefsummary),regex("ulcer"))|
                       str_detect(tolower(briefsummary),regex("weakness"))|
                       str_detect(tolower(briefsummary),regex("amputation")
                       )==TRUE,1,0),
  
  Gestational = ifelse(str_detect(tolower(condition),regex("gestational")) |
                         str_detect(tolower(condition),regex("gdn"))|
                         
                         str_detect(tolower(keyword),regex("gestational")) |
                         str_detect(tolower(keyword),regex("gdn"))|
                         
                         str_detect(tolower(briefsummary),regex("gestational")) |
                         str_detect(tolower(briefsummary),regex("gdn")
                         )==TRUE,1,0),
  
  Hyperglycemia = ifelse(str_detect(tolower(condition),regex("hyperglycem"))|
                           str_detect(tolower(keyword),regex("hyperglycem"))|
                           str_detect(tolower(briefsummary),regex("hyperglycem")
                           )==TRUE,1,0),
  
  Hypoglycemia = ifelse(str_detect(tolower(condition),regex("hypoglycem"))|
                          str_detect(tolower(keyword),regex("hypoglycem"))|
                          str_detect(tolower(briefsummary),regex("hypoglycem")
                          )==TRUE,1,0),
  
  Hypertension = ifelse(str_detect(tolower(condition),regex("hypertension"))|
                          str_detect(tolower(keyword),regex("hypertension"))|
                          str_detect(tolower(briefsummary),regex("hypertension")
                          )==TRUE,1,0),
  
         dummy = 0

) %>% filter(geoPoint.lon < 0)



names(finaldata) <- toupper(names(finaldata))

saveRDS(finaldata,file="trials.RDS")
