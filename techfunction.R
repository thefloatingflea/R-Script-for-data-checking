
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Troubleshooting Function Saul M
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)

evaluate_exif <- function(df) {
  df1 <-  df
  check_colnames(df1)
  check_tagformat(df1)
  check_NA(df1)
  check_contacts(df1)
  check_species(df1)
  df1
}

check_colnames <- function(df) {
  ifelse("Subject" %in% colnames(df) == TRUE,"Column 'Subject' detected",
         stop(print(
           "ERROR Subject columns is missing, this means the column with 
                 the tags has a different name or none of these images may have 
                 been tagged
                 ---------------------------------------------------------------
                 The tag column MUST be called'Subject' with a capital S"))
  )
}

check_NA <- function(df){
  df_Subject <- as.vector(df$Subject)
  df_Sourcefile <- as.vector(df$SourceFile)
  y <- 0
  x <- 0
  for (i in df_Subject){
    y <- y+1
    if(i == ""){
      print("ERROR blank image")
      print(df_Sourcefile[y])
      x <- 1}
    if((grepl(pattern = "placeID: ",i)==FALSE)){
      print("ERROR images missing placeIDs, check you have added placeID to ALL images!")
      print(df_Sourcefile[y])
      x <- 1}
  }
  if(x == 1){stop("You have either Blank Images or missing placeID tags, please fix this and then start again")}
}

check_tagformat <- function(df){
  df_Subject <- as.vector(df$Subject)
  tagdat <- tagsep(as.character(df_Subject), ", ", ": ")
  colnam <- names(tagdat)
  for (i in colnam)
  {
    if((grepl(pattern = "species1|species2|species3|species4|species5|contact1|contact2|contact3|contact4|contact5|placeID",
              i)) == FALSE) {
      print("ERROR incorrect column, check tag formatting!")
      print(i)
    }
  }
  sp_colnam <- c()
  for (i in colnam){if ((grepl(pattern = "contact|placeID",i))==FALSE){sp_colnam <- c(sp_colnam, i)}}
  sp_col <- tagdat %>% select(all_of(sp_colnam))
  sp <- c()
  for (i in sp_col)
  {
    sp <- c(sp,i)
  }
  sp_list <- unique(sp %>%
                      as.data.frame(sp) %>% 
                      filter(!is.na(sp)) %>%
                      rename(species = "."))
  sp_list <- as.vector(sp_list$species)
  for (i in sp_list)
  {
    if ((grepl(pattern = "Cat|Dog|Fox|Hedgehog|Horse|Mouse|Rabbit|Rat|Squirrel",i))==FALSE)
    {
      print(
        "Potential error, Unrecognised species name?"
      )
      print(i)
    }
  }
}

check_contacts <- function(df){
  df_Subject <- as.vector(df$Subject)
  df_Sourcefile <- as.vector(df$SourceFile)
  y <- 0
  z <- 1
  repeat
  {
    for (i in df_Subject){
      y <- y+1
      if(grepl(paste0("contact",z), i, fixed = TRUE)) 
        (if((grepl(paste0("species",z,":"), i, fixed = TRUE))==FALSE)
        {
          print("ERROR There is a Contact with No Species!")
          print(df_Sourcefile[y])
        })}
    y <- 0
    z <- z+1
    if(z > 5){break}
  }
}


check_species <- function(df){
  sub_df <- df %>% select(CreateDate,Subject,SourceFile)
  df_Subject <- as.vector(df$Subject)
  tagdat <- tagsep(as.character(df_Subject), ", ", ": ")
  placeIDs <- unique(tagdat$placeID)
  
  x <- ""
  y <- 0
  z <- 1
  j <- 0
  k <- length(placeIDs)
  l <- 1
  m <- c()
  
  while(l < k+1)
  {  
    j <- placeIDs[l]
    if(j < 10) j <- as.character(paste0("0",j))
    sub_df1 <- filter(sub_df,grepl((paste0("placeID: ",j)),Subject))
    
    startime <- as_datetime(min(sub_df1$CreateDate))
    finishtime <- as_datetime(max(sub_df1$CreateDate))
    runtime <- as.data.frame(as_datetime(seq(from = startime,to = finishtime, by = "sec"))) %>% 
      rename(CreateDate = 'as_datetime(seq(from = startime, to = finishtime, by = "sec"))')
    sub_df1$CreateDate <- as_datetime(sub_df1$CreateDate)
    camalltime <- merge(runtime,sub_df1,by = "CreateDate",all = TRUE) 
    camalltime <- camalltime[order(camalltime$CreateDate),]
    
    vec_subject <- as.vector(camalltime$Subject)
    vec_sourcefile <- as.vector(camalltime$SourceFile)
    
    while (z < 6)
    {
      for (i in vec_subject){
        y <- y+1
        if(grepl(paste0("species",z), i, fixed = TRUE)){
          if((grepl(paste0("species",z), x, fixed = TRUE))==FALSE){
            if((grepl(paste0("contact",z),(paste0(unique(vec_subject[(ifelse(y>120,(y-120),y)):y+1]),collapse = "")), fixed = TRUE))==FALSE){
              if((grepl(paste0("contact",z), i, fixed = TRUE))==FALSE)
              {m <- c(m,vec_sourcefile[y])}
            }}}
        x <- i
      }
      x <- ""
      y <- 0
      z <- z+1
    }
    if((length(m))>0){
      print("POTENTIAL ERRORS, there appears to be a contact missing for these species sequences:")
      print(j)
      print(m)}
    m <- c()
    z <- 1
    l <- l+1
    
    rm(camalltime)
    
    }
  
}


