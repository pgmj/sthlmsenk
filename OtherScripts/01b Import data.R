
# First, read data for the municipality in focus, and adjust available columns/variables
# as needed (a vector of necessary variables should be made). Then jump to the 
# General recode preprocessing part and run the rest of the code. After that, open script 03
# to estimate person locations for each index

library(foreign)
library(tidyverse)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

# Import data -------------------------------------------------------------

# make vector of demographic variables to include in final datafile
demogr.vars<-read.csv("../data/demographicVariables.csv")
demogr.vars <- demogr.vars$demogr.vars

# all analyzed items
allAnalyzedItems <- read.csv("../data/allitems.csv")



## Stockholm stad ----------------------------------------------------------

# read and combine data
df.1420 <- read.spss("../data/SE 2014-2020 KI Leifman.sav", to.data.frame = TRUE)
df.0612 <- read.spss("../data/2006-2012 Stockholmsenkäten 201126.sav", to.data.frame = TRUE)
df.sthlm <- rbind(df.0612,df.1420)

# subset selected demographic variables and items
df.sthlm <- df.sthlm %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

# to match Vaxholms variables
df.sthlm <- df.sthlm %>% 
  add_column(SkolID_gammal = NA, .before = "SkolSDO") %>% 
  add_column(DIDkommun = 'Stockholm')

## Vallentuna ----------------------------------------------------------
df.vtuna1618 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2018 Vallentuna 2016-2018.sav"))
df.vtuna20 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2020 Vallentuna.sav"))

df.vtuna1618 <- df.vtuna1618 %>% 
  rename(ARSKURS = Arskurs)

df.vtuna20 <- df.vtuna20 %>% 
  rename(Kön = F2)

df.vtuna1618 <- df.vtuna1618 %>% 
  rename(Skolnamn = SkolID) %>% 
  add_column(Skolenhetskod = NA)

## subset variables
df.vtuna1 <- df.vtuna1618 %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))
df.vtuna2 <- df.vtuna20 %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

# Columns `F67`, and `F68` don't exist.
df.vtuna1 <- df.vtuna1 %>% 
  add_column(F67 = NA, .after = "f101l") %>% 
  add_column(F68 = NA, .before = "F14") %>%
  add_column(F3_Omkodad = NA, .after = "Kön") %>% 
  add_column(FNY12020 = NA, .after = "F14") %>% 
  add_column(F41= NA, .after = "F34") %>% 
  add_column(F44 = NA, .before = "F51") %>% 
  add_column(F37 = NA, .after = "F20") %>% 
  add_column(F45 = NA, .after = "F35") %>% 
  add_column(FNY22020 = NA, .after = "F40") 

# Then Vallentuna 2022
df.vtuna22 <- read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2022 Vallentuna.sav",
                        to.data.frame = TRUE) %>% 
  rename(Kön = F2) %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

#setdiff(names(df.vtuna1),names(df.vtuna22))

df.vtuna <- rbind(df.vtuna1,df.vtuna2,df.vtuna22)

# to match Vaxholms variables
df.vtuna <- df.vtuna %>% 
  add_column(SkolID_gammal = NA, SkolSDO = NA) %>% 
  add_column(DIDkommun = 'Vallentuna')

#setdiff(names(df.sthlm),names(df.vtuna))


## Vaxholm ----------------------------------------------------------

df.vaxholm <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vaxholm/Sthlmsenk/Stockholmsenkäten 2008-2022 Vaxholm (1).sav"))

df.vaxholm <- df.vaxholm %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Vaxholm')

## Danderyd ----------------------------------------------------------------

df.danderyd <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Danderyd/Stockholmsenkäten 2014-2022 Danderyd.sav"))

df.danderyd <- df.danderyd %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Danderyd')


## Täby --------------------------------------------------------------------

df.täby <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Täby/Stockholmsenkäten 2016 & 2020-2022 Täby (2).sav"))

df.täby <- df.täby %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Täby')

## Södertälje --------------------------------------------------------------

df.södertälje <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Södertälje/Stockholmsenkäten 2002-2022 Södertälje_granskad.sav"))
# estimerade <- names(df) %>% tail(7)
df.södertälje <- df.södertälje %>% 
  rename(Kön = F2) %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(Skolenhetskod = NA, .after = "Skolkommun") %>% 
  add_column(Skolnamn = NA, .before = "ARSKURS") %>% 
  add_column(SkolID_gammal = NA, 
             SkolSDO = NA,
             DIDkommun = 'Södertälje')


## Sigtuna -----------------------------------------------------------------

df.sigtuna <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Sigtuna/Stockholmsenkäten 2012-2022 Sigtuna (9).sav"))
#df.sigtuna2 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Sigtuna/Stockholmsenkäten 2012-2022 Sigtuna (4).sav"))

df.sigtuna <- df.sigtuna %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Sigtuna')



## Järfälla ----------------------------------------------------------------

df.jfl1 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Järfälla/Stockholmsenkäten 2006-2020 Järfälla.sav"))
df.jfl2 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Järfälla/Stockholmsenkäten 2022 Järfälla.sav"))

df.jfl1 <- df.jfl1 %>% 
  rename(Kön = F2) %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(SkolID_gammal = NA, 
             SkolSDO = NA,
             DIDkommun = 'Järfälla')

df.jfl2 <- df.jfl2 %>% 
  rename(Kön = F2) %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(SkolID_gammal = NA, 
             SkolSDO = NA,
             DIDkommun = 'Järfälla')

df.jfl <- rbind(df.jfl1,df.jfl2)


## Lidingö -----------------------------------------------------------------

df.lid1 <- read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Lidingö/Stockholmsenkäten 2016 Lidingö.sav", to.data.frame = TRUE) %>%
  rename(
    ARSKURS = Arskurs,
    Skolnamn = SkolID
  ) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    Skolenhetskod = NA,
    DIDkommun = "Lidingö"
  ) %>%
  relocate(Skolenhetskod, .after = "Skolkommun") %>% 
  add_column(F67 = NA, .after = "f101l") %>%
  add_column(F68 = NA, .before = "F14") %>%
  add_column(F3_Omkodad = NA, .after = "Kön") %>%
  add_column(FNY12020 = NA, .after = "F14") %>%
  add_column(F41 = NA, .after = "F34") %>%
  add_column(F44 = NA, .before = "F51") %>%
  add_column(F37 = NA, .after = "F20") %>%
  add_column(F45 = NA, .after = "F35") %>%
  add_column(FNY22020 = NA, .after = "F40")

df.lid2 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Lidingö/Stockholmsenkäten 2018 Lidingö.sav")) %>% 
  rename(
    ARSKURS = Arskurs,
    Skolnamn = SkolID
  ) %>%
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>%
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    Skolenhetskod = NA,
    DIDkommun = "Lidingö"
  ) %>%
  relocate(Skolenhetskod, .after = "Skolkommun") %>% 
  add_column(F67 = NA, .after = "f101l") %>%
  add_column(F68 = NA, .before = "F14") %>%
  add_column(F3_Omkodad = NA, .after = "Kön") %>%
  add_column(FNY12020 = NA, .after = "F14") %>%
  add_column(F41 = NA, .after = "F34") %>%
  add_column(F44 = NA, .before = "F51") %>%
  add_column(F37 = NA, .after = "F20") %>%
  add_column(F45 = NA, .after = "F35") %>%
  add_column(FNY22020 = NA, .after = "F40")


df.lid3 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Lidingö/Stockholmsenkäten 2020 Lidingö.sav")) %>% 
  rename(Kön = F2) %>% 
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>% 
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    DIDkommun = "Lidingö"
  )
  
df.lid4 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Lidingö/Stockholmsenkäten 2022 Lidingö.sav")) %>% 
  rename(Kön = F2) %>% 
  select(any_of(c(demogr.vars, allAnalyzedItems$itemnr, "SkolID_gammal", "SkolSDO"))) %>% 
  add_column(
    SkolID_gammal = NA,
    SkolSDO = NA,
    DIDkommun = "Lidingö"
  )

df.lidingö <- rbind(df.lid1,
                    df.lid2,
                    df.lid3,
                    df.lid4)


## Botkyrka ----------------------------------------------------------------

df.botkyrka <- read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Botkyrka/Stockholmsenkäten 2004-2022 Botkyrka (4).sav", to.data.frame = TRUE)

df.botkyrka <- df.botkyrka %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = "Botkyrka")
  

### compare within municipality
# setdiff(names(df.jfl1),names(df.jfl2))
# setdiff(names(df.jfl2),names(df.jfl1))
### compare to template
#setdiff(names(df),names(df.lid3))
#setdiff(names(df.lid3),names(df))


# df <- df %>%
#   rename(`Hur länge har du bott i Sverige?` = F5,
#          `Vilken högsta utbildning har din mamma?` = f6a,
#          `Vilken högsta utbildning har din pappa?` = f6b,
#          `Vad bor du i för typ av bostad?` = F7)
# df.old <- read_parquet("../DIDapp/data/SthlmsEnkRev_2022-12-20.parquet")
# df.old <- df.old %>%
#   rename(Community = Närsamhälle,
#          Parenting = Föräldraskap,
#          PsykSomBesv = 'Psykiska/ psykosomatiska besvär',
#          SkolaNegativ = 'Vantrivsel i skolan',
#          Wellbeing = Välbefinnande,
#          SkolaPositiv = 'Positiv skolanknytning'
#   )
# df.old <- df.old %>%
#   rename(DIDkommun = Kommun)

# write_parquet(df.new, sink = glue("../data/{Sys.Date()}_ScoredRev.parquet"))

# Combine all data --------------------------------------------------------

## find out which, if any, variables differ between df's
# df.sthlm %>%
#  select_at(vars(names(df.vaxholm)))

df <- rbind(df.sthlm,
            df.vtuna,
            df.vaxholm,
            df.danderyd,
            df.täby,
            df.södertälje,
            df.jfl,
            df.sigtuna,
            df.lidingö,
            df.botkyrka)

## On to next script! 02 for recodings.