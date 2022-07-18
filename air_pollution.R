library(dplyr)
library(ggplot2)

###1) load data and column names ####
dat1999 <- read.table(paste(getwd(), "/RD_501_88101_1999-0.txt", sep=""), sep = "|", header = F, comment.char = "#", na.strings = "")
colnombres99 <- readLines(paste(getwd(), "/RD_501_88101_1999-0.txt", sep=""), 1) %>% 
    strsplit(split = "|", fixed = T) %>% 
    unlist()
names(dat1999) <- colnombres99
dat1999 <- mutate(dat1999, year = 1999)
names(dat1999) <- gsub(" ","_", names(dat1999))

dat2012 <-read.table(paste(getwd(), "/RD_501_88101_2012-0.txt", sep=""), sep="|", header = F, na.strings = "", comment.char = "#")
colnombres12 <- readLines(paste(getwd(), "/RD_501_88101_2012-0.txt", sep=""), 1) %>% 
    strsplit(split="|", fixed=T) %>% 
    unlist()
names(dat2012) <- colnombres12
dat2012 <- mutate(dat2012, year = 2012)
names(dat2012) <- gsub(" ","_", names(dat2012))

airQ <- rbind(dat1999, dat2012)
names(airQ) <- gsub(" ","_", names(airQ))
airQ <- as_tibble(airQ)

#rm(list=grep("[^airQ]", ls(), value = T)) # remove useless objects

###2) comparaciones ####

summary(subset(airQ$Sample_Value, airQ$year==1999))
summary(subset(airQ$Sample_Value, airQ$year==2012))
boxplot(log10(dat1999$Sample_Value), log10(dat2012$Sample_Value))

###3) Por que hay valores negativos en 2012 ####
summary(dat2012$Sample_Value)
neg <- dat2012$Sample_Value<0
table(neg) # numero de valores positivos y negativos
table(neg)[2]/table(neg)[1] # % de valores negativos

    #¿en que fechas se dieron?
dat2012 <- mutate(dat2012, Date=as.Date(as.character(dat2012$Date), "%Y%m%d"))
ggplot(dat2012, aes(Date, Sample_Value))+geom_point(na.rm = T)+coord_cartesian(ylim = c(-12,2))


###4) cambios en una sola estación ####
est1 <- airQ %>% 
    select(County_Code, Site_ID, Sample_Value, year) %>% 
    filter(County_Code==36)
    
meandat <- est1 %>% 
    group_by(year) %>% 
    summarise(mean_by_year = mean(Sample_Value, na.rm=T))

maxdat <- est1 %>% 
    group_by(year) %>% 
summarise(max_by_year = max(Sample_Value, na.rm = T))
    
mindat <- est1 %>% 
    group_by(year) %>% 
    summarise(min_by_year = min(Sample_Value, na.rm = T))

quantdat <- est1 %>% 
    group_by(year) %>% 
    summarise(quantile = quantile(Sample_Value, na.rm = T))

        # Mismo grafico con qplot() y ggplot()
qplot(Sample_Value, data=est1, facets = .~year, color=year, binwidth=3)
ggplot(est1, aes(Sample_Value))+geom_histogram(aes(color=year), binwidth=3)+facet_grid(.~year)

###5) cambios por estado ####

#generar data frames para luego graficar y calcular diferencias
stats99 <- dat1999 %>% 
    select(State_Code, Date, Sample_Value) %>% 
    group_by(State_Code) %>% 
    summarize(Est_Mean = mean(Sample_Value, na.rm=T)) %>% 
    mutate(year = 1999)
stats12 <- dat2012 %>% 
    select(State_Code, Date, Sample_Value) %>% 
    group_by(State_Code) %>% 
    summarize(Est_Mean = mean(Sample_Value, na.rm=T)) %>% 
    mutate(year = 2012)

#grafico de puntos para ver distribución de los promedios en ambos años
    #los datos tienden a ser menores y con menos dispersión en 2012
rbind(stats99, stats12) %>% 
    ggplot(aes(State_Code, Est_Mean))+geom_point()+facet_grid(.~year)   


#calculo de diferencias entre ambos años, presentadas en grafico de barras
mergeddf <- merge(stats99, stats12, by="State_Code") %>% 
    mutate(diference = (Est_Mean.y - Est_Mean.x))
ggplot(mergeddf, aes(State_Code, diference))+geom_col()
