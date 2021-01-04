# 2. faza: Uvoz podatkov


sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# Funkcija, ki uvozi regije iz Wikipedije
uvozi.regije <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "Regija", "odcepitev")
  tabela$Regija <- gsub('Jugovzhodna', 'Jugovzhodna Slovenija', tabela$Regija)
  tabela$Regija <- gsub('Spodnjeposavska', 'Posavska', tabela$Regija)
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  
  for (col in c("obcina", "pokrajina", "Regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

Slovenski_podatki <- uvozi.regije()
Slovenske_regije <- Slovenski_podatki %>% group_by(Regija) %>% summarise(Regija,povrsina=sum(povrsina)) %>% unique()

Sloveski_podatki_urejeni <- Slovenski_podatki %>% group_by(Regija) %>% summarise(Površina = sum(povrsina), Prebivalci = sum(prebivalci), Naselja=sum(naselja), Gostota = Prebivalci/Površina)
Sloveski_podatki_urejeni$Regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', Sloveski_podatki_urejeni$Regija)


#Preberem povprecni pridelek Slovenije
podatki_Slovenija <- read_xlsx("podatki/Povprecje_pridelkov_slovenija.xlsx") %>% pivot_longer(-(1), names_to="leto", values_to="Kolicina") 
podatki_Slovenija  %>% write.csv2("podatki/Povprecje_pridelkov_slovenija_Predelano.csv",fileEncoding = "utf8", row.names = FALSE)

#Preberem povprecni pridelek Slovenskih regij
podatki_Regija <- read_xlsx("podatki/Pregled_pridelkov_regije.xlsx", na=c("", " ", "-")) %>% fill(1:2) %>% drop_na("Kolicina")
podatki_Regija %>% write.csv2("podatki/Pregled_pridelkov_regije_Predelano.csv",fileEncoding = "utf8", row.names = FALSE)

#############################################
##Preberem povprecni pridelek Regije za 2010
#podatki_Regija_2010 <- read_xlsx("podatki/Pregled_pridelkov_regije_2010.xlsx", na=c("", " ", "-")) %>% pivot_longer(-(1), names_to="Proizvodno leto", values_to="Kolicina") %>% drop_na("Kolicina") 
#podatki_Regija_2010  %>% write.csv2("podatki/Pregled_pridelkov_regije_2010_Predelano.csv",fileEncoding = "utf8", row.names = FALSE)

#Preberem povprecni pridelek Regije za 2019
#podatki_Regija_2019 <- read_xlsx("podatki/Pregled_pridelkov_regije_2019.xlsx", na=c("", " ", "-")) %>% pivot_longer(-(1), names_to="Proizvodno leto", values_to="Kolicina") %>% drop_na("Kolicina") 
#podatki_Regija_2019  %>% write.csv2("podatki/Pregled_pridelkov_regije_2019_Predelano.csv",fileEncoding = "utf8", row.names = FALSE)
#############################################

Skupna_tabela <- left_join(podatki_Regija, Sloveski_podatki_urejeni, by=c('Regija'))


# Graf: Zemljevid

Slovenija <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1") %>% fortify()
colnames(Slovenija)[12] <- 'Regija'
Slovenija$Regija <- gsub('Spodnjeposavska', 'Posavska', Slovenija$Regija)
Slovenija$Regija <- gsub('GoriĹˇka', 'Goriška', Slovenija$Regija)
Slovenija$Regija <- gsub('Obalno-kraĹˇka', 'Obalno-kraška', Slovenija$Regija)
Slovenija$Regija <- gsub('Notranjsko-kraĹˇka', 'Notranjsko-kraška', Slovenija$Regija)
Slovenija$Regija <- gsub('KoroĹˇka', 'Koroška', Slovenija$Regija)








#???????????????????????????????? zbriši nakoncu
# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% pivot_longer(`1`:`4`, names_to="velikost.druzine", values_to="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

#=============================================
