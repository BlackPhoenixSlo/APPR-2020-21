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

Slovenski_podatki_urejeni <- uvozi.regije() %>% group_by(Regija) %>% summarise(Povrsina = sum(povrsina), Prebivalci = sum(prebivalci), Naselja=sum(naselja), Gostota = Prebivalci/Povrsina)
Slovenski_podatki_urejeni$Regija <- gsub('Notranjsko-kraška', 'Primorsko-notranjska', Sloveski_podatki_urejeni$Regija)


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
                             "SVN_adm1", encoding = "UTF-8") %>% fortify()
colnames(Slovenija)[12] <- 'Regija'
Slovenija$Regija <- gsub('Spodnjeposavska', 'Posavska', Slovenija$Regija)
Slovenija$Regija <- gsub('GoriĹˇka', 'Goriška', Slovenija$Regija)
Slovenija$Regija <- gsub('Obalno-kraĹˇka', 'Obalno-kraška', Slovenija$Regija)
Slovenija$Regija <- gsub('Notranjsko-kraĹˇka', 'Notranjsko-kraška', Slovenija$Regija)
Slovenija$Regija <- gsub('KoroĹˇka', 'Koroška', Slovenija$Regija)






