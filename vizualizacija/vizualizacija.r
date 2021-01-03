# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
# Če zemljevid nima nastavljene projekcije, jo ročno določimo
proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))



#Prvi trije grafi
graf_SLO_Kolicina_proizvodov <- podatki_Slovenija %>% group_by(pridelek) %>%  
  ggplot(aes(x=pridelek, y=Kolicina)) + geom_boxplot() + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton/ha proizvedbe vsakega proizvoda, v celi Sloveniji: 2010-2019") + xlab("Vrsta pridelka") + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_proizvodov_na_regio <- podatki_Regija %>% group_by(pridelek) %>%  
  ggplot(aes(x=pridelek, y=Kolicina)) + geom_boxplot() + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton/ha proizvedbe vsakega proizvoda, za vsako regijo: 2010-2019") + xlab("pridelka") + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_koruze_v_regiji_na_Leto <- podatki_Regija %>% group_by(pridelek) %>% filter(pridelek=="Koruza") %>%
  ggplot(aes(x=leto, y=Kolicina)) + geom_boxplot() + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton proizvedbe koruze na hektarski meter, v celi sloveniji: 2010-2019") + xlab("Leto proizvoda koruze") + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_breskev_v_regiji_na_Leto <- podatki_Regija %>% group_by(pridelek) %>% filter(pridelek=="Breskve") %>%
  ggplot(aes(x=leto, y=Kolicina)) + geom_boxplot() + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton proizvedbe brezkev na hektarski meter, v celi sloveniji: 2010-2019") + xlab("Leto proizvoda brezkev") + ylab("Količina t/ha proizvoda")


#========================================================================================================


graf_slovenija <- ggplot(Slovenija, aes(x=long, y=lat, group=group, fill=Regija)) +
  geom_polygon() +
  labs(title="Slovenija") +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank())

#========================================================================================================
# Koliko produktov se je proizvedno na posamezni hektarski meter v tonah v posamezni regiji
proizvedba_regij <- podatki_Regija %>% group_by(Regija,pridelek) %>% summarise(Regija,Kolicina=sum(Kolicina, na.rm = TRUE),pridelek) %>% unique() %>% group_by(Regija) %>% summarise(Regija=Regija,Kolicina=sum(Kolicina)) %>% unique()
proizvedba_regij$Regija = gsub('Primorsko-notranjska', 'Notranjsko-kraška', proizvedba_regij$Regija)
proizvedba_regij <- left_join(proizvedba_regij, Slovenske_regije , by= c('Regija'))
jaka <- proizvedba_regij$Kolicina * proizvedba_regij$povrsina
proizvedba_regij$Kolicina_v_tonah <- jaka


zemljevid <- ggplot() +
  geom_polygon(data = right_join(proizvedba_regij,Slovenija, by = c('Regija')),
               aes(x = long, y = lat, group = group, fill = Kolicina_v_tonah ))+
  xlab("") + ylab("") + ggtitle('Količina proizvoda v posameznih regijah v tonah') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) 
  


