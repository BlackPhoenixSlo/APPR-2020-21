# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
# Če zemljevid nima nastavljene projekcije, jo ročno določimo
proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))


# geom_col
# geom_bar

#Prvi trije grafi
graf_SLO_Kolicina_proizvodov <- podatki_Slovenija %>% group_by(pridelek) %>%  
  ggplot(aes(x=pridelek, y=Kolicina)) + geom_boxplot(color="blue", fill="yellow", alpha=0.3) + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton/ha proizvedbe vsakega proizvoda, v celi Sloveniji: 2010-2019") + xlab("Vrsta pridelka") + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_proizvodov_na_regio <- podatki_Regija %>% group_by(pridelek) %>%  
  ggplot(aes(x=pridelek, y=Kolicina)) + geom_boxplot(color="black", fill="green", alpha=0.3) + theme(axis.text.x = element_text(
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
# Površina slovenije je 20329.9 ha sem izračunal <= sum(Slovenske_regije$povrsina)
 



graf_slovenija <- ggplot(Slovenija, aes(x=long, y=lat, group=group, fill=Regija)) +
  geom_polygon() +
  labs(title="") +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank())

#========================================================================================================
# Koliko produktov se je proizvedno na posamezni hektarski meter v tonah v posamezni regiji
proizvedba_regij <- podatki_Regija %>% group_by(Regija,pridelek) %>% summarise(Regija,Kolicina=sum(Kolicina, na.rm = TRUE),pridelek) %>% unique() %>% group_by(Regija) %>% summarise(Regija=Regija,Kolicina=sum(Kolicina)) %>% unique()
proizvedba_regij$Regija = gsub('Primorsko-notranjska', 'Notranjsko-kraška', proizvedba_regij$Regija)
Skupna_tabela$Regija = gsub('Primorsko-notranjska', 'Notranjsko-kraška', Skupna_tabela$Regija)
proizvedba_regij <- left_join(proizvedba_regij, Slovenske_regije , by= c('Regija'))
jaka <- proizvedba_regij$Kolicina * proizvedba_regij$povrsina
proizvedba_regij$Kolicina_v_tonah <- jaka
proizvedba_regij <- left_join(proizvedba_regij,Skupna_tabela %>% summarise(Regija, Naselja) %>% unique(), by=c('Regija'))

proizvedba_regij$Kolicina_v_tonah <- proizvedba_regij$Kolicina_v_tonah / 1000000
zemljevid <- ggplot() +
  geom_polygon(data = right_join(proizvedba_regij,Slovenija, by = c('Regija')),
               aes(x = long, y = lat, group = group, fill = Kolicina_v_tonah ))+
  xlab("") + ylab("") + ggtitle('Količina proizvoda v posameznih regijah v miljon tonah') + 
  theme( axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
 scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(0,8)) # dodej nekej masesto "4x +06"
zemljevid$labels$fill <- 'Količina proizvoda \\n |n v miljonih'


zemljevid_reduciran <- ggplot() +
  geom_polygon(data = right_join(proizvedba_regij,Slovenija, by = c('Regija')),
               aes(x = long, y = lat, group = group, fill = (Kolicina_v_tonah / Naselja) ))+
  xlab("") + ylab("") + ggtitle('Količina proizvoda v posameznih regijah v tonah relativno z številom naselij') + 
  theme( axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_gradient(low = '#25511C', high='#2BFF00') 
zemljevid_reduciran$labels$fill <- 'Količina proizvoda v tonah'



graf_otroci_indeks <- 
  
ggplot((data = podatki_Slovenija), aes(x=leto, y=Kolicina, col= pridelek, group = pridelek)) +  geom_line() + geom_point()
#kako narest da je ta grav tak da ma povezane samo oranyne pike


#kako narest, da bi bili pridelki povezani z črto koliko se večajo leta horizontalno
graf_indeks <- ggplot(data = podatki_Regija , aes(x=leto, y=Kolicina, color=Regija, group = Regija)) + geom_line(aes(frame=pridelek)) + geom_line(aes(frame=pridelek)) +scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(0,30))
graf_indeks <- graf_indeks + xlab('Leto proizvodnje') + ylab('Količina proizvodov t/ha')
graf_indeks <- ggplotly(graf_indeks)


sestevek_po_pridelkih <- Skupna_tabela %>% group_by(leto, Regija) %>% summarise(Regija, leto, Kolicina=sum(Kolicina)) %>% unique()
Sestevek_po_pridelkih_regijah  <- sestevek_po_pridelkih %>% group_by(leto) %>% summarise(leto, Kolicina=sum(Kolicina)) %>% unique()

sestevek_po_pridelkih <- left_join(sestevek_po_pridelkih,Skupna_tabela %>% summarise(Regija, Naselja) %>% unique(), by=c('Regija'))
sestevek_po_pridelkih$Kolicina_reduciran <- sestevek_po_pridelkih$Kolicina / sestevek_po_pridelkih$Naselja * 100
graf_indeks_reduciran <- ggplot(data = sestevek_po_pridelkih, aes(x=leto, group = Regija)) + geom_line(aes(y=Kolicina_reduciran,frame=Regija, color="Reducirano * 100")) + geom_line(aes(y = Kolicina,frame=Regija, color="Regija"))+ geom_line(aes(y = Naselja /10,frame=Regija, color="Št. Naselj / 10"))
graf_indeks_reduciran <- graf_indeks_reduciran + xlab('Leto proizvodnje') + ylab('Količina proizvodov t/ha')
graf_indeks_reduciran <- ggplotly(graf_indeks_reduciran)

#??=================
Sestevek_po_pridelkih_regijah$leto <- as.character(Sestevek_po_pridelkih_regijah$leto)
Sestevek_po_pridelkih_regijah$Kolicina <- as.character(Sestevek_po_pridelkih_regijah$Kolicina)


Sestevek_po_pridelkih_regijah <- Sestevek_po_pridelkih_regijah %>% mutate(leto = parse_integer(leto), Kolicina = parse_number(Kolicina))
prileganje <- lm(Kolicina ~ leto, data = Sestevek_po_pridelkih_regijah)
predict(prileganje, data.frame(leto=seq(2010,2022,1)))


graf_napoved <- ggplot(Sestevek_po_pridelkih_regijah, aes(x=leto, y=Kolicina)) + geom_point() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='green') + scale_x_continuous('leto', breaks = seq(2010, 2022, 1), limits = c(2010,2022))






