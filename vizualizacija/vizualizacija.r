# 3. faza: Vizualizacija podatkov



# geom_col
# geom_bar

#Prvi trije grafi
graf_SLO_Kolicina_proizvodov <- podatki_Slovenija %>% group_by(pridelek) %>%  
  ggplot(aes(x=pridelek, y=Kolicina)) + geom_boxplot(color="blue", fill="yellow", alpha=0.3) + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=75,hjust=0.5,vjust=0.9)) +
  ggtitle("Količina ton/ha proizvedbe vsakega proizvoda, Slovenija") + xlab("Vrsta pridelka") + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_proizvodov_na_regio <- podatki_Regija %>% group_by(pridelek) %>%  
  ggplot(aes(x=pridelek, y=Kolicina)) + geom_boxplot(color="black", fill="green", alpha=0.3) + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton/ha proizvedbe vsakega proizvoda, Po regijah") + xlab("Vrsta pridelka") + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_koruze_v_regiji_na_Leto <- podatki_Regija %>% group_by(pridelek) %>% filter(pridelek=="Koruza") %>%
  ggplot(aes(x=leto, y=Kolicina)) + geom_boxplot() + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton/ha proizvedbe koruze, Slovenija") + xlab("Leto proizvoda koruze") + geom_boxplot(color="black", fill="yellow", alpha=0.3) + ylab("Količina t/ha proizvoda")

graf_SLO_Kolicina_breskev_v_regiji_na_Leto <- podatki_Regija %>% group_by(pridelek) %>% filter(pridelek=="Breskve") %>%
  ggplot(aes(x=leto, y=Kolicina)) + geom_boxplot() + theme(axis.text.x = element_text(
    color="#000000", size=8, angle=90)) +
  ggtitle("Količina ton/ha proizvedbe brezkev, Slovenia") + xlab("Leto proizvoda brezkev") + ylab("Količina t/ha proizvoda")


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

podatki <- Slovenija %>% left_join(proizvedba_regij, by="Regija")

zemljevid <- ggplot(data = podatki, aes(x = long, y = lat, fill = Kolicina_v_tonah , label=(Regija))) +
  geom_polygon(aes( group = group))+ geom_text(data=podatki %>% group_by(Regija, Kolicina_v_tonah)  %>% 
                                                 summarise(long=mean(long), lat=mean(lat)), size=3)+
  xlab("") + ylab("") + ggtitle('Količina proizvoda po regijah v miljon tonah') + 
  theme( axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(0,8)) # dodej nekej masesto "4x +06"
zemljevid$labels$fill <- 'Količina proizvoda v miljonih ton'

zemljevid_reduciran <- ggplot() +
  geom_polygon(data = right_join(proizvedba_regij,Slovenija, by = c('Regija')),
               aes(x = long, y = lat, group = group, fill = (Kolicina_v_tonah / Naselja) ))+
  xlab("") + ylab("") + ggtitle('Količina proizvoda po regijah v ton / ha / št. Naselij') + 
  theme( axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_gradient(low = '#25511C', high='#2BFF00') 
zemljevid_reduciran$labels$fill <- 'Količina proizvoda v tonah'



podatki_Slovenija_2 <- podatki_Slovenija %>% group_by(pridelek) %>% filter(Kolicina > 10 )
graf_otroci_indeks <- 
  
ggplot((data = podatki_Slovenija_2), aes(x=leto, y=Kolicina, col= pridelek, group = pridelek))  + ggtitle('Proizvodnja kmetiskih kultur') +  geom_line() + geom_point()
#kako narest da je ta grav tak da ma povezane samo oranyne pike
graf_otroci_indeks <- graf_otroci_indeks + xlab('Leto') + ylab('Količina')
graf_otroci_indeks$labels$fill <- 'Pridelki nad 10 ton/ha'

podatki_Regija2 <- podatki_Regija %>% group_by(pridelek) %>% filter(Kolicina > 15 ) 


#kako narest, da bi bili pridelki povezani z črto koliko se večajo leta horizontalno

graf_indeks <- ggplot(data = podatki_Regija2 , aes(x=Regija, y=Kolicina, color=pridelek, group = pridelek))  + geom_point(aes(frame=leto)) +scale_fill_gradient(low = '#25511C', high='#2BFF00', limits = c(0,30)) + theme(axis.text.x = element_text(
  color="#000000", size=8, angle=75,hjust=0.5,vjust=0.9))
graf_indeks <- graf_indeks + xlab('Pokrajna') + ylab('Količina proizvodov t/ha') 
graf_indeks <- ggplotly(graf_indeks)


sestevek_po_pridelkih <- Skupna_tabela %>% group_by(leto, Regija) %>% summarise(Regija, leto, Kolicina=sum(Kolicina)) %>% unique()
Sestevek_po_pridelkih_regijah  <- sestevek_po_pridelkih %>% group_by(leto) %>% summarise(leto, Kolicina=sum(Kolicina)) %>% unique()

sestevek_po_pridelkih <- left_join(sestevek_po_pridelkih,Skupna_tabela %>% summarise(Regija, Naselja) %>% unique(), by=c('Regija'))
sestevek_po_pridelkih$Kolicina_reduciran <- sestevek_po_pridelkih$Kolicina / sestevek_po_pridelkih$Naselja * 100
graf_indeks_reduciran <- ggplot(data = sestevek_po_pridelkih, aes(x=leto, group = Regija)) + geom_line(aes(y=Kolicina_reduciran,frame=Regija, color="Reducirano * 100")) + geom_line(aes(y = Kolicina,frame=Regija, color="Regija"))+ geom_line(aes(y = Naselja /10,frame=Regija, color="Št. Naselj / 10"))
graf_indeks_reduciran <- graf_indeks_reduciran + xlab('Leto proizvodnje') + ylab('Količina proizvodov t/ha')
graf_indeks_reduciran <- ggplotly(graf_indeks_reduciran)

#??=================




