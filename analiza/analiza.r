# 4. faza: Analiza podatkov





Sestevek_po_pridelkih_regijah$leto <- as.character(Sestevek_po_pridelkih_regijah$leto)
Sestevek_po_pridelkih_regijah$Kolicina <- as.character(Sestevek_po_pridelkih_regijah$Kolicina)


Sestevek_po_pridelkih_regijah <- Sestevek_po_pridelkih_regijah %>% mutate(leto = parse_integer(leto), Kolicina = parse_number(Kolicina))
prileganje <- lm(Kolicina ~ leto, data = Sestevek_po_pridelkih_regijah)
predict(prileganje, data.frame(leto=seq(2010,2022,1)))


graf_napoved <- ggplot(Sestevek_po_pridelkih_regijah, aes(x=leto, y=Kolicina)) + geom_point() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='green') + scale_x_continuous('leto', breaks = seq(2010, 2022, 1), limits = c(2010,2022))


