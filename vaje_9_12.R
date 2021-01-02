library(readr)
library(tidyr)
druzine <- read_csv("druzine.csv", col_names=c("obcina", 1:4), skip=1,
                    locale=locale(encoding="Windows-1250")) %>%
  pivot_longer(-obcina, names_to="velikost", values_to="stevilo") %>%
  mutate(velikost=parse_number(velikost))

lvls <- levels(obcine$OB_UIME) %>% sort()
druz <- unique(druzine$obcina) %>% sort()
razlicni <- lvls != druz

primerjava <- data.frame(obcina.zemljevid=lvls,
                         obcina.druzine=druz,
                         stringsAsFactors=FALSE)[razlicni, ]

library(stringr)
levels(obcine$OB_UIME) <- levels(obcine$OB_UIME) %>%
  str_replace("Slov[.]", "Slovenskih")
druzine <- druzine %>% mutate(obcina=obcina %>%
                                str_replace("/.*", "") %>%
                                str_replace(" - ", "-") %>%
                                str_replace("Slov[.]", "Slovenskih") %>%
                                parse_factor(levels(obcine$OB_UIME)))