---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:12:54.278240-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Komentorivillä nykyisen päivämäärän hakeminen on pientä digitalista taikuutta: näpäytä komento ja näyttöön ilmestyy päiväys. Miksi? Skriptit käyttävät päivämääriä lokien luomiseen, tiedostonimien merkkaamiseen ja aikasidonnaisten tehtävien automatisointiin.

## How to: (Kuinka tehdään:)
```Bash
# Peruskomento päivämäärän näyttämiseen
date

# Esimerkkituloste
pe maali 12 15:23:08 EET 2023

# Mukautettu muotoilu
date "+%Y-%m-%d"

# Esimerkkituloste
2023-03-12

# Aikaleiman lisääminen tiedostonimeen
echo "Tämä on esimerkkitiedosto" > "tiedosto_$(date "+%Y-%m-%d").txt"
```

## Deep Dive (Sukellus syvyyksiin):
Unix-tyyppisissä käyttöjärjestelmissä `date` on perinyt geeneissään 70-luvun alusta asti ajan ja päivämäärien käsittelyn. Vaihtoehtoja? `strftime` C-ohjelmointikielessä tai modernit skriptikielet, kuten Python, jossa `datetime`-moduuli. Unix-aika, aikaleimat sekä alueelliset aika-asetukset ovat osa `date`:n taustamagiaa.

## See Also (Katso myös):
- `man date`: Komentorivin ohje `date`-komennolle
- GNU Coreutils: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/x23170.html
- Linux Date Command Tutorial for Beginners (8 Examples): https://www.howtoforge.com/linux-date-command/
