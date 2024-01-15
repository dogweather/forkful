---
title:                "Kahden päivämäärän vertailu"
html_title:           "Bash: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Kahden päivämäärän vertaileminen on tärkeää monissa ohjelmointiprojekteissa, kuten kalenterisovelluksissa tai tapahtumienjärjestämissä työkaluissa. Vertailemalla päivämääriä voidaan esimerkiksi tarkistaa, onko tietty tapahtuma jo mennyt tai onko se vasta tulossa.

## Kuinka tehdä

Vertailemalla kahta päivämäärää Bashilla voi olla monia erilaisia sovelluskohteita, mutta alla on yksinkertainen esimerkki, joka näyttää kuinka vertailla kahta päivämäärää ja tulostaa ero päivissä.

```Bash
# Alustetaan muuttujat
date1="2021-01-01"
date2="2021-01-05"

# Muutetaan päivämäärät sekunneiksi ja lasketaan niiden erotus
date1_sec=$(date -d "$date1" +%s)
date2_sec=$(date -d "$date2" +%s)
diff_sec=$((date2_sec - date1_sec))

# Muutetaan sekunnit päiviksi
days_diff=$((diff_sec / 86400))

# Tulostetaan tulos
echo "Ensimmäisen päivämäärän ja toisen päivämäärän ero on $days_diff päivää."
```

**Tuloste:**

```
Ensimmäisen päivämäärän ja toisen päivämäärän ero on 4 päivää.
```

## Syvällinen sukellus

Bashilla päivämäärien vertaileminen onnistuu helposti muutamalla komennolla. Käytettäessä `date -d` komentoa, voimme muuttaa päivämäärät sekunneiksi ja siten helposti vertailla niitä eri tavoin. Jos haluamme vertailla päivämääriä kahden eri aikavyöhykkeen välillä, voimme käyttää `TZ` muuttujaa määrittelemään halutun aikavyöhykkeen.

Tarkempaa tietoa `date` komennosta ja sen käyttömahdollisuuksista löytyy esimerkiksi Bashin virallisesta dokumentaatiosta.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/bash.html)
- [TZ muuttujan käyttö päivämäärien vertailussa](https://www.gnu.org/software/bash/manual/bash.html#Shell-Variables)
- [Lisätietoa `date` komennosta](https://www.gnu.org/software/bash/manual/bash.html#Bash-Conditional-Expressions)