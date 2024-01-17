---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Säännöllisten lausekkeiden käyttäminen on tapa löytää ja manipuroida tekstiä Bash-skripteissä. Ohjelmoijat käyttävät niitä yleensä, kun he haluavat tarkistaa tai muokata tietynlaisia merkkijonoja.

## Kuinka:
Bash-ohjelmoijien on helppo käyttää säännöllisiä lausekkeita käyttämällä sisäänrakennettua grep-komentoa. Esimerkiksi, jos haluat löytää kaikki tiedostot, jotka päättyvät ".txt": iin, voit käyttää seuraavaa komentoa:

```Bash
grep ".*\.txt$" *
```

Tämä hakee kaikki tiedostot tämän hakemiston, jotka päättyvät ".txt": iin ja tulostaa ne näytölle.

## Syvemmälle:
Säännöllisten lausekkeiden historia juontaa juurensa 1950-luvulle, ja niitä käytettiin alun perin tietotekniikassa tiedontyhjiön manipulointiin. Nykyään on muitakin vaihtoehtoja, kuten awk ja sed, jotka voivat myös käsitellä säännöllisiä lausekkeita. Voit käyttää niitä Bash-skripteissäsi tarpeen mukaan.

## Katso myös:
Lisätietoja Bash-säännöllisistä lausekkeista löytyy Bashin virallisesta dokumentaatiosta sekä muista online-resursseista, kuten sivustoista kuten Stack Overflow.