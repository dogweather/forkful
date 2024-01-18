---
title:                "Ajan parsiminen merkkijonosta"
html_title:           "Bash: Ajan parsiminen merkkijonosta"
simple_title:         "Ajan parsiminen merkkijonosta"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän erottaminen merkkijonosta on tapa, jolla ohjelmoijat pystyvät muokkaamaan päivämäärädataa merkkijonoista sellaiseen muotoon, jota tietokoneet ymmärtävät ja voivat käsitellä. Tämä on tärkeä taito, koska päivämäärät ovat yleisiä tietoja, jotka tarvitsevat usein muokkaamista ohjelmoinnissa.

## Miten:
```Bash
date -d "10/22/2021"
2021-10-22
```
Esimerkissä näytämme, kuinka päivämäärä merkkijonosta "10/22/2021" muutetaan muotoon, jota tietokone ymmärtää. Tämä tehdään date-komennolla, joka osaa lukea päivämäärämuodot erilaisista merkkijonoista.

```Bash
date +"%a %b %-d, %Y"
Fri Oct 22, 2021
```
Tämä toinen esimerkki näyttää, kuinka voimme muuttaa päivämäärän merkkijonosta haluamaamme muotoon. Käytämme tässä %-d muotoa, joka jättää nolla eteen alle kymmenen päivämäärissä.

## Syvopeilaus:
Päivämäärän erottaminen merkkijonosta on ollut tärkeä taito ohjelmoinnissa jo pitkään. Ennen kuin tietokoneet osasivat lukea päivämääriä, päivämäärät esitettiin monilla eri tavoilla ja niiden muokkaaminen oli haastavaa.

On olemassa myös muita tapoja muokata tai esittää päivämäärädataa. Jotkut ohjelmoijat käyttävät esimerkiksi Unix-timestampia, joka esittää päivämäärän sekunteina vuodesta 1970 lähtien. Tämä tapa on kuitenkin vaikeampi ymmärtää ihmisille.

Date-komennolla on myös paljon muita vaihtoehtoja, joilla voi muokata päivämäärämuotoa haluamakseen. Esimerkiksi voit käyttää "date --help" nähdäksesi kaikki saatavilla olevat vaihtoehdot.

## Katso myös:
Date-komennon manuaalisivu: https://linux.die.net/man/1/date
Unix-timestamp: https://en.wikipedia.org/wiki/Unix_time