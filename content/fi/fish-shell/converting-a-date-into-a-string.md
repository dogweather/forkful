---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Muuntaa päivämäärän merkkijonoksi tarkoittaa päivämäärän esittämistä luettavassa muodossa. Ohjelmoijat tekevät tämän siksi, että päivämäärät ovat ymmärrettäviä ja helposti käsiteltäviä ikään kuin ne olisivat normaaleja tekstejä.

## Näin teet:

Alla on esimerkki siitä, kuinka muuntaa päivämäärä merkkijonoksi Fish Shellissä:

```Fish Shell
set -l nykyinen_pvm (date "+%d-%m-%Y")
echo $nykyinen_pvm
```

Kun ajetat tämän, se näyttää jotain tältä:

```Fish Shell
20-11-2021
```

## Syvällisemmin:

Muuntaa päivämäärän merkkijonoksi -konsepti on ollut olemassa jo pitkään, koska se on olennainen osa päivämäärien käsittelyä ohjelmistokehityksessä. Erilaisia kirjastoja ja kieliominaisuuksia on olemassa tätä tarkoitusta varten eri ohjelmointikielissä. Fish Shell käyttää GNU-Coreutilit 'date'-komentoa, joka antaa meille mahdollisuuden määrittää muodot päivämäärien muuntamiseksi.

Vaihtoehtoisesti, jos haluat määrittää tietyn aikavyöhykkeen, voit tehdä sen seuraavasti:

```Fish Shell
set -l nykyinen_pvm (env TZ=Europe/Helsinki date "+%d-%m-%Y %T %Z")
echo $nykyinen_pvm
```

Esimerkki tulostaa päivämäärän muodossa 'dd-mm-YYYY HH:MM:SS TZ', joka on määritelty aikavyöhykkeeseen 'Europe/Helsinki'.

## Katso myös:

2. GNU Coreutils Date Documentation: [Link here](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)