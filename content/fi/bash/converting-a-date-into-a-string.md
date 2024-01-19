---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän muuntaminen merkkijonoksi on operaatio, jossa päivämääräobjekti esitetään luettavassa muodossa. Tätä suoritetaan usein, jotta päivämäärät voidaan helposti tallentaa, lajitella ja vertailla.

## Näin se tehdään:

Bashissa päivämäärän muuntaminen merkkijonoksi on yksinkertaista. Käytämme `date`-komentoa ja formatoitua ulostuloa:

```Bash
# Nykyinen päivämäärä ja aika
date +"%Y-%m-%dT%H:%M:%S"
```
Esimerkiksi tuloste voisi olla `2023-03-28T12:13:14`.

## Syvempi sukellus

Historiallisesti Unix-tyyliset järjestelmät ovat käyttäneet `date`-komentoa ja sen formaatteja. Vaihtoehtoisina menetelminä voidaan käyttää muita komentoja tai kielikirjastoja, kuten Pythonin `datetime`.

Muunnos perustuu suoraan C-kielen `strftime`-funktioon. Sitä muutetaan kuvauksen mukaan, määrittämällä aikaleimat formatoiduissa merkkijonoissa.

## Katso myös

Seuraavat linkit ovat hyödyllisiä tutkittaessa lisää:

1. Bash: https://www.gnu.org/software/bash/
2. strftime: http://man7.org/linux/man-pages/man3/strftime.3.html
3. Python datetime: https://docs.python.org/3/library/datetime.html
4. GNU Core Utilities (`date` sivu): https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html