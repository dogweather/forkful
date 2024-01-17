---
title:                "Muuntaa päivämäärä merkkijonoksi"
html_title:           "Gleam: Muuntaa päivämäärä merkkijonoksi"
simple_title:         "Muuntaa päivämäärä merkkijonoksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän muuttaminen merkkijonoksi tarkoittaa, että muutamme päivämäärän tiedon tietokoneen ymmärtämään muotoon. Tämä on hyödyllistä, koska tietokoneet käsittelevät datoja eri tavalla kuin ihmiset. Ohjelmoijat muuttavat päivämääriä merkkijonoiksi, jotta tietokoneet voivat käsitellä niitä oikein ja näyttää ne käyttäjälle.

## Kuinka:

Seuraavassa esimerkissä näytämme miten muutat Gleamillä päivämäärän merkkijonoksi:

```
Gleam
import Gleam.Date

Gleam.Date.to_string({year: 2021, month: 4, day: 15})

```

Tulostus: "2021-04-15"

Voit myös muuttaa päivämäärän merkkijonoksi haluamallasi tavalla, esimerkiksi näin:

```
Gleam.Date.to_string({year: 2021, month: 4, day: 15}, format="%d.%m.%Y")

```

Tulostus: "15.04.2021"

## Deep Dive:

Päivämäärän muuttaminen merkkijonoksi on yleinen tehtävä monissa ohjelmoinnin kielissä. Se auttaa ohjelmoijia käsittelemään tietoja ja esittämään niitä käyttäjille. Aiemmin tämä tehtiin usein manuaalisesti monimutkaisten algoritmien avulla, mutta nykyään monilla kielillä on valmiita kirjastoja, kuten Gleamilla, jotka tekevät tämän tehtävän helpoksi.

Vaihtoehtoisesti ohjelmoijat voivat käyttää erilaisia merkkijonomuotoja päivämääriä ja kellonaikoja varten, kuten ISO 8601-standardia tai maakohtaisia muotoja. Näiden eroavaisuuksien vuoksi on tärkeää käyttää oikeaa muotoa sen mukaan, mitä tietokoneen tulisi käyttäjälle näyttää.

Gleamin Date-moduuli hyödyntää Erlangin kalenterikirjastoa päivämäärien käsittelyyn. Tämä tarkoittaa, että päivämäärän muuttaminen merkkijonoksi toimii samaan tapaan kuin Erlangissakin.

## Katso myös:

Voit lukea lisää Gleamin Date-moduulista [virallisesta dokumentaatiosta] (https://gleam.run/documentation/standard-library/date) tai tutustua enemmän erilaisiin päivämääränmuutostapoihin Gleamissa [Gleamin oppaan] (https://gleam.run/book/tutorials/handling-date-time) avulla.