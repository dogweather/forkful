---
title:                "Gleam: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kukaan haluaisi laskea tulevaisuuden tai menneisyyden päivämäärän? Ehkäpä tarvitset päivämäärän tarkistamista varten, kuten tärkeän tapahtuman tai muistettavan päivän muistamiseen.

## Miten 
Koodiesimerkkien ja tuotosten avulla näytämme, kuinka voit laskea tulevia tai menneitä päivämääriä ```Gleam```-ohjelmointikielen avulla.

#### Laske tuleva päivämäärä
```Gleam
import gleam/time

let tuleva_päivä = time.add_days(time.now(), 30)

IO.print("Tuleva päivämäärä: ", time.format(tuleva_päivä, "%d.%m.%Y"))
```
Tuotos:
> Tuleva päivämäärä: 23.11.2021

#### Laske menneisyys päivämäärä
```Gleam
import gleam/time

let menneisyys_päivä = time.subtract_days(time.now(), 100)

IO.print("Menneisyys päivämäärä: ", time.format(menneisyys_päivä, "%d.%m.%Y"))
```
Tuotos:
> Menneisyys päivämäärä: 18.08.2021

## Syvällisempi sukellus
Päivämäärien laskeminen tulevaisuuteen tai menneisyyteen voi olla hyödyllistä monissa tapauksissa, kuten kalenterisovellusten kehittämisessä tai ajastettujen muistutusten ohjelmoinnissa. Gleam tarjoaa monipuoliset toiminnot päivämäärien käsittelyyn, kuten päivien, kuukausien ja vuosien lisäämistä ja vähentämistä.

Lisäksi Gleamilla on käytettävissä erilaisia muotoiluvaihtoehtoja päivämäärien esittämiseen halutussa muodossa, kuten päivämäärän osoittimena tai päivämäärän kirjaimellisena esityksenä. Näiden toimintojen avulla päivämäärien manipulointi ja esittäminen on helppoa ja tarkkaa.

## Katso myös
- [Gleam documentation on time module](https://gleam.run/documentation/#time-module)
- [Gleam documentation on date formatting](https://gleam.run/documentation/#format-date-string-flags)

Kiitos lukemisesta! Toivottavasti tämä opas auttoi sinua ymmärtämään, kuinka laskea päivämääriä tulevaisuuteen tai menneisyyteen Gleamin avulla. Jatka tutustumista Gleamiin ja sen moniin mahdollisuuksiin!