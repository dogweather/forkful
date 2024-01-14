---
title:                "Gleam: Nykyisen päivämäärän hakeminen"
simple_title:         "Nykyisen päivämäärän hakeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa on tarve käyttää tietoa nykyisestä päivästä. Tämä voi olla esimerkiksi päivämäärän näyttäminen käyttöliittymässä tai tietyn ajankohtaan liittyvän toiminnon suorittaminen. Onneksi Gleam tarjoaa helpon tavan saada nykyinen päivämäärä ohjelmassa käyttämällä sisäänrakennettua `gleam/time` kirjastoa.

## Miten

Voit hankkia nykyisen päivämäärän Gleamilla käyttämällä `gleam/time` kirjaston funktiota `now`. Tämä palauttaa nykyisen ajan Gleam `Time` tietorakenteena, josta voit hakea erilaisia tietoja, kuten vuosi, kuukausi tai päivä. Tässä on esimerkki, miten voit käyttää `now` funktiota:

```gleam
import gleam/time

let now = time.now()

time.Year(now) // Palauttaa nykyisen vuoden
time.Month(now) // Palauttaa nykyisen kuukauden
time.Day(now) // Palauttaa nykyisen päivän
```

Sen lisäksi, voit myös tulostaa nykyisen päivämäärän suoraan konsoliin käyttämällä `gleam/io` kirjastoa:

```gleam
import gleam/time
import gleam/io

let now = time.now()

io.format("{:04}/{:02}/{:02}", [time.Year(now), time.Month(now), time.Day(now)]) // Tulostaa nykyisen päivämäärän muodossa "yyyy/mm/dd"
```

## Syvällisempi sukellus

`gleam/time` kirjastossa on myös muita hyödyllisiä funktioita nykyisen ajan käsittelyyn, kuten `to_unix` joka muuntaa Gleam `Time` tietorakenteen Unix-timestampiksi, sekä `parse` joka muuntaa merkkijonon päivämääräksi. Voit tutustua kaikkiin tarjolla oleviin funktioihin [dokumentaatiosta](https://gleam.run/modules/gleam_time/latest/).

## Katso myös

- [Gleamin dokumentaatio](https://gleam.run/) - Lisää tietoa Gleamista ja sen käytöstä.
- [Gleam/time kirjaston dokumentaatio](https://gleam.run/modules/gleam_time/latest/) - Lisätietoa `gleam/time` kirjastosta ja sen tarjoamista funktioista.
- [Io/format funktion dokumentaatio](https://gleam.run/modules/gleam_io/latest/#type.formatter) - Yksityiskohtainen selitys `io/format` funktion käytöstä.