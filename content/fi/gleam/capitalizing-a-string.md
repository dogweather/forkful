---
title:                "Merkkijonon pääkirjaintaminen"
html_title:           "Gleam: Merkkijonon pääkirjaintaminen"
simple_title:         "Merkkijonon pääkirjaintaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kirjainten muuttaminen isoiksi on ohjelmoinnissa käytetty toiminto, jolla muutetaan annetun merkkijonon kaikki kirjaimet isoiksi kirjaimiksi. Tämä on hyödyllistä esimerkiksi vakiomuotoistamisessa ja yhtenäisyyden luomisessa tiedon käsittelyssä.

## Näin se tehdään:
Gleam kielen avulla merkkijonon voi muuttaa isoiksi kirjaimiksi käyttämällä `to_uppercase` funktiota. Tässä esimerkki ja sen tuottama tulos:

```GLEAM
import gleam/str

pub fn main() {
  let kotona = "olen kotona"
  let tulostus = str.to_uppercase(kotona)
  io.println(tulostus)
}
```
Tulostus:
```
OLEN KOTONA
```

## Syvempi tarkastelu
Historiallisessa kontekstissa kirjainten muuttaminen isoiksi on pitkäaikainen ohjelmointikäytäntö, joka juontaa juurensa aikoihin, jolloin tietokoneiden näytöt kykenivät tuottamaan vain suuria kirjaimia. Gleam tarjoaa helposti käytettävän `to_uppercase` funktion merkkijonojen muuttamiseen isoiksi kirjaimiksi. Vaihtoehtona on luoda oma funktio, mutta se on yleensä tarpeetonta, koska Gleam tarjoaa jo valmiin ja tehokkaan työkalun. 

## Katso myös
Gleam-koodin kirjoittamista ja ymmärtämistä voi oppia lisää seuraavista lähteistä:
1. [Gleam's str module documentation](https://docs.gleam.run/stdlib/str.html): Auttaa ymmärtämään Gleam-kielen `str`-moduulin tarjoamia funktioita.
2. [Functional programming in Gleam](https://gleam.run/book/tour/functional-programming.html): Tarjoaa perusteellisen katsauksen funktionaaliseen ohjelmointiin Gleam-kielellä.