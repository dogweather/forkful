---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Muuttaa merkkijono pieniksi kirjaimiksi tarkoittaa sitä, että muutetaan merkkijonon kaikki kirjaimet pieniksi kirjaimiksi. Tätä tehdään yleensä verrattaessa tekstiä, silloin kirjainkoon erilaisuudet eivät vaikuta tulokseen.

## Näin teet:

Lua tarjoaa valmiin toiminnon merkkijonon muuttamiseksi pieniksi kirjaimiksi: `string.lower()`. Tässä esimerkki sen käytöstä:

```Lua
local lause = "Moi, Miten Menee?"
print(string.lower(lause))
```

Suoritettaessa tämä koodi, tuloste on:

```
moi, miten menee?
```

## Syvä sukellus

Historiallisesti merkkijonojen muuntaminen pieniksi kirjaimiksi on ensin tehty C-ohjelmointikielessä, josta se on levinnyt muihin kieliin. Lua perii tämän toiminnon C-kielestä. 

Vaihtoehtoina voisi käyttää painotonta tekstivertailua (case-insensitive comparison), mutta tämä ei ole aina mahdollista tai johdonmukaista eri ohjelmointikielissä tai ympäristöissä. Siksi usein valitaan tämä perinteinen tapa.

`string.lower()` toimii hyvin useimmilla länsimaisilla kielillä. Joillakin kielillä, kuten turkissa, ei ole yksinkertaista yhtä-merkkiä-yhteen vastaavuutta isoille ja pienille kirjaimille.

Tämä toiminto vaatii sisäisesti muistinvarauksen ja kopiointia, koska Lua-merkkijonot ovat muuttumattomia (immutable).

## Katso myös

1. Lua 5.4 Reference Manual - string.lower: [linkki](https://www.lua.org/manual/5.4/manual.html#6.4.2)
2. Wikipedia - Case Folding: [linkki](https://en.wikipedia.org/wiki/Case_folding) 

Tarkista nämä lähteet lisätietojen saamiseksi ja syvällisemmän ymmärryksen saavuttamiseksi.