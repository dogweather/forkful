---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Gleam: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Stringien muuntaminen pieniksi kirjaimiksi Gleamilla

## Mikä & Miksi?

Stringien muuttaminen pieniksi kirjaimiksi on operaatio, jossa isoista kirjaimista tehdään pieniä kirjaimia. Ohjelmoijat tekevät näin, jotta he voisivat vertailla stringejä luotettavasti riippumatta kirjainkoosta. 

## Kuinka:

Tämä on yksinkertainen Gleam-koodiesimerkki, joka muuntaa stringin pieniksi kirjaimiksi.

```Gleam
import gleam/string

let r = string.lowercase("Hei MAailma!")
```

Ohjelman suorittaminen tulostaisi "hei maailma!".

## Syvempi sukellus

Aikaisemmin, ennen kuin ohjelmointikielissä oli valmiit funktiot stringien muuntamiseen pieniksi kirjaimiksi, ohjelmoijat tekivät tämän matalan tason kielissä itse. 

Vaihtoehtoisesti, voit luoda oman funktion, joka käyttää Unicode-taulukoita muuntamaan isoja kirjaimia pieniksi. Tämä voi olla hyödyllistä, jos sinun pitää käsitellä erikoisia merkkejä tai kieliä, joita standardi lower case -funktio ei tue.

Gleamissa, `string.lowercase` on toteutettu käyttäen Erlangin `unicode:characters_to_lower` -funktiota, joka tukee kaikkia Unicode merkkejä.

## Katso Myös

Lisätietoa Gleamista ja sen string-funktioista voi löytyä seuraavista lähteistä:

- Gleam Docs: https://hexdocs.pm/gleam_stdlib/gleam/string.html
- Unicode in Erlang: http://erlang.org/doc/man/unicode.html
- How to compare strings: https://www.baeldung.com/cs/string-comparison-convert-to-lowercase