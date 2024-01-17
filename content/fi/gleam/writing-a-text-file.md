---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Gleam: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstitiedoston kirjoittaminen on yksinkertainen ja tärkeä osa ohjelmointia. Se tarkoittaa tekstin tallentamista tiedostoon tietokoneella, jota voidaan sitten käyttää myöhemmin. Ohjelmoijat tekevät sitä esimerkiksi tallentaakseen käyttäjän tietoja tai tallentaakseen tietoja ohjelman toiminnan aikana.

## Miten:

Gleamilla tekstitiedoston kirjoittaminen on helppoa. Käytä funktiota ```File.write``` ja anna tiedoston nimi ja teksti, jonka haluat tallentaa.

```Gleam
let texti = "Hei maailma!"
File.write("tekstitiedosto.txt", texti)
```

Tämä koodi luo ```tekstitiedosto.txt``` nimisen tiedoston ja tallentaa siihen tekstin "Hei maailma!".

## Syvällisempi sukellus:

Tekstitiedoston kirjoittaminen on ollut osa ohjelmointia jo pitkään. Ennen Gleamia, jotkut ohjelmoijat käyttivät C-kielen tiedostokirjastoa, mutta Gleamilla tämä on paljon helpompaa. On myös muita vaihtoehtoisia tapoja tallentaa tietoa, kuten tietokantoihin tai pilvipalveluihin. Tämä voi olla kätevämpiä tietyissä tilanteissa, mutta tekstitiedostoilla on edelleen tärkeä rooli ohjelmoinnissa.

## Katso myös:

Lisää tietoa Gleamin tiedostokirjoituksesta löydät Gleamin virallisesta dokumentaatiosta: https://gleam.run/stdlib/file.html#write

Toinen hyödyllinen artikkeli Gleamin perusteista löytyy täältä: https://gleam.run/articles/basic/