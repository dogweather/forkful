---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Elm: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi? 

Mikä on merkkijonon pituuden löytäminen ja miksi ohjelmoijat tekevät sitä? Merkkijonon pituuden löytäminen tarkoittaa yksinkertaisesti merkkijonon merkkien lukumäärän laskemista. Tätä taitoa tarvitaan usein, kun halutaan esimerkiksi tarkistaa, että käyttäjän antama salasana täyttää minimipituusvaatimuksen tai kun käsitellään tekstin syötteitä.

## Miten:

```Elm
length "Tämä on merkkijono"
-- Output: 21
```

Älä huoli, vaikka et ymmärtäisi täysin yllä olevaa koodilohkoa. Se on vain yksi tapa käyttää Elm-ohjelmointikielen tarjoamaa `length`-funktiota löytääksesi merkkijonon pituuden.

Toinen tapa voisi olla esimerkiksi käyttää `String.length` funktiota, kuten alla näemme:

```Elm
String.length "Toinen tapa"
-- Output: 12
```

## Syvemmälle:

Nyt kun tiedämme, miten löytää merkkijonon pituus Elm:llä, voisimme miettiä hieman lisää taustaa. Merkkijonon pituuden löytäminen on yksi yleisimmistä ohjelmoinnissa tehtävistä toiminnoista ja siksi lähes jokainen ohjelmointikieli tarjoaa siihen valmiin funktion tai tavan.

Esimerkiksi JavaScriptissä voimme käyttää `length`-ominaisuutta seuraavasti:

```JavaScript
"Tämäkin on merkkijono".length
// Output: 23
```

Kuten huomaat, käytettävän kielen syntaksi ja toimintatapa saattavat vaihdella, mutta lopputulos on aina sama - merkkijonon pituus.

## Katso myös:

Jos haluat lukea lisää merkkijonon pituuden laskemisesta, suosittelemme lukemaan tarkemmin Elm-ohjelmointikielen `String`-kirjaston dokumentaatiota, joka tarjoaa lisätietoa erilaisista merkkijonoihin liittyvistä toiminnoista. Voit myös katsoa muita ohjelmointikieliä ja niiden tarjoamia ratkaisuja tämän ongelman ratkaisemiseksi.