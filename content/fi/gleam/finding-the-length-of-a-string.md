---
title:                "Gleam: Merkkijonon pituuden löytäminen"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa tarvitset selvittää merkkijonon pituuden. Ehkä kehität ohjelmaa, joka tarkistaa salasanasi pituuden, tai ehkä haluat tietää kuinka monta merkkiä voit lähettää tekstiviestissä. Olipa syy mikä tahansa, Gleamilla on helppo tapa löytää merkkijonon pituus.

## Kuinka tehdä

Käytä Gleamin integroituja merkkijonon pituus -funktioita löytääksesi merkkijonon pituuden. Tässä on esimerkki koodista ja tulosteesta:

```Gleam
let str = "Hei kaikki!"

let length = String.length(str)

/* Tuloste:
 Kaikkien merkkien määrä on 11
*/
```

## Syvällinen sukellus

Voit myös käyttää Gleamin nimikirjastoa löytääksesi merkkijonon pituuden. Tämä tarjoaa enemmän joustavuutta ja tarkkuutta merkkijonon pituuden määrittämisessä. Voit esimerkiksi ottaa huomioon myös välilyönnit ja erikoismerkit.

```Gleam
import gleam/string

let str = "Hei kaikki!"

let length = string.length(str)

/* Tuloste:
Kaikkien merkkien määrä on 10
*/

```

## Katso myös

- [Gleamin merkkijonon pituuden dokumentaatio](https://gleam.run/documentation/standard-library/string.html#length)
- [Gleamin nimikirjaston dokumentaatio](https://gleam.run/documentation/standard-library/string.html)