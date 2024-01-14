---
title:                "Clojure: Merkkijonon muuttaminen isoin kirjaimin"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Englannin kielessä on tärkeää käyttää sanojen alkukirjaimia oikein, ja joskus tarvitaan myös muuttaa tekstin muotoa. Clojure-ohjelmointikielessä on sisäänrakennettu toiminto nimeltä `capitalize`, joka muuttaa annetun merkkijonon ensimmäisen kirjaimen isoiksi kirjaimiksi. Tässä blogipostauksessa opimme mikä `capitalize` on ja miksi sitä käytetään.

## Miten

Clojure-ohjelmointikielissä koodin suorittamiseen käytetään usein REPL-työkalua. Voimme kokeilla `capitalize`-toimintoa helposti REPL-työkalun avulla.

```Clojure
(capitalize "heimo")
```

Tämä koodinpätkä tuottaa tuloksen "Heimo". `capitalize` toimii myös tyhjissä merkkijonoissa:

```Clojure
(capitalize "")
```

Tämä koodinpätkä tuottaa tuloksen "" eli tyhjän merkkijonon. Toiminto ei myöskään vaikuta muihin merkkijonon kirjaimiin, vaan ainoastaan ensimmäinen kirjain muutetaan isoksi.

## Syväsukellus

`capitalize` toiminto hyödyllinen esimerkiksi silloin kun käsittelemme käyttäjän antamaa dataa ja haluamme varmistua, että merkkijonon ensimmäinen kirjain on iso. Voimme myös käyttää toimintoa yhdessä `map`-toiminnon kanssa, jolloin voimme muuttaa useita merkkijonoja kerralla.

Toinen tapa muuttaa merkkijonon ensimmäinen kirjain isoksi on käyttää `clojure.string/capitalize` toimintoa. Tämä toiminto löytyy `clojure.string` -kirjastosta ja on hyödyllinen esimerkiksi silloin kun käsittelemme useita merkkijonoja ja haluamme käyttää samaa muotoilua kaikille.

## Katso myös

- [Clojure - virallinen sivusto](https://clojure.org/)
- [CLJ-kurssit](https://clojure.org/guides/getting_started)li>
- [Clojure-repl työkalu](https://repl.it/languages/clojure)