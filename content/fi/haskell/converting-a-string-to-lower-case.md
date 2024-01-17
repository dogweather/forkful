---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Haskell: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Miksi sinun kannattaa muuttaa merkkijono pieniin kirjaimiin? Koska joskus tarvitset tietyn kaavan tai vertailun, joka ei välitä kirjainten koosta. Esimerkiksi "Apple" ja "apple" ovat eri merkkijonoja, mutta jos haluat verrata niitä toisiinsa, on helpompaa ensin muuttaa molemmat pieniksi kirjaimiksi.

# Kuinka?

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString = map toLower

toLowerString "Hello World!" -- "hello world!"
```
Käytämme `Data.Char` kirjastoa, joka sisältää `toLower` funktion, joka muuttaa yhden merkin pieneksi kirjaimeksi. `toLowerString` funktio ottaa vastaan merkkijonon ja käyttää `map` funktiota soveltamaan `toLower` funktiota jokaiselle merkille. Lopputuloksena saamme uuden merkkijonon, joka sisältää pieniä kirjaimia.

# Syvempi sukellus

Tämä toiminto on ollut olemassa Haskellissa jo alusta asti, ja se on yleinen monissa muissakin ohjelmointikielissä. Joskus saatat törmätä myös `toUpper` funktioon, joka tekee päinvastaisen muunnoksen, eli muuttaa merkkijonon isoiksi kirjaimiksi.

Tämä toiminto on myös saatavilla `Data.Text` kirjastossa, mutta suosittelemme käyttämään `Data.Char` versiota sen yksinkertaisuuden vuoksi.

# Näe myös

[Data.Char - Haskellin dokumentaatio](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)