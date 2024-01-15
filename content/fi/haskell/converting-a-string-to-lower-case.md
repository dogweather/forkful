---
title:                "Muunna merkkijono pienaakkoseksi"
html_title:           "Haskell: Muunna merkkijono pienaakkoseksi"
simple_title:         "Muunna merkkijono pienaakkoseksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Kaikki meistä ovat jossain vaiheessa törmänneet tarpeeseen muuttaa merkkijono pienaakkosiksi. Olipa kyse sitten tekstin muokkaamisesta tai tietokantahakuja suorittaessa, on tärkeää tietää, miten tämä tehdään tehokkaasti ja oikein.

## Miten

Merkkijonon muuttaminen pienaakkosiksi on helppoa ja nopeaa käyttämällä vain muutamaa rivikoodia. Kaikki mitä tarvitset on `toLower`-funktio, joka on osa Haskellin `Data.Char`-moduulia.

```Haskell
import Data.Char

toLower "TÄMÄ ON MALLIMERKIT merkkijono" 
--tulostaa "tämä on mallimerkit merkkijono"
```

Kuten nähdään esimerkistä, `toLower` muuttaa kaikki merkkijonon isot kirjaimet pieniksi.

## Syvällinen sukellus

Haskelin `toLower`-funktio käyttää Unicode-standardia muuntaessaan merkkijonon pienaakkosiksi. Tämä tarkoittaa, että jos merkkijono sisältää kansainvälisiä tai erikoismerkkejä, ne muutetaan myös näiden sääntöjen mukaan.

On myös tärkeä huomata, että `toLower` toimii vain merkkijonoilla. Jos haluat muuttaa yksittäisen merkin pienaakkoseksi, voit käyttää `toLower`-funktiota yhden kirjaimen sisältävälle listalle (esim. `[c]`). Tämä palauttaa yksittäisen merkin, joka on muutettu pienaakkoseksi.

## Katso myös

- [Haskellin viralliset verkkosivut](https://www.haskell.org/)
- [Haskellin oppikirja](http://learnyouahaskell.com/)
- [Haskellin Data.Char-moduulin dokumentaatio](https://hackage.haskell.org/package/base/docs/Data-Char.html)