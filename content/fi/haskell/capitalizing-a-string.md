---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Mikä & Miksi?
Stringin kapitalisointi muuttaa tekstin kirjaimet isoiksi kirjaimiksi. Ohjelmoijat käyttävät sitä datan yhtenäistämiseen ja formaattien standardointiin, kuten nimien ja otsikoiden muodostukseen.

## How to: / Kuinka tehdä:
```Haskell
import Data.Char(toUpper)

-- Perusfunktio stringin kapitalisointiin
capitalize :: String -> String
capitalize = map toUpper

-- Esimerkki ja tuloste
main :: IO ()
main = putStrLn (capitalize "hei maailma")

-- Tuloste
-- "HEI MAAILMA"
```

## Deep Dive / Sukellus syvyyksiin:
Stringin kapitalisointi on Haskellissa selvää kauraa: käytä `map`-funktiota yhdessä `toUpper`-funktion kanssa `Data.Char`-kirjastosta. Historiallisesti tämä lähestymistapa on perua funktio-ohjelmoinnista, jossa dataa käsitellään yhdistelemällä funktioita. Vaihtoehtoisia tapoja voi löytää kirjoittelemalla omia funktioita tai käyttämällä muita kirjastoja, mutta `map` ja `toUpper` ovat tehokas pari.

Haskell ei suoraan sisällä merkkijonon kapitalisointifunktiota, joten yksinkertainen `capitalize`-funktio onkin usein itse tehty. Kielen luonteen vuoksi tämä opettaa ohjelmoijille toiminnallisen ajattelun tapoja, kun he muovautuvat käyttämään olemassa olevia funktioita uusiin tarkoituksiin.

Ohjelmoijat käyttävät joskus `toUpper`-funktioon vastinparina `toLower`-funktiota pienten kirjainten tuottamiseen. Muita kirjastoja voivat olla mukana esimerkiksi käsittelemässä monimutkaisempia tekstiformaatteja tai lokaaliin liittyviä erikoistapauksia.

## See Also / Lisätietoja:
- Haskell Documentation for `Data.Char`: [Haskell Data.Char](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)
- Functional Programming Basics: [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
