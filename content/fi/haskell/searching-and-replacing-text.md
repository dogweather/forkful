---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tekstin hakeminen ja korvaaminen on toiminto, joka etsii tietyt merkkijonot ja korvaa ne sitten uusilla. Koodaajat käyttävät tätä metodia ohjelmoinnissa manipuloidakseen merkkijonoja ja korjatakseen virheitä.

## Miten:
Tässä on Haskell esimerkkikoodi - koodi joka etsii ja korvaa merkkijonot:

```Haskell
import Data.List.Utils

replaceText :: String -> String -> String -> String
replaceText old new = join new . split old

main :: IO ()
main = putStrLn $ replaceText "vanha" "uusi" "Tämä on vanha teksti."
```

Käynnistä ohjelma, se tulostaa: "Tämä on uusi teksti."

## Syvä sukellus:
Historiallisesti, merkkijonojen korvaaminen on ollut ohjelmoinnin tärkeä toiminto, sen juuret ulottuvat takaisin varhaiseen konekoodaukseen.

Vaihtoehtoisia menetelmiä tekstin korvaamiseen Haskellissa ovat esimerkiksi regex-kirjastot tai Text.ICU-kirjasto. Valinta riippuu kuitenkin vaatimuksista ja henkilökohtaisista mieltymyksistä.

Haskell toteuttaa merkkijonojen korvaamisen luomalla väliaikaisen listan, joka sisältää kaikki erilliset tekstin osat (split function), nämä osat yhdistetään sitten uudestaan uuden merkkijonon kanssa (join function).

## Katso myös:
Tässä on muutamia linkkejä, joista saa lisätietoja:
- Tekstin korvaaminen Haskelissa: https://stackoverflow.com/questions/30551033/replace-a-substring-in-haskell 
- Data.List.Utils dokumentaatio: http://hackage.haskell.org/package/MissingH-1.4.3.0/docs/Data-List-Utils.html
- Text.ICU kirjasto: http://hackage.haskell.org/package/text-icu