---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:38:52.466993-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Merkkijonon muuntaminen pieniksi kirjaimiksi tarkoittaa sen kirjainsarjan konvertoimista vastaaviin pieniin kirjaimiin. Tämä on hyödyllistä, kun halutaan vertailla sanoja riippumatta alkuperäisestä kirjoitusasuista tai tehdä tekstihaku herkäksi kirjainkoosta.

## How to:
Haskellissa saat merkkijonon muutettua pieniksi kirjaimiksi käyttämällä `Data.Char` moduulin `toLower` funktiota yhdessä listankäsittelyn kanssa. Tässä on miten se tehdään:

```haskell
import Data.Char (toLower)

lowercaseStr :: String -> String
lowercaseStr str = map toLower str

-- Esimerkkikäyttö
main :: IO ()
main = putStrLn (lowercaseStr "Moikka Maailma!")
```

Esimerkkikäyttö tulostaa:

```
moikka maailma!
```

## Deep Dive
Muuntaessa merkkijonoja pieniin kirjaimiin Haskellissa käytetään yleisesti `Data.Char`-moduulia, joka sisältää `toLower` funktion. Tämä lähestymistapa on tehty mahdolliseksi vuodesta 2003, kun Haskell 98-standardi laajennettiin includeamaan `Data.Char`.

Alternativeina, voit käyttää ulkopuolisia kirjastoja kuten `text` tai `bytestring`, jos työskentelet spesifisten tekstityyppien kanssa. Nämä kirjastot tarjoavat omat funktionsa merkkijonojen muuntamiseen, jotka voivat olla tehokkaampia suurille datamassoille.

Implementointi yksityiskohtana, `toLower`-funktio itse on yksinkertainen. Se ottää yhden merkin ja palauttaa vastaavan pienikirjaimisen merkin. Listankäsittelykeinona `map` soveltuvasti ajaa `toLower` jokaiselle merkille jonossa.

## See Also
Haskellin dokumentaatio `Data.Char`-moduulille: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html

`text`-kirjasto: https://hackage.haskell.org/package/text

`bytestring`-kirjasto: https://hackage.haskell.org/package/bytestring

Haskell oppaita ja tutoriaaleja: https://www.haskell.org/documentation/