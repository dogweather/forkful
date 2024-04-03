---
date: 2024-01-20 17:38:52.466993-07:00
description: "How to: Haskellissa saat merkkijonon muutettua pieniksi kirjaimiksi\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `Data.Char` moduulin `toLower` funktiota yhdess\xE4 listank\xE4\
  sittelyn kanssa.\u2026"
lastmod: '2024-03-13T22:44:56.603018-06:00'
model: gpt-4-1106-preview
summary: "Haskellissa saat merkkijonon muutettua pieniksi kirjaimiksi k\xE4ytt\xE4\
  m\xE4ll\xE4 `Data.Char` moduulin `toLower` funktiota yhdess\xE4 listank\xE4sittelyn\
  \ kanssa."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

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
