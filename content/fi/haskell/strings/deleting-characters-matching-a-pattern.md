---
date: 2024-01-20 17:42:19.161789-07:00
description: "How to: - Kuinka tehd\xE4: Haskellissa voimme poistaa merkkej\xE4 merkkijonosta\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `filter`-funktiota yhdess\xE4 ehtojen kanssa, jotka m\xE4\
  \xE4rittelev\xE4t,\u2026"
lastmod: '2024-04-05T21:53:58.167832-06:00'
model: gpt-4-1106-preview
summary: ''
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to: - Kuinka tehdä:
```Haskell
-- Ladataan tarvittavat kirjastot
import Data.Char (isSpace)
import Data.List (isInfixOf)

-- Esimerkki: Poistetaan kaikki välilyönnit merkkijonosta
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

-- Käyttö:
main :: IO ()
main = putStrLn $ removeSpaces "Hei Maailma! Tämä on Haskell."

-- Tuloste:
-- "HeiMaailma!TämäonHaskell."

-- Esimerkki: Poistetaan kaikki numerot merkkijonosta
removeDigits :: String -> String
removeDigits = filter (not . isDigit)

-- Käyttö:
main :: IO ()
main = putStrLn $ removeDigits "H4sk3ll 0n k1v4!"

-- Tuloste:
-- "Hskll n kv!"
```
Haskellissa voimme poistaa merkkejä merkkijonosta käyttämällä `filter`-funktiota yhdessä ehtojen kanssa, jotka määrittelevät, mitkä merkit halutaan jättää pois.

## Deep Dive - Syväsukellus:
Haskellissa merkkien suodattaminen on funktio-ohjelmointikonsepti, joka perustuu listojen käsittelyyn. Historiallisesti Haskell on periytynyt ML:n ja Lispin kehittämistä ideoista, ja sen suodatusmekanismit heijastavat funktionaalisen ohjelmoinnin deklaratiivista luonnetta. Vaihtoehtoisesti, voimme käyttää `Data.Text`-moduulia, joka on tehokkaampi suurille teksteille.

Koodin ymmärtämisen kannalta on tärkeä tietää, että `filter` työskentelee korkeamman asteen funktiona, joka ottaa toisen funktion (ehtofunktion) ja listan, ja palauttaa listan, joka sisältää vain alkioita, jotka täyttävät ehtofunktion.

Toteutuksen yksityiskohdissa `filter` on usein osa laajempaa koodirunkoa. Esimerkiksi tiedostojen käsittelyssä tai verkkoliikenteeseen liittyvissä toiminnoissa voidaan käyttää suodatusta hyödyksi poistamalla ei-toivotut merkit ennen datan tallentamista tai sen lähettämistä.

## See Also - Katso myös:
- Haskellin dokumentaatio suodatustoiminnalle: [Haskell filter](http://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:filter)
- `Data.Text` moduuli: [Data.Text documentation](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- Johdatus funktionaaliseen ohjelmointiin: [Learn You a Haskell](http://learnyouahaskell.com/)
- Haskellen regex-kirjastot, kuten `regex-tdfa`: [regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
