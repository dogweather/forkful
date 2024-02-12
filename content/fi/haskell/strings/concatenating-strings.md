---
title:                "Merkkijonojen yhdistäminen"
aliases:
- /fi/haskell/concatenating-strings/
date:                  2024-01-20T17:35:12.789124-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? | Mikä & Miksi?
Yhdistelemme merkkijonoja (eli teemme konkatenointia) liittääksemme tekstinpätkiä yhteen. Se on peruskauraa kaikessa ohjelmoinnissa: loihdit viestejä, teet tulosteista luettavia tai kasaat dataa käsittelyä varten.

## How to: | Näin teet:
Haskellissa on suoraan sanottuna nautinnollisen puhdasta koodata. Katsoppas näitä esimerkkejä:

```Haskell
-- Yksinkertainen yhdistäminen
main = putStrLn $ "Moikka, " ++ "maailma!"

-- Funktio merkkijonojen yhdistämiseen
yhdistä :: String -> String -> String
yhdistä a b = a ++ b

-- Käyttö
main = putStrLn $ yhdistä "Koodausta " "Haskellilla."

-- Käyttämällä `concat` funktiota listoille
main = putStrLn $ concat ["Haskell ", "on ", "täällä ", "jäädäkseen."]

-- Tulos kullekin:
-- "Moikka, maailma!"
-- "Koodausta Haskellilla."
-- "Haskell on täällä jäädäkseen."
```

Ei turhia kommervenkkejä. Käynnistä GHCi ja kokeile itse.

## Deep Dive | Syvä Sukellus:
Haskellissa merkkijonojen yhdistäminen on ikivanhaa perinnettä. Ne ovat listoja, joissa on `Char`-tyypin alkiot – siksi '++' toimii niin saumattomasti.

Muita tapoja? Funktio `concatMap` tekee näppärän tempun listoille. Monadeille `[Char]` eli `String` -tyypille löytyy `>>=` operaattori, joka tekee saman mutta hieman eri tavalla. Muista että `Text` ja `ByteString` ovat tehokkaampia isommille datamäärille.

Merkkijonojen yhdistäminen on pohjimmiltaan listojen yhdistämistä. Haskellissa tämä on optimoitu käytettäväksi järkevästi, mutta älä unohda suorituskykyseikkoja suurten datamäärien kanssa.

## See Also | Katso Myös:
- Haskellin viralliset dokumentit: https://www.haskell.org/documentation
- "Learn You a Haskell" ilmaiseksi netissä osoitteessa: http://learnyouahaskell.com/
- Hoogle (Haskell API-haku): https://hoogle.haskell.org/

Koodaile rohkeasti ja kokeile eri tapoja. Haskell on siitä kiehtova, että se pakottaa ajattelemaan asioita hieman eri kulmasta. Hauskoja hetkiä merkkijonojen parissa!
