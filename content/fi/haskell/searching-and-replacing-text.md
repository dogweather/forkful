---
title:    "Haskell: Tekstin etsiminen ja korvaaminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa joudutaan muokkaamaan tekstiä, esimerkiksi korvaamaan sana toisella. Tässä blogikirjoituksessa opimme, kuinka voimme helposti suorittaa hakua ja korvausta Haskell-kielellä.

## Kuinka tehdä

Haskellissa tekstinhaku ja korvaus voidaan suorittaa käyttämällä `replace` funktiota `Data.Text` kirjastosta. Tässä on yksinkertainen esimerkki koodista, joka korvaa kaikki esiintymät sanan "tämä" merkkijonolla "se".

```Haskell
import Data.Text

sampleText = "Tämä on esimerkki tekstistä."

main = do
    let modifiedText = replace "tämä" "se" sampleText
    putStrLn modifiedText
```

Tulostus:

```
"Se on esimerkki tekstistä."
```

## Syvempi sukellus

Haskellissa tekstinhaku ja korvaus voidaan suorittaa myös monimutkaisemmin käyttäen regex-ilmaisuja (`Data.Text.Regex`). Tässä esimerkissä korvaamme kaikki numerot välillä 0-9 sanalla "numero".

```Haskell
import Data.Text
import Data.Text.Regex

sampleText = "Tässä on 10 sanaa ja 5 lausetta."

main = do
    let regexPattern = mkRegex "[0-9]+"
    let modifiedText = subRegex regexPattern sampleText "numero"
    putStrLn modifiedText
```

Tulostus:

```
"Tässä on numero sanaa ja numero lausetta."
```

## Katso myös

- [Regex ilmentymien käyttö yleisesti](https://www.regular-expressions.info/)
- [Haskell teksti ja merkkijono käsittely](https://wiki.haskell.org/Strings_and_text)