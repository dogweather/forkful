---
title:                "Gör en sträng versal"
html_title:           "Haskell: Gör en sträng versal"
simple_title:         "Gör en sträng versal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Kapitalisering av en sträng innebär att ändra varje tecken i början av varje ord till en stor bokstav. Programmerare gör detta för att förbättra läsbarheten och indikera viktiga ord och fraser.

## Hur gör man:
Här är ett enkelt exempel på hur man kapitaliserar varje ord i en sträng i Haskell:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
```
I det här exemplet konverterar ``toUpper`` varje enskilt tecken till stora bokstäver vilket resulterar i en huvudbokstav vid början av varje ord.

## Fördjupning
Kapitalisering av strängar går tillbaka till begynnelsen av programmeringsspråk. Dess primära användning är att främja läsbarhet och förståelse, särskilt inom områden som naturalspråksbehandling.

I Haskell, till exempel, har vi "Data.Char" biblioteket, vilket ger oss funktionen `toUpper`. Haskell tillhandahåller också "words" och "unwords" funktioner som bryter upp och kombinerar strängar respektive.

Det är dock viktigt att notera att denna metod bara ändrar det första tecknet i varje sträng. Om du vill göra mer komplexa manipulationer, såsom att upprätthålla stavning med första bokstavsstorleken för specifika termer, kan du behöva skapa mer detaljerade funktioner eller använda mer avancerade bibliotek.

## Se även 
Om du är intresserad av att lära dig mer om Haskell och dess bibliotek, kan du kolla följande länkar:

1. [Learn You a Haskell](http://learnyouahaskell.com/) : En gratis onlinebok för att lära sig Haskell från grunden.
2. [Haskell Documentation](https://www.haskell.org/documentation/) : Officiella dokument för Haskell, inklusive detaljer om strängbearbetning och mer.
3. [Hoogle](https://www.haskell.org/hoogle/) : En Haskell-specifik sökmotor som låter dig söka efter bibliotek, moduler och funktioner.