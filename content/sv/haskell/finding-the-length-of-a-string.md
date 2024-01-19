---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hitta strängens längd i Haskell

## Vad & Varför?
Att hitta längden på en sträng är en grundläggande operation inom programmering som innebär att fastställa antalet tecken i en given sträng. Det är viktigt för att hantera strängmanipuleringar, loop iterationer, och data valideringar effektivt.

## Hur gör man:
I Haskell är `length` funktionen det enklaste sättet att hitta längden på en sträng. Här är ett kort exempel:

```Haskell
strang = "Hej Sverige"
main = print(length strang)
```
Output:
```Haskell
12
```
`length` funktionen räknar varje tecken, inklusive mellanslag, för att beräkna strängens längd.

## Djupdykning
Historiskt sett, har `length` funktionen implementerats genom att iterera igenom varje element i en lista (i det här fallet är en sträng en lista av `Char`, eller tecken). 

Ett alternativ till `length` är att använda `foldl'` från `Data.List` för att simulera en loop som räknar tecknen:

```Haskell
import Data.List (foldl')
strang = "Hej Sverige"
main = print (foldl' (\n _ -> n + 1) 0 strang)
```
Detta ger samma output, men `foldl'` versionen kan vara mer fördelaktig för mycket långa strängar då det undviker en möjlig stack overflow.

Observera att både längdfunktionen och foldl'-metoden räknar antalet 'Chars' i en sträng, vilket kanske inte är vad man förväntar sig om man arbetar med Unicode-strängar där vissa tecken faktiskt kan representera flera databyte.

## Se Även
För mer information om andra metoder för att arbeta med strängar och listor i Haskell, kolla in följande resurser:

1. [Haskell Wikibok](https://en.wikibooks.org/wiki/Haskell)
2. [Haskell Documentation: Data.List](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html)
3. [Lär dig You a Haskell (Learn You a Haskell)](http://learnyouahaskell.com/chapters)
4. [Real World Haskell](http://book.realworldhaskell.org/)