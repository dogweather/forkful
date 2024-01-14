---
title:                "Haskell: Stor bokstavsättning av en sträng"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanlig uppgift för många programmerare, oavsett vilket programspråk de arbetar med. Det är ett enkelt sätt att ändra utseendet på en textsträng och göra den mer lättläslig eller önskad.

## Så här

För att kapitalisera en sträng i Haskell finns det flera olika metoder som kan användas. Nedan följer några exempel på hur detta kan göras:

```Haskell
-- Metod 1: Använda inbyggd funktion 'map'
map toUpper "hej världen" -- output: "HEJ VÄRLDEN"

-- Metod 2: Använda rekursion och inbyggda funktioner
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : (capitalize xs)
-- Användning: capitalize "hej världen" -- output: "HEJ VÄRLDEN"

-- Metod 3: Använda punktnotation på en inbyggd funktion
import Data.Char (toUpper)
toUpper <$> "hej världen" -- output: "HEJ VÄRLDEN"
```

Som du kan se finns det flera sätt att kapitalisera en sträng. Det viktiga är att förstå hur de olika funktionerna och metoderna fungerar och välja den som passar bäst för den specifika uppgiften.

## Djupdykning

För att förstå kapitalisering av strängar i Haskell på en djupare nivå är det viktigt att ha grundläggande kunskaper om datatyper och funktioner. I Haskell, precis som i andra funktionella programspråk, är strängar listor av tecken, vilket är anledningen till att vi kan använda funktioner som `map` och rekursion för att manipulera dem.

Funktionen `toUpper` är också otroligt användbar för att konvertera en bokstav till versal. Genom att använda den tillsammans med andra funktioner i Haskell-kärnbiblioteket kan vi konstruera en mängd olika strängoperationer, inklusive kapitalisering.

Det är också viktigt att notera att kapitalisering av strängar kan ha olika utfall beroende på det aktuella språket som används i Haskell-programmet. Till exempel kan den inbyggda funktionen `map` kapitalisera versaler korrekt på engelska, men kan behöva anpassas för andra språk som har särskilda tecken och diakritiska markeringar.

## Se också

- [Haskell-tutorial på svenska](https://www.haskell.org/documentation/och-andra-strategier-for-att-lara-sig-haskell/)
- [Officiell dokumentation för Data.Char-modulen](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Tips för att skriva lättläslig Haskell-kod](https://chrisdone.com/posts/beautiful-haskell-style/)