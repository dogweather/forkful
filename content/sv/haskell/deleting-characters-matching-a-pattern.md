---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Radera tecken som matchar ett visst mönster är en programmeringsmetod som tillåter oss att manipulera strängar efter behov. Programmerare gör det för att förbättra datakvalitet, hantera datainmatning och optimera information för specifika användningar.

## Hur man gör:
Här är ett enkelt exempel om hur man raderar alla förekomster av ett visst mönster från en sträng. Anta att vi har en sträng "Hej, det är trevligt att träffa dig!" och vi vill ta bort alla kommatecken.
```Haskell
import Data.Char
import Data.List
str = "Hej, det är trevligt att träffa dig!"
cleanStr = filter (/= ',') str
```
Ovanstående kod kommer att returnera strängen "Hej det är trevligt att träffa dig!".

## Djup Dykning
Det finns en historisk kontext till varför vi tar bort tecken som matchar ett visst mönster. I tidigare datorer var lagringsutrymmet begränsat, så att radera onödvändiga tecken kunde spara dyrbar lagringsutrymme. Det finns flera sätt att ta bort tecken från en sträng i Haskell, inklusive 'filter', 'delete' och 'strip'. 'Filter' funktionen är den mest allmänna metoden och 'delete' kan radera första förekomsten av det angivna tecknet. Specifika val beror alltid på det specifika use-case.

## Se Även
Mer information om textmanipulationsmetoder i Haskell kan hittas på:
- Haskell officiella dokumentation: https://www.haskell.org/tutorial/strings.html
- Real World Haskell: http://book.realworldhaskell.org/read/strings-bytes-and-character-encoding.html
- Learn You a Haskell for Great Good: http://learnyouahaskell.com/input-and-output#strings-and-their-cases.