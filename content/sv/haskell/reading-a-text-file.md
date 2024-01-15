---
title:                "Läsa en textfil"
html_title:           "Haskell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika anledningar till varför man skulle vilja läsa en textfil med Haskell-kod. Det kan vara för att hitta och lösa problem, för att lära sig mer om språket eller för att bara utforska vad som är möjligt med denna funktionella programmeringsteknologi.

## Hur man gör

För att läsa en textfil i Haskell behöver du först importera modulen "System.IO". Sedan kan du använda funktionen "readFile" för att läsa innehållet i filen. Här är ett exempel på kod för att läsa en fil som heter "exempel.txt" och skriva ut innehållet på skärmen:

```Haskell
import System.IO

main = do
  innehall <- readFile "exempel.txt"
  putStr innehall
```

Kodblocket ovan kommer att skriva ut innehållet i filen "exempel.txt" på skärmen när du kör programmet. Du kan även använda "writeFile" för att skriva till en fil från Haskell.

## Djupdykning

När du läser en textfil i Haskell, läses allt in som en enda lång sträng. Detta kan vara problematiskt om du vill utföra olika operationer på olika delar av filen. För att undvika detta kan du använda funktionen "lines". Den delar upp strängen i en lista av rader, vilket gör det lättare att hantera olika delar av filen.

En annan användbar funktion när du läser textfiler är "words". Den delar upp strängen i en lista av ord, perfekt för att behandla text som är uppdelad i ord eller fraser.

## Se även

- [Haskell-dokumentationen om textfiler](https://www.haskell.org/documentation/)
- [En guide till funktionell programmering med Haskell](https://www.tutorialspoint.com/functional_programming/functional_programming_haskell.htm)
- [En komplett introduktion till Haskell](https://www.springer.com/us/book/9783319714514)