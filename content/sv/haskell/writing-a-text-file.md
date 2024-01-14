---
title:                "Haskell: Att skriva en textfil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en grundläggande färdighet som alla som programmerar behöver för att spara och strukturera data. Genom att använda Haskell kan du enkelt skapa textfiler som är snabba och effektiva att läsa och bearbeta.

## Hur man gör det

För att skriva en textfil i Haskell behöver vi först importera modulen "System.IO" som ger oss funktioner för att öppna, läsa och skriva till filer. Vi behöver också en variabel som håller namnet på vår fil, t.ex. "minFil.txt".

```Haskell
import System.IO

-- öppna filen i läge för att skriva
main = do
    fil <- openFile "minFil.txt" WriteMode
```

Nu kan vi använda funktionen "hPutStrLn" för att skriva en sträng till filen och funktionen "hClose" för att stänga filen.

```Haskell
-- skriv "Hej världen" till filen
hPutStrLn fil "Hej världen"

-- stäng filen
hClose fil
```

## Djupdykning

När vi skriver till en textfil i Haskell använder vi "String" som datatyp. Detta innebär att vi kan skriva ut alla typer av text, inklusive specialtecken, direkt till filen. Om vi vill skriva ut andra datatyper som t.ex. en Int eller Bool behöver vi först konvertera dem till en String.

Vi kan också använda funktionen "withFile" istället för "openFile" för att hantera det steg där vi öppnar och stänger filen. Detta hjälper till att undvika eventuella problem med stängda filer.

## Se också

- [Haskell.org](https://www.haskell.org/)
- [Haskell för nybörjare (Svenska)](https://medium.com/swiftly-swift/haskell-f%C3%B6r-nyb%C3%B6rjare-en-introduktion-till-funktionell-programmering-och-haskell-b04e9d613fdf)
- [Haskell Wikibooks](https://en.wikibooks.org/wiki/Haskell)