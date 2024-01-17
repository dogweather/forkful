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

## Vad & Varför?

Läsning av en textfil är en process där en datorprogram läser och hämtar information från en textfil. Detta är användbart för programmerare eftersom det tillåter dem att läsa och analysera information från externa källor, vilket kan användas i deras program.

## Hur man gör:

```Haskell
import System.IO

main = do
    handle <- openFile "textfil.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

## Djupdykning:

Det är inte ovanligt att behöva läsa information från en textfil i ett programmeringsprojekt. Det finns också andra sätt att läsa filer, till exempel med hjälp av inbyggda funktioner i Haskell som `readFile` eller `Data.Text` biblioteket.

Implementeringen av att läsa en textfil kan också vara olika beroende på vilken plattform och operativsystem som används. Det är därför viktigt att se till att koden är kompatibel med olika system.

## Se även:

För mer information om att läsa textfiler i Haskell, se följande resurser:

- [Haskell dokumentation om IO-operations](https://www.haskell.org/tutorial/io.html)
- [Haskell biblioteket "System.IO"](https://hackage.haskell.org/package/base-4.15.1.0/docs/System-IO.html)
- [Haskell biblioteket "Data.Text"](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)