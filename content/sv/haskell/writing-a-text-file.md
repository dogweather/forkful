---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att spara data i läsbar form på en permanent lagringsenhet, ofta för lagring eller kommunikation av information. Programmerare gör detta för att hantera konfigurationsfiler, logga information, eller exportera data som ska användas av andra program eller system.

## Hur gör man:
```Haskell
-- Skriver en enkel textfil
import System.IO

-- Huvudfunktionen som utför filskrivningen
main :: IO ()
main = do
    let innehall = "Hej, detta är en textfil skapad med Haskell!"
    writeFile "exempel.txt" inenhall
    putStrLn "Fil skriven!"

-- readFile är en annan användbar funktion
-- Enkel filinläsning
laddaFilOchSkrivUt :: IO ()
laddaFilOchSkrivUt = do
    innehall <- readFile "exempel.txt"
    putStrLn innehall
```
Efter att ha kört `main`, kontrollera den skapade `exempel.txt`-filen. Dess innehåll bör vara "Hej, detta är en textfil skapad med Haskell!".

## Djupdykning
Den aktuella versionen av Haskell använder `System.IO` biblioteket för filoperationer som historiskt sett baserades på I/O-systemet i språket. Alternativ som `ByteString` eller `text` paketen erbjuder oftast prestandaförbättringar och extra funktionalitet. Vid filskrivning hanterar Haskell buffring automatiskt. Men det går att ändra buffertbeteendet manuellt genom att använda `hSetBuffering`.

## Se Även
- [Haskell `System.IO` Documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html)
- [Haskell Wiki on I/O](https://wiki.haskell.org/IO_inside)
