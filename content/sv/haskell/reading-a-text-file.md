---
title:                "Haskell: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande färdighet inom programmering och kan vara särskilt användbar när man vill lagra och behandla stora mängder data. Det är också ett bra sätt att förstå hur man kan använda Haskell för att interagera med filsystemet.

## Hur man gör det

För att läsa en textfil i Haskell behöver vi använda oss av ett par funktioner från modulen "System.IO". Vi börjar med att importera modulen och sedan öppna filen genom att använda "openFile" funktionen:

```Haskell
import System.IO

main = do
  handle <- openFile "textfil.txt" ReadMode
```

Vi kan antingen ange en absolut sökväg till filen eller om filen ligger i samma mapp som Haskell-filen kan vi bara skriva filnamnet. Den andra parametern "ReadMode" anger att vi enbart vill läsa från filen, inte skriva eller ändra i den.

När filen är öppnad har vi tillgång till variabeln "handle" som representerar vår fil. För att faktiskt läsa från filen använder vi funktionen "hGetLine" som läser en rad från filen och returnerar den som en sträng:

```Haskell
line <- hGetLine handle
```

Vi kan sedan skriva ut raden eller göra vad vi vill med den. När vi är klara behöver vi stänga filen igen för att frigöra resurserna, vilket vi gör med "hClose" funktionen:

```Haskell
hClose handle
```

## Djupdykning

Förutom "hGetLine" finns det även andra funktioner för att läsa från filer, som "hGetChar" för att läsa en enskild tecken eller "hGetContents" för att läsa in hela filens innehåll som en sträng.

Det är också möjligt att läsa in filen direkt till en lista med funktionen "words", som delar upp en sträng i sin delar baserat på ett separatortecken. Till exempel kan vi läsa in varje ord i filen till en lista med hjälp av följande kod:

```Haskell
str <- hGetContents handle
let wordsList = words str
```

## Se även

- [Haskell-dokumentation om filhantering](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [En tutorial om filhantering i Haskell](http://learnyouahaskell.com/input-and-output#files-and-streams)