---
title:    "Haskell: Läsning av en textfil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan vara en viktig del av programmering i Haskell. Det kan tillåta oss att läsa och behandla data på ett effektivt sätt.

## Hur man gör

För att läsa en textfil i Haskell, måste vi först importera "System.IO" biblioteket. Sedan kan vi använda funktionen "readFile" för att läsa innehållet i en fil och lagra det i en variabel.

```Haskell
import System.IO

main = do
  fil <- readFile "minTextfil.txt"
  putStr fil
```

I detta exempel använder vi "putStr" funktionen för att skriva ut innehållet i filen till skärmen. Om du vill spara innehållet i filen i en variabel, kan du använda "return" funktionen.

```Haskell
import System.IO

main = do
  fil <- readFile "minTextfil.txt"
  innehall <- return fil
```

## Djupdykning

Förutom att enkelt läsa innehållet i en textfil, kan vi använda olika funktioner för att bearbeta och manipulera data. Till exempel kan vi använda "lines" funktionen för att dela upp innehållet i filen i rader och sedan använda "words" funktionen för att dela upp varje rad i ord.

```Haskell
import System.IO

main = do
  fil <- readFile "minTextfil.txt"
  let rader = lines fil
  print rader

  let ord = words rader
  print ord
```

Med hjälp av dessa funktioner kan vi sedan utföra olika åtgärder på datan och producera önskade resultat.

## Se även

- [Haskell.org - I/O och filhantering](https://www.haskell.org/tutorial/io.html)
- [Learn You a Haskell - I/O](http://learnyouahaskell.com/input-and-output)
- [Haskell programming from first principles - I/O och filhantering](https://haskellbook.com/chapters/09-input-and-output.html#inputandoutput)