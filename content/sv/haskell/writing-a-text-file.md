---
title:                "Att skriva en textfil"
html_title:           "Haskell: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att kunna skriva en textfil kan vara väldigt användbart när man arbetar med programmering. Det kan till exempel användas för att spara data eller skapa konfigurationsfiler.

## Så här gör du
Det finns två olika sätt att skriva en textfil i Haskell, antingen med hjälp av standardbiblioteket eller genom att använda en tredjepartsmodul som `Data.Text.IO`. Nedan följer exempel på båda metoderna.

```Haskell
-- Skriv en textfil med hjälp av standardbiblioteket
import qualified Data.Text.IO as T

-- Öppna en fil för skrivning
file <- openFile "textfil.txt" WriteMode

-- Skriv en sträng till filen
hPutStr file "Det här är en textsträng!"

-- Stäng filen
hClose file

-- Läs innehållet i filen och skriv ut det
T.readFile "textfil.txt"
```

```Haskell
-- Skriv en textfil med hjälp av Data.Text.IO
import qualified Data.Text.IO as T

-- Skapa en textsträng
let text = "Det här är en textsträng!"

-- Skriv textsträngen till filen
T.writeFile "textfil.txt" text

-- Läs innehållet i filen och skriv ut det
T.readFile "textfil.txt"
```

Förväntad utgång:

```Haskell
"Det här är en textsträng!"
```

## Deep Dive
För att kunna skriva en textfil i Haskell behöver man först öppna en fil för skrivning. Detta görs med `openFile` funktionen från standardbiblioteket. Sedan kan man använda funktioner som `hPutStr` eller `hWrite` för att skriva till filen. När man är klar måste filen stängas med `hClose`.

En annan metod är att använda `Data.Text.IO` modulen, där man skapar en textsträng och sedan skriver den till filen med `writeFile` funktionen. Båda metoderna är enkla och effektiva sätt att skriva en textfil i Haskell.

## Se även
- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell](http://learnyouahaskell.com/chapters)
- [Real World Haskell](http://book.realworldhaskell.org/read/)