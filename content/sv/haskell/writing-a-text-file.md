---
title:    "Haskell: Skriva en textfil"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en viktig del av programmering eftersom det gör det möjligt för dig att lagra och manipulera data på ett enkelt och strukturerat sätt. Det kan också vara ett sätt att kommunicera med andra programmerare eller användare av ditt program.

## Så här gör du

För att skriva en textfil i Haskell behöver du först importera modulen "System.IO". Sedan kan du använda kommandot "writeFile" för att skriva en sträng till en textfil och "appendFile" för att lägga till mer text till en befintlig fil.

```
import System.IO

-- Skriva en helt ny fil
writeFile "minfil.txt" "Hej världen!"

-- Lägga till text till en befintlig fil
appendFile "minfil.txt" " Det här är en extra rad."
```

Detta är bara en enkel implementation, men det finns många olika sätt att manipulera textfiler i Haskell, såsom att läsa från en fil, ta bort innehåll eller ändra ordning på rader.

## Djupdykning

Skrivande av textfiler i Haskell är en del av modulen "System.IO", som innehåller många andra funktioner för att hantera in- och utmatning. Detta inkluderar läsning och skrivning från terminalen, hantering av fel och hantering av binära filer. Det är också möjligt att använda modulerna "Data.Text" och "Data.ByteString" för att hantera textfiler på ett mer effektivt sätt.

En annan viktig sak att notera är att när en textfil öppnas för skrivning eller läsning, bör den stängas när man är klar för att undvika läckor av resurser. Detta kan göras med funktionen "hClose".

## Se också

- [Haskell Wiki page on IO](https://wiki.haskell.org/IO)
- [Haskell documentation on System.IO module](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Haskell tutorial on handling I/O](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)