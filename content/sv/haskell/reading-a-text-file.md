---
title:                "Haskell: Läsning av en textfil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är en viktig del av programmering, oavsett vilket språk man använder. Det är ett sätt att läsa och behandla data som är sparat i en textfil, och det kan vara användbart för att hämta information eller skapa en ny fil med specifikt innehåll.

## Så här gör du
För att läsa en textfil i Haskell behöver du först öppna filen med funktionen `openFile`, som tar två argument: filnamn och åtkomstläge. Det finns flera åtkomstlägen, men de två vanligaste är `ReadMode` för att läsa en befintlig fil och `WriteMode` för att skapa en ny fil.

```Haskell
import System.IO
main = do
    handle <- openFile "exempel.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
```

I exemplet ovan öppnar vi en fil med namnet "exempel.txt" i läge `ReadMode` och använder sedan funktionen `hGetContents` för att läsa filens innehåll och spara det i variabeln `contents`. Sedan skriver vi ut innehållet med `putStrLn` och stänger filen med `hClose`.

## Djupdykning
Det finns flera andra funktioner och sätt att läsa en textfil i Haskell, såsom att läsa rad för rad med `hGetLine`, läsa en given mängd tecken med `hGet`, eller läsa filen i strömmande format med `hGetContents`. Det är även möjligt att använda Haskell's `File` typ för att manipulera och behandla filen som ett objekt.

En viktig aspekt att tänka på när man läser en textfil i Haskell är att man måste hantera eventuella fel som kan uppstå, såsom om filen inte finns eller om filen inte har rätt behörigheter. Det är därför viktigt att förstå och hantera felhantering i Haskell.

## Se även
- [Haskell's System.IO modul](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Haskell's File typ](https://hackage.haskell.org/package/base/docs/System-IO.html#g:2)
- [En detailed guide till att läsa och skriva filer i Haskell](https://www.tutorialspoint.com/haskell/haskell_input_output.htm)