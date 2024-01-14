---
title:    "Haskell: Skapa en tillfällig fil"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapandet av tillfälliga filer är en vanlig praxis inom programmering. Detta kan vara användbart när man behöver skapa en temporär fil för att lagra data eller utföra en temporär uppgift.

## Hur man gör

Skapandet av en tillfällig fil är enkelt med hjälp av haskellfunktionen `openTempFile` som tar två parametrar, sökvägen och filnamnet. Här är ett exempel på hur man skapar en tillfällig fil och sedan skriver strängen "Hej, världen!" till filen.

```Haskell
import System.IO
main = do
   (tempName, tempHandle) <- openTempFile "." "tempfile"
   hPutStrLn tempHandle "Hej, världen!"
   hClose tempHandle
```

Output:

```
Skapar en temporär fil i den nuvarande mappen med filnamnet "tempfile" och skriver innehållet "Hej, världen!" till filen.
```

## Djupdykning

För att förstå processen bakom skapandet av en temporär fil måste vi först förstå hur filsystemet fungerar. När en fil skapas används en bitmapp för att identifiera lediga sektorer på lagringsenheten. För att skapa en tillfällig fil kan operativsystemet använda en slumpmässigt genererad filnamn och skriva filen till en av de lediga sektorerna. När filen stängs eller tas bort frigörs de lediga sektorerna igen för att användas till andra filer.

## Se även

- [Haskells dokumentation för `openTempFile`](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html#v:openTempFile)
- [En handledning för tillfälliga filer i Haskell](https://wiki.haskell.org/Introduction_to_IO#Temporary_files_and_directories)