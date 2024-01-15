---
title:                "Skapa en tillfällig fil"
html_title:           "Haskell: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapande av temporära filer är ett användbart verktyg i Haskell-programmering eftersom det tillåter program att temporärt spara data eller utföra operationer utan att permanent förändra filsystemet. Detta kan vara användbart för att testa kod eller för att hantera tillfällig data i en applikation.

## Så här gör du

```Haskell
import System.IO
import System.Directory
import System.IO.Temp
import Control.Exception (finally)

main = do
  withTempFile "tempfile.txt" $ \tempFile handle -> do
    -- Gör något med temporär filen, t.ex. skriv data till den
    hPutStrLn handle "Det här är en temporär fil"
    hClose handle
  -- När withTempFile är klar så raderas den temporära filen automatiskt
```

Output:
```
Innehåll i "tempfile.txt":
Det här är en temporär fil

"tempfile.txt" raderad
```

## Djupdykning

I Haskell kan det finnas flera sätt att skapa en temporär fil. En av de vanligaste är att använda funktionen `withTempFile` från modulen `System.IO.Temp`. Denna funktion tar ett filnamn som argument och returnerar en hanterare för filen samt skapar filen i det temporära mappen. När hanteraren används så kommer all data skrivet till filen att sparas i den temporära filen och när hanteraren stängs så raderas filen automatiskt.

Det finns också andra funktioner såsom `withTempDirectory` och `openTempFile` för att skapa temporära mappar och filer. Det är viktigt att notera att dessa temporära filer och mappar endast finns så länge som programmet körs och kommer automatiskt att raderas när programmet avslutas.

## Se även

- [Officiell documentation för `System.IO.Temp` modulen](https://hackage.haskell.org/package/temporary-1.3.0.1/docs/System-IO-Temp.html)
- [Haskell för nybörjare](https://wiki.haskell.org/Beginners)