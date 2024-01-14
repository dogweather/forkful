---
title:                "Haskell: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en tillfällig fil i Haskell kan vara användbart när du behöver bearbeta data temporärt och vill undvika att permanent lagra det på din dator. Detta kan vara särskilt användbart vid hantering av stora datamängder som kan ta upp mycket utrymme.

## Så här gör du

För att skapa en temporär fil i Haskell, kan du använda funktionen "with System.IO.Temp", som är en del av Haskell biblioteket för att hantera temporära filer. Detta bibliotek ger också funktioner för att skapa filer av olika typer, såsom textfiler, binärfiler eller kanske CSV-filer.

```Haskell
import System.IO.Temp
import System.IO

main = do
  withSystemTempFile "sample.txt" $ \tempFile handle -> do
    hPutStrLn handle "Det här är en tillfällig fil som skapas i Haskell."
    putStrLn $ "Tillfällig fil skapad på plats: " ++ tempFile
```

När du kör detta program kommer det att skapa en textfil med namnet "sample.txt" på din dator. Innehållet i filen kommer att vara "Det här är en tillfällig fil som skapas i Haskell.". Efter programmet är klart kommer filen automatiskt att raderas.

## Lite djupare

Det finns flera anledningar till varför man skulle vilja skapa en tillfällig fil i Haskell. En av de vanligaste är att hantera stora datamängder som behöver processas temporärt. Genom att använda en temporär fil istället för att bearbeta data direkt på din dator, kan du undvika att belasta ditt system och riskera att det kraschar.

Man kan också använda tillfälliga filer för att testa kod och funktioner innan de implementeras i huvudprogrammet. På så sätt kan man undvika att göra permanenta förändringar i koden tills dess att den är helt färdig och testad.

## Se även

- [Haskell biblioteket för hantering av temporära filer](https://hackage.haskell.org/package/temporary)
- [En guide till Haskell för nybörjare](https://arewehaskellyet.com/)
- [Här finns fler praktiska användningsområden för temporära filer i Haskell](https://wiki.haskell.org/Temporary_files)