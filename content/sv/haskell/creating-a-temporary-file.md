---
title:                "Haskell: Skapa en tillfällig fil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa temporära filer är ett vanligt använd tool inom Haskell programmering. Det är användbart när man arbetar med tillfälliga data som inte behövs sparas permanent.

## Hur man gör
För att skapa en temporär fil i Haskell kan man använda funktionen `withSystemTempFile` från modulen System.IO.Temp. Här är ett enkelt exempel på hur man kan använda denna funktion:

```Haskell
import System.IO.Temp

main :: IO ()
main = withSystemTempFile "example.txt" $ \path handle -> do
  putStrLn $ "Skapade en temporär fil på: " ++ path
  hPutStrLn handle "Detta är en temporär fil"
```

I det här exemplet skapar vi en temporär fil vid namn "example.txt" och skriver en kort text till den. Efter att programmet avslutas kommer filen automatiskt att raderas.

## Djupdykning
När vi använder `withSystemTempFile` skapas en temporär fil i det vanliga operativsystemets temporära filsystem. Detta kan vara annorlunda beroende på vilket operativsystem man använder. Det finns också möjligheter att kontrollera var filen ska skapas, samt att manuellt radera den om man inte vill att den ska tas bort direkt.

## Se även
- [System.IO.Temp dokumentation](https://www.stackage.org/haddock/lts-18.1/base-4.15.0.0/System-IO-Temp.html)
- [Haskell wiki: Creating temporary files](https://wiki.haskell.org/Creating_temporary_files)