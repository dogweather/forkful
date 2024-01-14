---
title:    "Haskell: Att Stora En Sträng"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna formatera text är en viktig del av programmering och kan vara avgörande för användarens upplevelse av ett program eller en webbplats. Att kunna ändra en sträng till versaler kan vara användbart i många situationer, till exempel när man vill skriva ut en titel eller ta emot användarinput i en formaterad form.

## Hur man gör
För att kunna ändra en sträng till versaler i Haskell finns det några olika sätt att göra det. Ett enkelt sätt är att använda funktionen `toUpper` från modulen `Data.Char`. Denna funktion tar en karaktär som argument och returnerar motsvarande versal. Det betyder att man kan använda funktionen på varje enskild karaktär i strängen för att få en sträng med bara versaler.
 
```Haskell
import Data.Char
 
capitalize :: String -> String
capitalize str = map toUpper str
 
main = do
    let str = "hejsan"
    putStrLn (capitalize str)
```
**Resultat:**
`HEJSAN`

Detta enkla exempel visar hur man kan använda `map` för att applicera en funktion på varje element i en lista. Vi använder `toUpper` på varje karaktär i strängen "hejsan" och får som ett resultat en sträng med versaler.

## Djupdykning
Förutom att använda `toUpper` finns det andra funktioner som kan vara användbara för att ändra en sträng till versaler. I modulen `Data.Text` finns till exempel funktionerna `toUpper` och `toUpperLoc`. Den första funktionen tar emot en `Text` (en sträng som är optimerad för prestanda) och returnerar den i versaler. Den andra funktionen tar emot en `Text` och returnerar en `LocaleText`, vilket är en sträng som behåller information om språk och skriftsystem.

En annan intressant funktion är `toTitle` från modulen `Data.Text.Title`. Denna funktion omvandlar en sträng till titelcase, vilket betyder att varje ord i strängen får en stor bokstav i början och resten av ordet är i gemener. Detta kan vara användbart för att formattera titlar eller rubriker.

## Se även
- [Haskell Wiki](https://wiki.haskell.org/Capitalizing_strings)
- [Haskell Dokumentation om Data.Char modulen](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Haskell Dokumentation om Data.Text modulen](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskell Dokumentation om Data.Text.Title modulen](https://hackage.haskell.org/package/text/docs/Data-Text-Title.html)