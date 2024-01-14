---
title:    "Haskell: Konvertera en sträng till versaler"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför
Att kunna stora bokstäver i en sträng är en grundläggande och användbar funktion i Haskell-programmering. Det kan hjälpa till att göra koden mer läsbar och hanterbar, särskilt när man arbetar med stora datamängder eller komplexa algoritmer.

## Så här gör du
För att stadkomma en sträng i Haskell kan du använda funktionen `toUpper` från modulen `Data.Char`. Detta kommer att konvertera alla små bokstäver i strängen till stora bokstäver.

```Haskell
import Data.Char (toUpper)

capString :: String -> String
capString str = map toUpper str

main = do
    let myString = "Hej, mitt namn är Haskell"
    putStrLn (capString myString)
```

Output:
`HEJ, MITT NAMN ÄR HASKELL`

Det är också möjligt att koda en funktion som bara konverterar den första bokstaven i strängen till stor bokstav, genom att använda funktionerna `toUpper` och `toLower` från samma modul.

```Haskell
import Data.Char (toUpper, toLower)

capFirstLetter :: String -> String
capFirstLetter (x:xs) = toUpper x : map toLower xs

main = do
    let myString = "hej, mitt namn är Haskell"
    putStrLn (capFirstLetter myString)
```

Output:
`Hej, mitt namn är Haskell`

## Djupdykning
En sträng i Haskell representeras som en lista av tecken (chars). När vi använder funktionen `map` för att applicera `toUpper` eller `toLower` på varje tecken i listan, blir den nya listan av tecken återigen en sträng. Detta kallas för listkomprehension.

För att utföra de här operationerna på en sträng, behöver vi modulen `Data.Char` eftersom Haskell använder unicode för att representera tecken i strängar. Unicode är ett system som tillåter representation av alla tecken i alla språk i en datamaskin.

## Se även
- [Haskell dokumentation för Data.Char](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Haskell tutorial på svenska](https://github.com/Haskoli-islands/Haskell/wiki/Introduktion)
- [Haskell community i Sverige](https://haskell.se/)