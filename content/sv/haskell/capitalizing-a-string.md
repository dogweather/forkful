---
title:                "Att skriva ut en sträng"
html_title:           "Haskell: Att skriva ut en sträng"
simple_title:         "Att skriva ut en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "capitalize"-a en sträng betyder att förvandla den till en sträng där varje ord börjar med stor bokstav. Programmörer gör detta för att göra en sträng läsbar och tydlig för användare.

## Så här:
```Haskell
import Data.Char  -- importerar Data.Char modul för att använda dess funktioner

capitalize :: String -> String  -- funktion för att capitalize a sträng
capitalize [] = []  -- om strängen är tom, returnera tom sträng
capitalize (x:xs) = toUpper x : map toLower xs  -- omvandla första bokstaven till stor bokstav och resten till små bokstäver

-- exempel på användning
capitalize "haskell"  -- "Haskell"
capitalize "programming language"  -- "Programming language"
```

## Djupdykning:
Historiskt sett har att "capitalize"-a en sträng varit viktigt för att göra text läsbar och tydlig för läsare. Ett alternativ till att bara omvandla första bokstaven är att helt enkelt skapa ett nytt ord genom att lägga till ett mellanrum och en ny ord efter det gamla. Det kan också göras genom att använda "Title Case" där varje ord börjar med stor bokstav. I Haskell implementeras capitalize funktionen genom att använda Data.Char modulen som ger tillgång till funktioner för att konvertera bokstäver.

## Se även:
För mer information om att "capitalize"-a en sträng och andra string manipulationer i Haskell, besök följande länkar:

- [Haskell Data.Char Modul](https://www.geeksforgeeks.org/haskell-data-char-module/)

- [Haskell Tutorial: String Operations and String Functions](https://www.tutorialspoint.com/haskell/haskell_string_operations.htm)

- [Real World Haskell: Strings and Characters](http://book.realworldhaskell.org/read/strings-and-characters.html)