---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att konvertera en sträng till gemener innebär att ändra alla stor bokstäver i strängen till små bokstäver. Detta utförs ofta av programmerare för att jämföra strängar på ett sätt som är oberoende av versalisering.

## Hur Fungerar Det:

I Haskell konvertera vi en sträng till gemener med hjälp av funktionen `map toLower`. Vi måste dock importera `Data.Char` paketet först.
Här är ett exempel:

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString = map toLower

main :: IO ()
main = do
    let test = "GODT NYTT ÅR"
    putStrLn $ toLowerString test
```
När vi kör detta program kommer utdatan att vara `godt nytt år`.

## Fördjupning

- Historisk kontext: Funktionen `toLower`, definierad i paketet `Data.Char`, tillhandahålls som en del av biblioteket Preludium i Haskell. Preludium är det bibliotek som importerar allt man behöver för grundläggande programmering i Haskell.
- Alternativ: Det finns andra sätt att konvertera en sträng till små bokstäver i Haskell, t.ex. använda komposition istället för mappning: `toLowerString = foldr ((:) . toLower) []`.
- Implementation detaljer: Funktionen `toLower` arbetar på enskilda `Char` värden. I Haskell är en `String` verkligen bara en lista av `Char`, så en `map` operation fördelar `toLower` funktion till varje `Char` i listan.

## Se Även

- [Haskell Prelude Dokumentation | Hackage](http://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html): För mer information om grundläggande Haskell-funktioner.
- [Data.Char Dokumentation | Hackage](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html): För detaljer om `toLower` och andra `Char` relaterade funktioner.