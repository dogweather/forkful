---
title:                "Ta bort citattecken från en sträng"
date:                  2024-01-26T03:39:54.534616-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att ta bort citationstecken från en sträng innebär att strippa bort alla citattecken—enkla (' ') eller dubbla (" ")—som är en del av strängdatan. Programmerare gör detta för att sanera inmatningar, förbereda text för bearbetning eller bli av med onödiga tecken som kan störa hanteringen och operationerna av datan.

## Hur man gör:
I Haskell kan vi snabbt skapa en funktion som tar bort alla citattecken från en given sträng. Det är som att säga åt citattecknen att dra åt skogen och se till att de fattar vinken.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell sa, \"Låt oss lära oss några funktioner!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Exempel på utdata:

```
Haskell sa, Låt oss lära oss några funktioner!
```

## Djupdykning
En gång i tiden, innan strängar i programmering var lika vanliga som kattvideor på internet, var textbehandling en knepig syssla. Men eftersom programmeringsspråk utvecklades, blev strängar en avgörande del av kodningen. Ändå förblev citattecken ett tveeggat svärd—avgörande för att definiera strängar, men ett gissel när de inkluderades som verkliga data.

Alternativ? Istället för att svepa bort alla citattecken som flugor, kan du vara selektiv. Du kanske vill ta bort enbart de yttersta citattecknen (en klassisk trimning) eller hantera undkomma citattecken inuti en sträng.

När det gäller implementeringen använder `removeQuotes`-funktionen ovan en lambda för att kontrollera varje tecken (`c`) för att se om det är ett irriterande citattecken och filtrerar bort dem därefter. Detta är en enkel ansats, men för större texter eller mer komplexa regler kanske du vill titta på parser-bibliotek som `Parsec` som kan ge dig mer finess och kraft i textbearbetning.

## Se också:
- För regex-älskare: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Dyk in i parsers med: [Parsec: A Practical Parser Library](https://research.microsoft.com/en-us/um/people/daan/download/parsec/parsec.pdf)
- En mjuk introduktion till Haskell-strängar: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)