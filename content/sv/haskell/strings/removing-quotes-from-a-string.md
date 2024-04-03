---
date: 2024-01-26 03:39:54.534616-07:00
description: "Hur man g\xF6r: I Haskell kan vi snabbt skapa en funktion som tar bort\
  \ alla citattecken fr\xE5n en given str\xE4ng. Det \xE4r som att s\xE4ga \xE5t citattecknen\
  \ att dra \xE5t\u2026"
lastmod: '2024-03-13T22:44:37.943545-06:00'
model: gpt-4-0125-preview
summary: "I Haskell kan vi snabbt skapa en funktion som tar bort alla citattecken\
  \ fr\xE5n en given str\xE4ng."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

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
- En mjuk introduktion till Haskell-strängar: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
