---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att göra den första bokstaven i varje ord stor. Programmerare gör detta för att formatera text på ett enhetligt sätt, ofta för gränssnitt eller dokumentation.

## Hur man gör:
```Haskell
import Data.Char (toUpper)

-- Kapitaliserar första bokstaven i ett ord
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

-- Använd exempel
main :: IO ()
main = do
    let text = "haskell är kul"
    putStrLn $ unwords $ map capitalize $ words text
```
Kör koden. Du får:
```
Haskell Är Kul
```

## Djupdykning
I tidiga datorsystem, var text ofta begränsad till stora bokstäver på grund av begränsad teckenstöd och enkelhet. Idag används textkapitalisering för att uppfylla språkliga konventioner och förbättra läsbarheten. Alternativ till `Data.Char` inkluderar att använda bibliotek som `text` eller `bytestring` för prestanda med större textmängder. Vid kapitalisering är det även viktigt att tänka på lokala konventioner, exempelvis olika regler i olika språk när det gäller vilka ord som skall börja med stor bokstav.

## Se också
- Haskell's `Data.Char` modul: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- `text` library: https://hackage.haskell.org/package/text
- `bytestring` library: https://hackage.haskell.org/package/bytestring