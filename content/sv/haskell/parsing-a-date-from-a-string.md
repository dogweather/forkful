---
title:                "Att tolka ett datum från en sträng"
html_title:           "Haskell: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsa ett datum från en sträng är en process där man tar en textsträng och omvandlar den till en användbar representationsform för datumet. Programmerare gör detta för att kunna hantera och manipulera datum på ett enklare och mer effektivt sätt i sina program.

## Hur man gör:

```Haskell
parseDate :: String -> Maybe Day 
parseDate date = case parseTimeM True defaultTimeLocale "%d/%m/%Y" date of
                    Just d -> Just d
                    Nothing -> Nothing
```
```Haskell
parseDate "12/03/2019" -- Just 02/01/2019
parseDate "Not a valid date" -- Nothing
```

## Djupdykning

Att parsa datum från en sträng har funnits sedan de första programmeringsspråken utvecklades, för att möjliggöra hanteringen av tids- och datuminformation. Alternativ till parsing inkluderar att använda inbyggda typer för datum och tid eller att skriva egna funktioner för att omvandla strängar till datum. I Haskell använder man funktionen `parseTimeM` från modulen `Data.Time.Format` för att parsa datum från strängar.

## Se även

- [Haskell hemsida](https://www.haskell.org/)
- [Haskell programmeringsspråk](https://en.wikipedia.org/wiki/Haskell_(programming_language))
- [Parsing](https://en.wikipedia.org/wiki/Parsing)