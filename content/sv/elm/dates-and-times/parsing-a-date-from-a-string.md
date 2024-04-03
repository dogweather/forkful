---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:17.502954-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng i Elm inneb\xE4r att konvertera\
  \ textuell information som representerar datum och tider till ett format som Elm\
  \ kan f\xF6rst\xE5\u2026"
lastmod: '2024-03-13T22:44:37.838889-06:00'
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng i Elm inneb\xE4r att konvertera\
  \ textuell information som representerar datum och tider till ett format som Elm\
  \ kan f\xF6rst\xE5 och manipulera, specifikt till `Date`-typen."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
Elm har inte inbyggda funktioner som är lika robusta som vissa andra språk för datumtolkning, och förlitar sig huvudsakligen på Javascript-interoperabilitet eller bibliotek för mer komplexa operationer. Du kan dock använda paketet `elm/time` för grundläggande tolkning, och för mer komplexa behov rekommenderas ofta det tredjepartsbiblioteket `justinmimbs/date`.

### Tolkning med `elm/time`:
`elm/time` tillhandahåller modulen `Time`, som låter dig arbeta med tidsstämplar istället för datum som är läsbara för människor. Även om den inte direkt tolkar datum från strängar, kan du omvandla en ISO 8601-sträng till en POSIX-tidsstämpel, som du sedan kan arbeta med.

```elm
import Time exposing (Posix)

-- Antag att du har en ISO 8601 datumsträng
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Konvertera den till en POSIX-tidsstämpel (denna funktion returnerar ett `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Exempel på utdata: Ok <posix tidsvärde>
```

### Tolkning med `justinmimbs/date`:
För mer invecklad tolkning, som att hantera icke-ISO-format, är `justinmimbs/date`-biblioteket ett utmärkt val. Så här kan du använda det för att tolka en anpassad datumsträng:

1. Se till att du har installerat biblioteket:

```shell
elm install justinmimbs/date
```

2. Använd funktionen `Date.fromString` för att tolka anpassade datumformat:

```elm
import Date
import Result exposing (Result(..))

-- Låt oss säga att du har ett anpassat datumsträngformat `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Funktion för att tolka det anpassade formatet
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Exempelanvändning
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Exempel på utdata: Ok (Date.fromCalendarDate 2023 Jan 1)
```

I dessa exempel inkapslar `Result`-typen antingen en framgångsrik tolkning som ger ett datum (`Ok`) eller ett fel (`Err`), vilket möjliggör robust felhantering i dina Elm-applikationer.
