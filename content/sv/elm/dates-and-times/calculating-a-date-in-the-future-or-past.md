---
date: 2024-01-20 17:30:43.331723-07:00
description: "S\xE5 h\xE4r g\xF6r du: Elm ger inget inbyggt st\xF6d f\xF6r datum och\
  \ tid, s\xE5 vi lutar oss mot `elm/time` paketet. H\xE4r kommer en kodsnutt f\xF6\
  r att r\xE4kna ut framtida och\u2026"
lastmod: '2024-03-13T22:44:37.842806-06:00'
model: gpt-4-1106-preview
summary: "Elm ger inget inbyggt st\xF6d f\xF6r datum och tid, s\xE5 vi lutar oss mot\
  \ `elm/time` paketet."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
weight: 26
---

## Så här gör du:
Elm ger inget inbyggt stöd för datum och tid, så vi lutar oss mot `elm/time` paketet. Här kommer en kodsnutt för att räkna ut framtida och förflutna datum:

```Elm
import Time exposing (..)
import Task

calculateFutureDate : Posix -> Time.Zone -> Int -> Task.Task x Posix
calculateFutureDate baseDate zone daysToAdd =
    Task.succeed baseDate
        |> Task.map (\date -> add (daysToMillis daysToAdd) date)
        |> Task.map (\date -> toTimezone zone date)

daysToMillis : Int -> Int
daysToMillis days =
    days * 24 * 60 * 60 * 1000

-- Användning:
-- Antag att `zone` är din lokala tidszon och `now` är det nuvarande Posix-värdet.
-- calculateFutureDate now zone 10 skulle beräkna datumet 10 dagar framåt.
```

Sample output är beroende av den aktuella tidszonen och basdatumet.

## Fördjupning:
Tidigare, användes enkla tidsberäkningar i Elm, men tidszonshantering komplicerade saker. Med `elm/time`, är det nu enklare och mer robust. Alternativ till `elm/time` inkluderar att använda JavaScript direkt via ports. För implementation, var medveten om att tidszonerna påverkar resultaten, så det är viktigt att alltid specificera och arbeta inom en korrekt tidszon.

## Se även:
- Elm Time paketdokumentation: https://package.elm-lang.org/packages/elm/time/latest/
- Elm Guide om tid: https://guide.elm-lang.org/effects/time.html
- World Clock API för att hantera olika tidszoner i Elm: https://worldtimeapi.org/
