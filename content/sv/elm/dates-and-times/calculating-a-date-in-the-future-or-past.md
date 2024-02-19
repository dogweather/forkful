---
aliases:
- /sv/elm/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:30:43.331723-07:00
description: "Ber\xE4kning av datum i framtiden eller f\xF6rflutna handlar om att\
  \ hitta en specifik tidspunkt f\xF6re eller efter ett k\xE4nt datum. Programmerare\
  \ g\xF6r detta f\xF6r att\u2026"
lastmod: 2024-02-18 23:08:51.722307
model: gpt-4-1106-preview
summary: "Ber\xE4kning av datum i framtiden eller f\xF6rflutna handlar om att hitta\
  \ en specifik tidspunkt f\xF6re eller efter ett k\xE4nt datum. Programmerare g\xF6\
  r detta f\xF6r att\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
---

{{< edit_this_page >}}

## Vad och varför?
Beräkning av datum i framtiden eller förflutna handlar om att hitta en specifik tidspunkt före eller efter ett känt datum. Programmerare gör detta för att hantera bokningar, uppföljningar, och tidbaserade påminnelser i sina applikationer.

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
