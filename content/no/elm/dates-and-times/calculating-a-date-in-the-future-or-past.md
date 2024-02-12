---
title:                "Beregning av en dato i fremtiden eller fortiden"
aliases:
- /no/elm/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:08.903313-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beregning av en dato i fremtiden eller fortiden"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Beregning av fremtidig eller tidligere dato betyr å finne en dato før eller etter en spesifikk dag. Programmerere gjør dette for funksjonalitet som påminnelser, utløpskontroller eller timeplanlegging.

## How to:
I Elm bruker du ofte `Time` og `Date` biblioteker for å håndtere datoer. Her er hvordan du kan beregne en dato i fremtiden eller fortiden:

```Elm
import Time exposing (..)
import Date exposing (Date)

calculateDate : Date -> Int -> Date
calculateDate baseDate daysToAdd =
    let
        basePosix = Date.toPosix baseDate
        millisToAdd = daysToAdd * 24 * 60 * 60 * 1000
    in
    Date.fromPosix (basePosix + millisToAdd)

main =
    let
        today = Date.fromCalendarDate 2023 Jan 1
        futureDate = calculateDate today 10 -- 10 dager frem i tid
        pastDate = calculateDate today -10 -- 10 dager tilbake i tid
    in
    -- Output:
    -- Fremtidig dato: Date.fromCalendarDate 2023 Jan 11
    -- Tidligere dato: Date.fromCalendarDate 2022 Dec 22
```

## Deep Dive
Tidlig i programmeringshistorien, ble datooperasjoner utført med enkle aritmetiske operasjoner på tidsstempler. I dag har språk som Elm abstrakte biblioteker som gjør jobben for oss. Det skjuler kompleksiteten ved å håndtere tidssoner, skuddår og annen tidrelatert hodebry. 

Når du beregner datoer i Elm, er det viktig å arbeide med Posix-tid (også kjent som Unix-tid), som representerer tid i millisekunder siden midnatt 1. januar 1970 UTC. For å regne ut en dato i fremtiden eller fortiden, legger du til eller trekker fra millisekunder basert på antall dager og den konstante verdien for døgnets millisekunder (86 400 000).

Alternativer for tidshåndtering i Elm inkluderer bruk av tredjeparts pakker som `elm-chrono` for mer sofistikerte operasjoner. Slike pakker kan tilby mer funksjonalitet som parsing, formatering og komplekse tidssoner.

Implementasjonen ovenfor antar at du alltid arbeider i UTC. Husk at lokale tidssoner kan påvirke beregningene hvis du ikke håndterer dem nøyaktig.

## See Also
- Elm Date documentation: https://package.elm-lang.org/packages/elm/time/latest/Time
- Elm Time documentation: https://package.elm-lang.org/packages/elm/time/latest/Date
- `elm-chrono` for complex date operations: https://package.elm-lang.org/packages/justinmimbs/elm-chrono/latest/
